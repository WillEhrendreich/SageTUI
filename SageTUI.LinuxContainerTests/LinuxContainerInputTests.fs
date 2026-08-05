module SageTUI.LinuxContainerInputTests

open System
open System.Diagnostics
open System.IO
open Expecto

let private repositoryRoot =
  Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."))

let private probeProject = Path.Combine(repositoryRoot, "SageTUI.ContainerProbe", "SageTUI.ContainerProbe.fsproj")

let private runProcess (timeout: TimeSpan) (workingDirectory: string) (fileName: string) (arguments: string list) =
  let startInfo = ProcessStartInfo()
  startInfo.FileName <- fileName
  startInfo.WorkingDirectory <- workingDirectory
  startInfo.RedirectStandardOutput <- true
  startInfo.RedirectStandardError <- true
  startInfo.UseShellExecute <- false

  arguments |> List.iter startInfo.ArgumentList.Add

  use proc = new Process(StartInfo = startInfo)
  proc.Start() |> ignore
  let stdout = proc.StandardOutput.ReadToEndAsync()
  let stderr = proc.StandardError.ReadToEndAsync()

  if not (proc.WaitForExit(int timeout.TotalMilliseconds)) then
    try proc.Kill(true) with _ -> ()
    failtestf "Timed out after %O: %s %s" timeout fileName (String.concat " " arguments)

  proc.ExitCode, stdout.Result + stderr.Result

let private requireDocker () =
  let exitCode, output = runProcess (TimeSpan.FromSeconds 20.0) repositoryRoot "docker" [ "info" ]
  if exitCode <> 0 then
    failtestf "Docker is required for Linux PTY integration tests:\n%s" output

let private publishProbe publishDirectory =
  let exitCode, output =
    runProcess
      (TimeSpan.FromMinutes 3.0)
      repositoryRoot
      "dotnet"
      [ "publish"; probeProject; "-c"; "Release"; "-r"; "linux-x64"; "--self-contained"; "true"; "-o"; publishDirectory ]

  if exitCode <> 0 then
    failtestf "Could not publish the Linux PTY probe:\n%s" output

let private writeRunner publishDirectory =
  let runner = Path.Combine(publishDirectory, "run-pty-probe.sh")
  File.WriteAllText(runner, """#!/bin/sh
set -eu

# script(1) owns the child's controlling PTY. A FIFO supplies its stdin and a
# typescript records the actual rendered terminal stream. Do not inject CR
# until the pre-input frame is observed; then require the post-input frame.
input=/tmp/sagetui-input
output=/tmp/sagetui-output
mkfifo "$input"
exec 3<> "$input"

wait_for() {
  marker=$1
  attempts=0
  while ! grep -q "$marker" "$output" 2>/dev/null; do
    attempts=$((attempts + 1))
    if [ "$attempts" -gt 200 ]; then
      cat "$output" 2>/dev/null || true
      exit 1
    fi
    sleep 0.05
  done
}

(
  wait_for WAITING_FOR_ENTER
  printf '\r' >&3
) &
feeder=$!

# Keep script in the foreground. The probe emits PTY_INPUT_CONFIRMED and exits
# only after SageTUI's KeySub receives the CR delivered through this PTY.
set +e
script -q -e -f -c 'stty rows 40 cols 120; exec env TERM=xterm-256color COLUMNS=120 LINES=40 SAGETUI_DISABLE_ALT_SCREEN=1 /app/SageTUI.ContainerProbe' "$output" < "$input"
status=$?
set -e
wait "$feeder" || true
test "$status" -eq 0
grep -q PTY_INPUT_CONFIRMED "$output"
cat "$output"
""")

let private runPtyProbe image bootstrapCommand =
  requireDocker ()

  let publishDirectory = Path.Combine(Path.GetTempPath(), "SageTUI.ContainerProbe", Guid.NewGuid().ToString("N"))
  Directory.CreateDirectory(publishDirectory) |> ignore

  try
    publishProbe publishDirectory
    writeRunner publishDirectory

    let exitCode, output =
      runProcess
        (TimeSpan.FromMinutes 3.0)
        repositoryRoot
        "docker"
        [ "run"; "--rm"; "--pull=missing"; "-i"
          "-v"; sprintf "%s:/app:ro" publishDirectory
          image
          "sh"; "-c"; bootstrapCommand ]

    if exitCode <> 0 && not (output.Contains("PTY_INPUT_CONFIRMED", StringComparison.Ordinal)) then
      failtestf "%s PTY probe failed with exit code %d:\n%s" image exitCode output

    Expect.stringContains
      output
      "PTY_INPUT_CONFIRMED"
      (sprintf "%s must consume CR through its controlling PTY and render the post-input state" image)
  finally
    if Directory.Exists(publishDirectory) then
      Directory.Delete(publishDirectory, true)

[<Tests>]
let linuxContainerInputTests =
  testSequenced <|
    testList "Linux container PTY input" [
      testCase "Debian receives CR through a controlling PTY" <| fun () ->
        runPtyProbe
          "debian:bookworm-slim"
          "apt-get update >/dev/null && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends util-linux coreutils libicu72 >/dev/null && sh /app/run-pty-probe.sh"

      testCase "Arch receives CR through a controlling PTY" <| fun () ->
        runPtyProbe
          "archlinux:latest"
          "pacman -Sy --noconfirm util-linux coreutils icu >/dev/null && sh /app/run-pty-probe.sh"
    ]
