module SageTUI.ProcessHelpers

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Threading.Tasks
open Hex1b
open Hex1b.Automation
open Expecto

let repoRoot =
  Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."))

type CommandResult = {
  FileName: string
  Arguments: string list
  WorkingDirectory: string
  ExitCode: int
  StdOut: string
  StdErr: string
}

type PtyProcessSpec = {
  WorkingDirectory: string
  FileName: string
  Arguments: string list
  Environment: Map<string, string>
}

type PtySession internal (terminal: Hex1bTerminal, outputLogPath: string, runTask: Task<int>) =
  member _.Terminal = terminal
  member _.OutputLogPath = outputLogPath
  member _.RunTask = runTask

  interface IDisposable with
    member _.Dispose() =
      terminal.Dispose()

type TestWorkspace(rootDir: string, cliHome: string, localFeed: string) =
  member _.RootDir = rootDir
  member _.CliHome = cliHome
  member _.LocalFeed = localFeed

  interface IDisposable with
    member _.Dispose() =
      try
        if Directory.Exists(rootDir) then
          Directory.Delete(rootDir, true)
      with _ ->
        ()

let createWorkspace (name: string) =
  let safeName =
    name.ToCharArray()
    |> Array.map (fun c -> if Char.IsLetterOrDigit c then c else '-')
    |> String

  let rootDir =
    Path.Combine(
      Path.GetTempPath(),
      "SageTUI.Tests",
      sprintf "%s-%s" safeName (Guid.NewGuid().ToString("N"))
    )

  let cliHome = Path.Combine(rootDir, ".dotnet-cli-home")
  let localFeed = Path.Combine(rootDir, "local-feed")

  Directory.CreateDirectory(rootDir) |> ignore
  Directory.CreateDirectory(cliHome) |> ignore
  Directory.CreateDirectory(localFeed) |> ignore

  new TestWorkspace(rootDir, cliHome, localFeed)

let cliEnvironment (workspace: TestWorkspace) =
  Map.ofList [
    "DOTNET_CLI_HOME", workspace.CliHome
    "HOME", workspace.CliHome
    "USERPROFILE", workspace.CliHome
    "DOTNET_CLI_TELEMETRY_OPTOUT", "1"
    "DOTNET_SKIP_FIRST_TIME_EXPERIENCE", "1"
    "DOTNET_NOLOGO", "1"
  ]

let private renderArg (arg: string) =
  if String.IsNullOrWhiteSpace arg || arg.Contains(" ") || arg.Contains("\"") then
    "\"" + arg.Replace("\"", "\\\"") + "\""
  else
    arg

let formatCommand (fileName: string) (args: string list) =
  String.concat " " (fileName :: (args |> List.map renderArg))

let runProcess
  (timeout: TimeSpan)
  (environment: Map<string, string>)
  (workingDirectory: string)
  (fileName: string)
  (args: string list)
  : CommandResult =

  let startInfo = ProcessStartInfo()
  startInfo.FileName <- fileName
  startInfo.WorkingDirectory <- workingDirectory
  startInfo.RedirectStandardOutput <- true
  startInfo.RedirectStandardError <- true
  startInfo.UseShellExecute <- false
  startInfo.CreateNoWindow <- true

  for arg in args do
    startInfo.ArgumentList.Add(arg)

  for KeyValue(key, value) in environment do
    startInfo.Environment.[key] <- value

  use proc = new Process()
  proc.StartInfo <- startInfo

  if not (proc.Start()) then
    failtestf "Failed to start process: %s" (formatCommand fileName args)

  let stdoutTask = proc.StandardOutput.ReadToEndAsync()
  let stderrTask = proc.StandardError.ReadToEndAsync()

  if not (proc.WaitForExit(int timeout.TotalMilliseconds)) then
    try
      proc.Kill(true)
    with _ ->
      ()

    failtestf
      "Command timed out after %O\nCommand: %s\nWorking directory: %s"
      timeout
      (formatCommand fileName args)
      workingDirectory

  proc.WaitForExit()

  {
    FileName = fileName
    Arguments = args
    WorkingDirectory = workingDirectory
    ExitCode = proc.ExitCode
    StdOut = stdoutTask.Result
    StdErr = stderrTask.Result
  }

let dotnet
  (environment: Map<string, string>)
  (workingDirectory: string)
  (args: string list)
  : CommandResult =
  runProcess (TimeSpan.FromMinutes 3.0) environment workingDirectory "dotnet" args

let requireSuccess (label: string) (result: CommandResult) =
  if result.ExitCode <> 0 then
    failtestf
      "%s failed\nCommand: %s\nWorking directory: %s\nExit code: %d\n\nstdout:\n%s\nstderr:\n%s"
      label
      (formatCommand result.FileName result.Arguments)
      result.WorkingDirectory
      result.ExitCode
      result.StdOut
      result.StdErr

let findSinglePackage (directory: string) (prefix: string) =
  Directory.GetFiles(directory, $"{prefix}*.nupkg")
  |> Array.filter (fun path -> not (path.EndsWith(".symbols.nupkg", StringComparison.OrdinalIgnoreCase)))
  |> Array.exactlyOne

let builtProjectExecutable (relativeProjectDir: string) (assemblyName: string) =
  // Normalize path separators for cross-platform compatibility.
  // Callers may use Windows backslash literals (@"examples\Counter") which fail on macOS/Linux.
  let normalized =
    relativeProjectDir.Replace('\\', Path.DirectorySeparatorChar)
  let projectDir = Path.Combine(repoRoot, normalized)
  let binDir = Path.Combine(projectDir, "bin")

  let exePath =
    match Directory.Exists(binDir) with
    | true ->
      // .NET on Windows produces AppName.exe; on Linux/macOS produces AppName (no extension).
      // Search both patterns so tests work cross-platform.
      [| $"{assemblyName}.exe"; assemblyName |]
      |> Array.collect (fun pattern ->
        Directory.GetFiles(binDir, pattern, SearchOption.AllDirectories))
      |> Array.sortByDescending File.GetLastWriteTimeUtc
      |> Array.tryHead
    | false -> None

  match exePath with
  | Some path -> path
  | None ->
    failtestf
      "Could not locate built executable %s(.exe) under %s. Ensure the project is referenced by the test project so it is built before test execution."
      assemblyName
      binDir

let private readAllTextShared (path: string) =
  match File.Exists(path) with
  | true ->
    use stream = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    use reader = new StreamReader(stream, Encoding.UTF8, true)
    reader.ReadToEnd()
  | false -> ""

let startPtySession
  (width: int)
  (height: int)
  (spec: PtyProcessSpec)
  : Task<PtySession> =
  task {
    let outputDir = Path.Combine(Path.GetTempPath(), "SageTUI.Tests", "hex1b-output")
    Directory.CreateDirectory(outputDir) |> ignore
    let outputLogPath = Path.Combine(outputDir, sprintf "%s.log" (Guid.NewGuid().ToString("N")))

    let environment = System.Collections.Generic.Dictionary<string, string>(StringComparer.Ordinal)
    for KeyValue(key, value) in spec.Environment do
      environment.[key] <- value
    environment.["COLUMNS"] <- string width
    environment.["LINES"] <- string height

    let builder =
      Hex1bTerminal.CreateBuilder()
        .WithHeadless()
        .WithDimensions(width, height)
        .WithWorkloadLogging(outputLogPath, false)
        .WithPtyProcess(Action<Hex1bTerminalProcessOptions>(fun options ->
          options.FileName <- spec.FileName
          options.Arguments <- ResizeArray(spec.Arguments)
          options.WorkingDirectory <- spec.WorkingDirectory
          options.InheritEnvironment <- true
          options.Environment <- environment))

    let terminal = builder.Build()
    let runTask = terminal.RunAsync()
    return new PtySession(terminal, outputLogPath, runTask)
  }

let sendInputSequence
  (build: Hex1bTerminalInputSequenceBuilder -> Hex1bTerminalInputSequenceBuilder)
  (session: PtySession)
  : Task =
  task {
    let sequence = build (Hex1bTerminalInputSequenceBuilder()) |> fun builder -> builder.Build()
    let! _ = sequence.ApplyAsync(session.Terminal)
    return ()
  }

let waitUntilOutputContains
  (needle: string)
  (timeout: TimeSpan)
  (session: PtySession)
  : Task =
  task {
    // Use CreateSnapshot().GetScreenText() to read the actual rendered terminal screen.
    // The workload log stores parsed ANSI tokens (e.g. "Text(Count:) Cursor(1,8) Text(0)")
    // so string literals like "Count: 0" do not appear verbatim in the log — the space
    // at column 7 is an existing blank cell, never written as a token.
    // CreateSnapshot may throw if the PTY process hasn't initialized yet or has already
    // exited unexpectedly. Returning "" lets the polling loop continue until timeout.
    let getScreenText () =
      try
        use snapshot = session.Terminal.CreateSnapshot()
        snapshot.GetScreenText()
      with _ -> ""

    let startedAt = DateTime.UtcNow
    let mutable found = false

    while not found && DateTime.UtcNow - startedAt < timeout do
      // If the PTY process faulted (e.g. native library failure), fail fast with the
      // actual exception rather than timing out with a misleading screen-content error.
      if session.RunTask.IsFaulted then
        let inner =
          match session.RunTask.Exception with
          | null -> "unknown"
          | ex -> ex.InnerException |> Option.ofObj |> Option.map (fun e -> e.ToString()) |> Option.defaultValue (ex.ToString())
        failtestf "PTY process faulted before '%s' appeared on screen.\n%s" needle inner

      found <- (getScreenText ()).Contains(needle, StringComparison.Ordinal)

      if not found then
        let delay =
          match session.RunTask.IsCompleted with
          | true -> 100
          | false -> 50
        do! Task.Delay(delay)

    if not found then
      let screenText = getScreenText ()
      failtestf
        "Timed out after %O waiting for screen to contain \"%s\".\nLog: %s\n\nScreen content:\n%s"
        timeout
        needle
        session.OutputLogPath
        screenText
  }

let waitForExit
  (timeout: TimeSpan)
  (session: PtySession)
  : Task<int> =
  task {
    let! completed = Task.WhenAny(session.RunTask :> Task, Task.Delay(timeout))

    if not (obj.ReferenceEquals(completed, session.RunTask :> Task)) then
      failtestf
        "PTY process did not exit within timeout.\nRecording: %s"
        session.OutputLogPath

    return! session.RunTask
  }
