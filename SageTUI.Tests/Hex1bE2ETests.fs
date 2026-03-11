module SageTUI.Hex1bE2ETests

open System
open System.IO
open System.Runtime.InteropServices
open Expecto
open Expecto.Flip
open SageTUI.ProcessHelpers

let private ensureHex1bCaptureSupported () =
  match RuntimeInformation.IsOSPlatform(OSPlatform.OSX) with
  | false ->
    Tests.skiptest
      "Hex1b PTY capture requires macOS (arm64). Native interop library is not available on Windows or Linux; skipping to avoid DllNotFoundException."
  | true ->
    // PTY input delivery is unreliable on macOS GitHub Actions runners (keys sent via
    // Hex1b automation don't reach the TUI process). Skip on CI to prevent job failure.
    match System.Environment.GetEnvironmentVariable("CI") with
    | ci when not (String.IsNullOrEmpty(ci)) ->
      Tests.skiptest
        "Hex1b PTY interaction times out on macOS CI runners (input not delivered to process). Run locally to validate."
    | _ -> ()

// Lazy specs: builtProjectExecutable is called at test-run time, not module-load time.
// If called at module-load time, a FailedException thrown by failtestf gets wrapped in
// TypeInitializationException, which Expecto reports as "errored" rather than "failed".
let private makeCounterSpec () =
  { WorkingDirectory = Path.Combine(repoRoot, "examples", "Counter")
    FileName = builtProjectExecutable (Path.Combine("examples", "Counter")) "Counter"
    Arguments = []
    Environment =
      Map.ofList
        [ "DOTNET_NOLOGO", "1"
          "SAGETUI_DISABLE_ALT_SCREEN", "1"
          "SAGETUI_DISABLE_RAW_MODE", "1"
          "TERM", "xterm-256color" ] }

let private makeInteractiveFormSpec () =
  { WorkingDirectory = Path.Combine(repoRoot, "samples", "04-InteractiveForm")
    FileName = builtProjectExecutable (Path.Combine("samples", "04-InteractiveForm")) "InteractiveForm"
    Arguments = []
    Environment =
      Map.ofList
        [ "DOTNET_NOLOGO", "1"
          "SAGETUI_DISABLE_ALT_SCREEN", "1"
          "SAGETUI_DISABLE_RAW_MODE", "1"
          "TERM", "xterm-256color" ] }

[<Tests>]
let hex1bE2ETests =
  testList "Hex1b.E2E" [
    testCaseAsync "counter app responds to real keypresses and exits cleanly" <| async {
      ensureHex1bCaptureSupported ()

      let! session =
        startPtySession 50 12 (makeCounterSpec ())
        |> Async.AwaitTask

      use session = session

      do!
        waitUntilOutputContains "Count: 0" (TimeSpan.FromSeconds 15.0) session
        |> Async.AwaitTask

      do!
        sendInputSequence (fun builder -> builder.Type("+")) session
        |> Async.AwaitTask

      do!
        waitUntilOutputContains "Count: 1" (TimeSpan.FromSeconds 10.0) session
        |> Async.AwaitTask

      do!
        sendInputSequence (fun builder -> builder.Type("q")) session
        |> Async.AwaitTask

      let! exitCode =
        waitForExit (TimeSpan.FromSeconds 15.0) session
        |> Async.AwaitTask

      exitCode |> Expect.equal "counter should exit cleanly" 0
    }

    testCaseAsync "interactive form accepts typing and backspace through PTY input" <| async {
      ensureHex1bCaptureSupported ()

      let! session =
        startPtySession 80 24 (makeInteractiveFormSpec ())
        |> Async.AwaitTask

      use session = session

      do!
        waitUntilOutputContains "Interactive Form" (TimeSpan.FromSeconds 15.0) session
        |> Async.AwaitTask

      do!
        sendInputSequence (fun builder -> builder.Type("zqx").Backspace()) session
        |> Async.AwaitTask

      do!
        waitUntilOutputContains "zq" (TimeSpan.FromSeconds 10.0) session
        |> Async.AwaitTask

      do!
        sendInputSequence (fun builder -> builder.Escape()) session
        |> Async.AwaitTask

      let! exitCode =
        waitForExit (TimeSpan.FromSeconds 15.0) session
        |> Async.AwaitTask

      exitCode |> Expect.equal "interactive form should exit cleanly" 0
    }
  ]
