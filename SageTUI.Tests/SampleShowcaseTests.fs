module SageTUI.SampleShowcaseTests

open Expecto
open Expecto.Flip
open SageTUI
open SageTUI.IntegrationTests
open VerifyExpecto

let private snapshotTest name (output: string) =
  testCaseAsync name (async {
    let! _ =
      Verifier.Verify(name, output)
        .UseDirectory("snapshots")
        .ToTask()
      |> Async.AwaitTask
    ()
  })

let private renderProgram width height (program: Program<'model, 'msg>) =
  TestHarness.init width height program
  |> TestHarness.render

[<Tests>]
let sampleInteractiveFormTests =
  testList "Samples.InteractiveForm" [
    test "typing into the focused name field updates the real sample" {
      let app =
        TestHarness.init 80 24 InteractiveForm.program
        |> TestHarness.typeText "Alice"

      app.Model.Name.Text |> Expect.equal "name field should capture typed text" "Alice"
      FocusRing.current app.Model.Focus |> Expect.equal "focus should stay on the name field" (Some InteractiveForm.Name)
    }

    test "shift-tab moves focus backward in the real sample" {
      let app =
        TestHarness.init 80 24 InteractiveForm.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.pressKeyWith Key.Tab Modifiers.Shift

      FocusRing.current app.Model.Focus |> Expect.equal "shift-tab should return focus to the name field" (Some InteractiveForm.Name)
    }

    test "real sample supports name then email entry" {
      let app =
        TestHarness.init 80 24 InteractiveForm.program
        |> TestHarness.typeText "Alice"
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "alice@example.com"

      app.Model.Name.Text |> Expect.equal "name should persist after tabbing" "Alice"
      app.Model.Email.Text |> Expect.equal "email should capture typed text" "alice@example.com"
    }
  ]

[<Tests>]
let sampleSnapshotTests =
  testList "Samples.Snapshots" [
    snapshotTest "sample-interactive-form-initial"
      (renderProgram 80 24 InteractiveForm.program)

    snapshotTest "sample-interactive-form-filled"
      (TestHarness.init 80 24 InteractiveForm.program
       |> TestHarness.typeText "Alice"
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.typeText "alice@example.com"
       |> TestHarness.render)

    snapshotTest "sample-system-monitor-overview"
      (renderProgram 100 28 SystemMonitor.program)

    snapshotTest "sample-composable-counter-initial"
      (renderProgram 80 24 ComposableCounter.program)
  ]
