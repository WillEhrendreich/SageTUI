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
let sampleLogViewerTests =
  testList "Samples.LogViewer" [

    test "init: LogList contains all 20 entries" {
      let m, _ = LogViewer.init ()
      m.LogList.Items.Length |> Expect.equal "should have all 20 sample entries" 20
    }

    test "init: FilterInput is empty" {
      let m, _ = LogViewer.init ()
      m.FilterInput.Text |> Expect.equal "filter should be empty on init" ""
    }

    test "init: focus starts on FilterField" {
      let m, _ = LogViewer.init ()
      FocusRing.current m.Focus |> Expect.equal "focus should start on FilterField" (Some LogViewer.FilterField)
    }

    test "init: no toasts on start" {
      let m, _ = LogViewer.init ()
      ToastQueue.count m.Toasts |> Expect.equal "no toasts on init" 0
    }

    test "matchesFilter: empty filter matches everything" {
      LogViewer.sampleEntries
      |> Array.forall (LogViewer.matchesFilter "")
      |> Expect.isTrue "empty filter should match all entries"
    }

    test "matchesFilter: 'error' matches Error and Fatal level entries" {
      let matchError = LogViewer.matchesFilter "err"
      LogViewer.sampleEntries
      |> Array.filter matchError
      |> Array.forall (fun e -> e.Level = LogViewer.Error || e.Source = "Worker" || e.Message.ToLowerInvariant().Contains("err"))
      |> Expect.isTrue "err filter should match entries containing err in message/source/level"
    }

    test "matchesFilter: 'ftl' matches Fatal entries" {
      let fatalEntries =
        LogViewer.sampleEntries
        |> Array.filter (LogViewer.matchesFilter "ftl")
      (fatalEntries.Length, 0) |> Expect.isGreaterThan "should find at least one fatal entry"
      fatalEntries |> Array.forall (fun e -> e.Level = LogViewer.Fatal)
      |> Expect.isTrue "all matched entries should be Fatal level"
    }

    test "matchesFilter: case-insensitive source match" {
      let dbEntries =
        LogViewer.sampleEntries
        |> Array.filter (LogViewer.matchesFilter "DB")
      let dbEntriesLower =
        LogViewer.sampleEntries
        |> Array.filter (LogViewer.matchesFilter "db")
      dbEntries.Length |> Expect.equal "DB and db should match same count" dbEntriesLower.Length
    }

    test "Tab key moves focus from FilterField to ListField" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
      FocusRing.current app.Model.Focus
      |> Expect.equal "Tab should move focus to ListField" (Some LogViewer.ListField)
    }

    test "Shift+Tab moves focus from ListField back to FilterField" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.pressKeyWith Key.Tab Modifiers.Shift
      FocusRing.current app.Model.Focus
      |> Expect.equal "Shift+Tab should return to FilterField" (Some LogViewer.FilterField)
    }

    test "typing filter text rebuilds list to matching entries only" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.typeText "db"
      let dbEntries =
        LogViewer.sampleEntries
        |> Array.filter (LogViewer.matchesFilter "db")
      app.Model.LogList.Items.Length
      |> Expect.equal "list should contain only DB-matching entries" dbEntries.Length
    }

    test "typing filter then clearing shows all entries" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.typeText "db"
        |> TestHarness.pressKey Key.Backspace
        |> TestHarness.pressKey Key.Backspace
      app.Model.LogList.Items.Length
      |> Expect.equal "clearing filter should restore all entries" 20
    }

    test "j key on ListField moves selection down" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab     // focus list
        |> TestHarness.typeText "j"         // navigate down
      app.Model.LogList.SelectedIndex
      |> Expect.equal "j should move selection to index 1" (Some 1)
    }

    test "k key on ListField moves selection up after going down" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "j"
        |> TestHarness.typeText "j"
        |> TestHarness.typeText "k"
      app.Model.LogList.SelectedIndex
      |> Expect.equal "k should move selection back to index 1" (Some 1)
    }

    test "Down arrow on ListField moves selection down" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.pressKey Key.Down
      app.Model.LogList.SelectedIndex
      |> Expect.equal "Down should move selection to index 1" (Some 1)
    }

    test "G key jumps to last entry" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "G"
      let lastIndex = LogViewer.sampleEntries.Length - 1
      app.Model.LogList.SelectedIndex
      |> Expect.equal "G should jump to last entry" (Some lastIndex)
    }

    test "g key after G jumps back to first entry" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "G"
        |> TestHarness.typeText "g"
      app.Model.LogList.SelectedIndex
      |> Expect.equal "g should jump to first entry" (Some 0)
    }

    test "Enter on selected entry adds a toast" {
      let app =
        TestHarness.init 120 30 LogViewer.program
        |> TestHarness.pressKey Key.Tab    // focus list
        |> TestHarness.typeText "j"         // select entry 1
        |> TestHarness.pressKey Key.Enter  // copy
      (ToastQueue.count app.Model.Toasts, 0) |> Expect.isGreaterThan "pressing Enter on a selected entry should add a toast"
    }

    test "DismissToast removes a specific toast" {
      let m0, _ = LogViewer.init ()
      let toasts1, id = ToastQueue.push "Test message" 30 Style.empty m0.Toasts
      let mWithToast = { m0 with Toasts = toasts1 }
      let m1, _ = LogViewer.update (LogViewer.DismissToast id) mWithToast
      ToastQueue.count m1.Toasts |> Expect.equal "DismissToast should remove the toast" 0
    }

    test "filter by 'debug' returns only Debug-level entries plus message matches" {
      let dbgEntries =
        LogViewer.sampleEntries
        |> Array.filter (LogViewer.matchesFilter "dbg")
      let debugCount = LogViewer.sampleEntries |> Array.filter (fun e -> e.Level = LogViewer.Debug) |> Array.length
      dbgEntries.Length |> Expect.equal "DBG filter should match exactly the Debug entries" debugCount
    }

    test "rebuildList preserves relative entry order" {
      let all   = LogViewer.sampleEntries
      let list  = LogViewer.rebuildList "" all
      let items = list.Items
      let ordered =
        items
        |> Array.pairwise
        |> Array.forall (fun (a, b) -> a.Index < b.Index)
      ordered |> Expect.isTrue "filtered list should preserve original entry order"
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

    snapshotTest "sample-logviewer-initial"
      (renderProgram 120 32 LogViewer.program)

    snapshotTest "sample-logviewer-filtered-db"
      (TestHarness.init 120 32 LogViewer.program
       |> TestHarness.typeText "db"
       |> TestHarness.render)

    snapshotTest "sample-logviewer-list-focused"
      (TestHarness.init 120 32 LogViewer.program
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.render)
  ]
