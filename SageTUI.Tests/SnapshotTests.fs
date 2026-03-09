module SageTUI.SnapshotTests

open Expecto
open SageTUI
open SageTUI.IntegrationTests
open VerifyExpecto

// ============================================================
// SNAPSHOT TEST HELPER
// ============================================================

let private render w h elem = TestHarness.renderElement w h elem

let private snapshotTest name (output: string) =
  testCaseAsync name (async {
    let! _ =
      Verifier.Verify(name, output)
        .UseDirectory("snapshots")
        .ToTask()
      |> Async.AwaitTask
    ()
  })

// ============================================================
// BORDER STYLE SNAPSHOTS
// ============================================================

let borderSnapshots =
  testList "Snapshot.Borders" [
    snapshotTest "border-rounded"
      (render 30 5 (El.text "Hello World" |> El.bordered Rounded))
    snapshotTest "border-light"
      (render 30 5 (El.text "Hello World" |> El.bordered Light))
    snapshotTest "border-heavy"
      (render 30 5 (El.text "Hello World" |> El.bordered Heavy))
    snapshotTest "border-double"
      (render 30 5 (El.text "Hello World" |> El.bordered Double))
    snapshotTest "border-ascii"
      (render 30 5 (El.text "Hello World" |> El.bordered Ascii))
  ]

// ============================================================
// LAYOUT SNAPSHOTS
// ============================================================

let layoutSnapshots =
  testList "Snapshot.Layout" [
    snapshotTest "layout-text"
      (render 20 3 (El.text "Simple Text"))
    snapshotTest "layout-bold-text"
      (render 20 3 (El.text "Bold Text" |> El.bold))
    snapshotTest "layout-row"
      (render 30 3 (El.row [
        El.text "Left" |> El.width 10
        El.text "Right" |> El.width 10
      ]))
    snapshotTest "layout-column"
      (render 20 6 (El.column [
        El.text "Line 1"
        El.text "Line 2"
        El.text "Line 3"
      ]))
    snapshotTest "layout-padded"
      (render 20 7 (El.text "Padded" |> El.padAll 2))
    snapshotTest "layout-centered"
      (render 30 5 (El.text "Center" |> El.center))
    snapshotTest "layout-nested-borders"
      (render 40 8 (El.row [
        El.column [ El.text "A"; El.text "B" ]
        |> El.bordered Rounded |> El.width 15
        El.text "Right" |> El.bordered Light |> El.width 15
      ]))
    snapshotTest "layout-gapped-column"
      (render 20 8 (
        El.column [ El.text "First"; El.text "Second"; El.text "Third" ]
        |> El.gap 1))
    snapshotTest "layout-overlay"
      (render 20 5 (El.overlay [ El.text "Background"; El.text "Foreground" ]))
    snapshotTest "layout-fill"
      (render 30 3 (El.row [
        El.text "Fixed" |> El.width 10
        El.text "Stretch" |> El.fill
      ]))
  ]

// ============================================================
// WIDGET SNAPSHOTS
// ============================================================

let widgetSnapshots =
  testList "Snapshot.Widgets" [
    // TextInput
    snapshotTest "widget-textinput-empty"
      (render 30 3 (TextInput.view true TextInput.empty))
    snapshotTest "widget-textinput-filled"
      (let model =
        "Hello World"
        |> Seq.fold (fun m c -> TextInput.handleKey (Key.Char c) m) TextInput.empty
       render 30 3 (TextInput.view true model))
    snapshotTest "widget-textinput-unfocused"
      (let model =
        "Hello"
        |> Seq.fold (fun m c -> TextInput.handleKey (Key.Char c) m) TextInput.empty
       render 30 3 (TextInput.view false model))

    // Select
    snapshotTest "widget-select-closed"
      (render 30 3 (Select.view id true (Select.create [ "Alpha"; "Beta"; "Gamma" ])))
    snapshotTest "widget-select-open"
      (render 30 8 (Select.view id true (
        Select.create [ "Alpha"; "Beta"; "Gamma" ] |> Select.toggle)))
    snapshotTest "widget-select-navigated"
      (render 30 8 (Select.view id true (
        Select.create [ "Alpha"; "Beta"; "Gamma" ]
        |> Select.toggle
        |> Select.moveDown)))

    // ProgressBar
    snapshotTest "widget-progress-0"
      (render 30 3 (ProgressBar.view { ProgressBar.defaults with Percent = 0.0; Width = 20 }))
    snapshotTest "widget-progress-50"
      (render 30 3 (ProgressBar.view { ProgressBar.defaults with Percent = 0.5; Width = 20 }))
    snapshotTest "widget-progress-100"
      (render 30 3 (ProgressBar.view { ProgressBar.defaults with Percent = 1.0; Width = 20 }))

    // Table
    snapshotTest "widget-table"
      (let columns = [
        { Header = "Name"; Width = 12; Render = fun (n: string, _: string) -> El.text n }
        { Header = "Score"; Width = 8; Render = fun (_, s) -> El.text s }
       ]
       render 30 8 (Table.view columns [ ("Alice", "95"); ("Bob", "87"); ("Carol", "91") ] None))
    snapshotTest "widget-table-selected"
      (let columns = [
        { Header = "Name"; Width = 12; Render = fun (n: string, _: string) -> El.text n }
        { Header = "Score"; Width = 8; Render = fun (_, s) -> El.text s }
       ]
       render 30 8 (Table.view columns [ ("Alice", "95"); ("Bob", "87"); ("Carol", "91") ] (Some 1)))

    // Tabs
    snapshotTest "widget-tabs-first"
      (render 40 3 (Tabs.view {
        Items = [ "Home"; "Settings"; "About" ]
        ActiveIndex = 0; ToString = id
        ActiveColor = None; InactiveColor = None }))
    snapshotTest "widget-tabs-second"
      (render 40 3 (Tabs.view {
        Items = [ "Home"; "Settings"; "About" ]
        ActiveIndex = 1; ToString = id
        ActiveColor = None; InactiveColor = None }))

    // Checkbox
    snapshotTest "widget-checkbox-checked"
      (render 30 3 (Checkbox.view "Accept terms" true true))
    snapshotTest "widget-checkbox-unchecked"
      (render 30 3 (Checkbox.view "Accept terms" false false))

    // Toggle
    snapshotTest "widget-toggle-on"
      (render 30 3 (Toggle.view "Enabled" "Disabled" true true))
    snapshotTest "widget-toggle-off"
      (render 30 3 (Toggle.view "Enabled" "Disabled" false false))

    // RadioGroup
    snapshotTest "widget-radio-default"
      (render 30 5 (RadioGroup.view id true (
        RadioGroup.create [ "Option A"; "Option B"; "Option C" ])))
    snapshotTest "widget-radio-moved"
      (render 30 5 (RadioGroup.view id true (
        RadioGroup.create [ "Option A"; "Option B"; "Option C" ]
        |> RadioGroup.moveDown)))

    // Toast
    snapshotTest "widget-toast"
      (render 30 5 (Toast.view (Toast.create "File saved!" 10)))

    // Modal
    snapshotTest "widget-modal"
      (render 40 12 (Modal.simple (El.text "Dialog Content")))

    // Spinner
    snapshotTest "widget-spinner-frame0"
      (render 10 3 (SpinnerWidget.view 0))
    snapshotTest "widget-spinner-frame3"
      (render 10 3 (SpinnerWidget.view 3))

    // SplitPane
    snapshotTest "widget-splitpane-horizontal"
      (render 40 10 (SplitPane.view
        (SplitPane.init SplitHorizontal 50
          (El.text "Left Pane" |> El.bordered Rounded)
          (El.text "Right Pane" |> El.bordered Rounded))))
    snapshotTest "widget-splitpane-vertical"
      (render 40 10 (SplitPane.view
        (SplitPane.init SplitVertical 50
          (El.text "Top Pane" |> El.bordered Rounded)
          (El.text "Bottom Pane" |> El.bordered Rounded))))
    snapshotTest "widget-splitpane-horizontal-grown"
      (render 40 10 (SplitPane.view
        (SplitPane.init SplitHorizontal 50
          (El.text "Left" |> El.bordered Rounded)
          (El.text "Right" |> El.bordered Rounded)
        |> SplitPane.grow 20)))
  ]

// ============================================================
// TREEVIEW SNAPSHOTS
// ============================================================

let private treeNodes =
  [ Branch("Documents", [
      Branch("Work", [
        Leaf "Report.doc"
        Leaf "Budget.xlsx"
      ])
      Branch("Personal", [
        Leaf "Photo.jpg"
      ])
    ])
    Leaf "README.md" ]

let treeViewSnapshots =
  testList "Snapshot.TreeView" [
    snapshotTest "tree-collapsed"
      (render 30 8 (TreeView.view id true treeNodes (TreeView.init ())))
    snapshotTest "tree-expanded"
      (render 30 10 (TreeView.view id true treeNodes (
        TreeView.expandAll treeNodes (TreeView.init ()))))
    snapshotTest "tree-partial-expand"
      (render 30 8 (TreeView.view id true treeNodes (
        TreeView.init () |> TreeView.expand [0])))
    snapshotTest "tree-cursor-moved"
      (render 30 8 (TreeView.view id true treeNodes (
        TreeView.init ()
        |> TreeView.expand [0]
        |> TreeView.moveCursorDown treeNodes)))
  ]

// ============================================================
// SCROLL SNAPSHOTS
// ============================================================

let private scrollItems = [ for i in 1..20 -> sprintf "Item %02d" i ]

let scrollSnapshots =
  testList "Snapshot.Scroll" [
    snapshotTest "scroll-top"
      (let scroll = ScrollState.create 20 5
       render 30 5 (Scroll.view scroll
         (fun i item -> El.text (sprintf "%d. %s" (i + 1) item)) scrollItems))
    snapshotTest "scroll-middle"
      (let scroll = ScrollState.create 20 5 |> ScrollState.scrollTo 10
       render 30 5 (Scroll.view scroll
         (fun i item -> El.text (sprintf "%d. %s" (i + 1) item)) scrollItems))
    snapshotTest "scroll-bottom"
      (let scroll = ScrollState.create 20 5 |> ScrollState.scrollToBottom
       render 30 5 (Scroll.view scroll
         (fun i item -> El.text (sprintf "%d. %s" (i + 1) item)) scrollItems))
    snapshotTest "scroll-with-indicator"
      (let scroll = ScrollState.create 20 5
       render 35 5 (Scroll.viewWithIndicator scroll
         (fun i item -> El.text (sprintf "%d. %s" (i + 1) item)) scrollItems))
    snapshotTest "scrollable-list"
      (let model =
        ScrollableList.create [ for i in 1..10 -> sprintf "Entry %d" i ] 5
        |> ScrollableList.selectIndex 3
       render 30 5 (ScrollableList.view (fun sel _ item ->
         let prefix = match sel with true -> "> " | false -> "  "
         El.text (prefix + item)
         |> (fun e -> match sel with true -> e |> El.bold | false -> e)) model))
  ]

// ============================================================
// TEA PROGRAM SNAPSHOTS
// ============================================================

let programSnapshots =
  testList "Snapshot.Programs" [
    snapshotTest "program-counter-init"
      (TestHarness.init 50 15 counterProgram |> TestHarness.render)
    snapshotTest "program-counter-3"
      (TestHarness.init 50 15 counterProgram
       |> TestHarness.pressKey (Key.Char 'j')
       |> TestHarness.pressKey (Key.Char 'j')
       |> TestHarness.pressKey (Key.Char 'j')
       |> TestHarness.render)
    snapshotTest "program-kanban-init"
      (TestHarness.init 80 25 kanbanProgram |> TestHarness.render)
    snapshotTest "program-kanban-navigated"
      (TestHarness.init 80 25 kanbanProgram
       |> TestHarness.pressKey Key.Right
       |> TestHarness.pressKey Key.Down
       |> TestHarness.render)
    snapshotTest "program-kanban-grabbed"
      (TestHarness.init 80 25 kanbanProgram
       |> TestHarness.pressKey (Key.Char ' ')
       |> TestHarness.pressKey Key.Right
       |> TestHarness.render)
    snapshotTest "program-form-init"
      (TestHarness.init 60 20 formProgram |> TestHarness.render)
    snapshotTest "program-form-filled"
      (TestHarness.init 60 20 formProgram
       |> TestHarness.typeText "Alice"
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.typeText "alice@example.com"
       |> TestHarness.render)
    snapshotTest "program-form-errors"
      (TestHarness.init 60 20 formProgram
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.pressKey Key.Enter
       |> TestHarness.render)
    snapshotTest "program-form-submitted"
      (TestHarness.init 60 20 formProgram
       |> TestHarness.typeText "Alice"
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.typeText "alice@example.com"
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.pressKey Key.Tab
       |> TestHarness.pressKey Key.Enter
       |> TestHarness.render)
  ]

// ============================================================
// ALL SNAPSHOT TESTS
// ============================================================

[<Tests>]
let snapshotTests =
  testList "Snapshot" [
    borderSnapshots
    layoutSnapshots
    widgetSnapshots
    treeViewSnapshots
    scrollSnapshots
    programSnapshots
  ]
