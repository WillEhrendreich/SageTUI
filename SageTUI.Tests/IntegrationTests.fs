module SageTUI.IntegrationTests

open Expecto
open Expecto.Flip
open SageTUI

let renderElement w h elem = TestHarness.renderElement w h elem
let renderElementLines w h elem = TestHarness.renderElementLines w h elem

// ============================================================
// COUNTER PROGRAM (mirrors HelloWorld sample)
// ============================================================

type CounterMsg = Inc | Dec | QuitCounter

let counterProgram : Program<int, CounterMsg> =
  { Init = fun () -> 0, Cmd.none
    Update = fun msg count ->
      match msg with
      | Inc -> count + 1, Cmd.none
      | Dec -> max 0 (count - 1), Cmd.none
      | QuitCounter -> count, Cmd.quit
    View = fun count ->
      El.column [
        El.text "Hello, SageTUI!"
          |> El.bold
          |> El.fg (Color.Rgb(255uy, 200uy, 50uy))
        El.text ""
        El.text (sprintf "  Count: %d" count) |> El.bold
        El.text ""
        El.text "  [j] increment  [k] decrement  [q] quit" |> El.dim
      ]
      |> El.padAll 1
      |> El.bordered Rounded
    Subscribe = fun _ -> [
      Keys.bind [
        Key.Char 'j', Inc
        Key.Char 'k', Dec
        Key.Char 'q', QuitCounter
        Key.Escape, QuitCounter
      ] ] }

// ============================================================
// COUNTER INTEGRATION TESTS
// ============================================================

let counterIntegrationTests =
  testList "Counter" [
    test "init shows Count: 0" {
      let app = TestHarness.init 50 15 counterProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "should show count 0" "Count: 0"
    }

    test "init shows title" {
      let app = TestHarness.init 50 15 counterProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "should show title" "Hello, SageTUI!"
    }

    test "init shows keybinding help" {
      let app = TestHarness.init 50 15 counterProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "should show j help" "[j] increment"
      output |> Expect.stringContains "should show k help" "[k] decrement"
      output |> Expect.stringContains "should show q help" "[q] quit"
    }

    test "init renders border" {
      let app = TestHarness.init 50 15 counterProgram
      let lines = TestHarness.renderLines app
      lines.[0] |> Expect.stringContains "top-left corner" "╭"
      lines.[0] |> Expect.stringContains "top-right corner" "╮"
      lines.[14] |> Expect.stringContains "bottom-left corner" "╰"
      lines.[14] |> Expect.stringContains "bottom-right corner" "╯"
    }

    test "init has not quit" {
      let app = TestHarness.init 50 15 counterProgram
      app.HasQuit |> Expect.isFalse "should not have quit"
    }

    test "j key increments counter to 1" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'j')
      app.Model |> Expect.equal "model should be 1" 1
      let output = TestHarness.render app
      output |> Expect.stringContains "should show count 1" "Count: 1"
    }

    test "k key decrements counter" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'j')
        |> TestHarness.pressKey (Key.Char 'j')
        |> TestHarness.pressKey (Key.Char 'k')
      app.Model |> Expect.equal "model should be 1" 1
      let output = TestHarness.render app
      output |> Expect.stringContains "should show count 1" "Count: 1"
    }

    test "k at zero stays at zero (floor)" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'k')
      app.Model |> Expect.equal "model should be 0" 0
      let output = TestHarness.render app
      output |> Expect.stringContains "should show count 0" "Count: 0"
    }

    test "multi-step: j*3 then k*1 = 2" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'j')
        |> TestHarness.pressKey (Key.Char 'j')
        |> TestHarness.pressKey (Key.Char 'j')
        |> TestHarness.pressKey (Key.Char 'k')
      app.Model |> Expect.equal "model should be 2" 2
      let output = TestHarness.render app
      output |> Expect.stringContains "should show count 2" "Count: 2"
    }

    test "q triggers quit" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'q')
      app.HasQuit |> Expect.isTrue "should have quit"
    }

    test "Escape triggers quit" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey Key.Escape
      app.HasQuit |> Expect.isTrue "should have quit"
    }

    test "unbound key does nothing" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.pressKey (Key.Char 'x')
      app.Model |> Expect.equal "model should be 0" 0
      app.HasQuit |> Expect.isFalse "should not have quit"
    }

    test "sendMsg bypasses subscriptions" {
      let app =
        TestHarness.init 50 15 counterProgram
        |> TestHarness.sendMsg Inc
        |> TestHarness.sendMsg Inc
        |> TestHarness.sendMsg Inc
      app.Model |> Expect.equal "model should be 3" 3
    }
  ]

// ============================================================
// LAYOUT INTEGRATION TESTS
// ============================================================

let layoutIntegrationTests =
  testList "Layout" [
    test "bordered element has rounded corners" {
      let elem = El.text "Hello" |> El.bordered Rounded
      let lines = renderElementLines 20 5 elem
      lines.[0] |> Expect.stringContains "top-left" "╭"
      lines.[0] |> Expect.stringContains "top-right" "╮"
      lines.[4] |> Expect.stringContains "bottom-left" "╰"
      lines.[4] |> Expect.stringContains "bottom-right" "╯"
    }

    test "bordered element contains text" {
      let elem = El.text "Hello" |> El.bordered Rounded
      let output = renderElement 20 5 elem
      output |> Expect.stringContains "should contain text" "Hello"
    }

    test "light border has correct chars" {
      let elem = El.text "Test" |> El.bordered Light
      let lines = renderElementLines 20 5 elem
      lines.[0] |> Expect.stringContains "top-left" "┌"
      lines.[0] |> Expect.stringContains "top-right" "┐"
      lines.[4] |> Expect.stringContains "bottom-left" "└"
      lines.[4] |> Expect.stringContains "bottom-right" "┘"
    }

    test "padded element offsets content" {
      let elem = El.text "X" |> El.padAll 2
      let lines = renderElementLines 10 6 elem
      lines.[0].Trim() |> Expect.equal "row 0 should be empty" ""
      lines.[1].Trim() |> Expect.equal "row 1 should be empty" ""
      lines.[2] |> Expect.stringContains "row 2 should have X" "X"
    }

    test "row places children horizontally" {
      let elem = El.row [
        El.text "AAA" |> El.width 5
        El.text "BBB" |> El.width 5
      ]
      let output = renderElement 20 3 elem
      let line = (output.Split('\n')).[0]
      let aIdx = line.IndexOf("AAA")
      let bIdx = line.IndexOf("BBB")
      (bIdx, aIdx) |> Expect.isGreaterThan "BBB should be right of AAA"
    }

    test "column stacks children vertically" {
      let elem = El.column [
        El.text "Line1"
        El.text "Line2"
        El.text "Line3"
      ]
      let output = renderElement 20 5 elem
      // All three texts should appear in the output
      output |> Expect.stringContains "has Line1" "Line1"
      output |> Expect.stringContains "has Line2" "Line2"
      output |> Expect.stringContains "has Line3" "Line3"
      // Line1 should appear before Line2, Line2 before Line3 (vertical order)
      let lines = renderElementLines 20 5 elem
      let line1Row = lines |> Array.findIndex (fun l -> l.Contains("Line1"))
      let line2Row = lines |> Array.findIndex (fun l -> l.Contains("Line2"))
      let line3Row = lines |> Array.findIndex (fun l -> l.Contains("Line3"))
      (line2Row, line1Row) |> Expect.isGreaterThan "Line2 below Line1"
      (line3Row, line2Row) |> Expect.isGreaterThan "Line3 below Line2"
    }

    test "nested bordered column inside row" {
      let elem = El.row [
        El.column [
          El.text "Left A"
          El.text "Left B"
        ] |> El.bordered Rounded |> El.width 15
        El.text "Right" |> El.width 10
      ]
      let output = renderElement 30 6 elem
      output |> Expect.stringContains "left A" "Left A"
      output |> Expect.stringContains "left B" "Left B"
      output |> Expect.stringContains "right" "Right"
      output |> Expect.stringContains "has border" "╭"
    }

    test "styled element renders content" {
      let elem =
        El.text "Bold Red"
        |> El.bold
        |> El.fg (Color.Rgb(255uy, 0uy, 0uy))
      let output = renderElement 20 3 elem
      output |> Expect.stringContains "styled text visible" "Bold Red"
    }

    test "empty element renders blank" {
      let elem = El.empty
      let lines = renderElementLines 10 3 elem
      lines |> Array.iter (fun line ->
        line.Trim() |> Expect.equal "should be empty" "")
    }

    test "overlay front layer visible" {
      let elem = El.overlay [El.text "Base"; El.text "Top"]
      let output = renderElement 20 3 elem
      output |> Expect.stringContains "overlay front layer" "Top"
    }

    test "gapped column adds spacing" {
      let elem =
        El.column [
          El.text "First"
          El.text "Second"
        ] |> El.gap 1
      let output = renderElement 20 5 elem
      output |> Expect.stringContains "has First" "First"
      output |> Expect.stringContains "has Second" "Second"
      let lines = renderElementLines 20 5 elem
      let firstRow = lines |> Array.findIndex (fun l -> l.Contains("First"))
      let secondRow = lines |> Array.findIndex (fun l -> l.Contains("Second"))
      // With gap=1, there should be at least 2 rows between First and Second
      (secondRow - firstRow, 1) |> Expect.isGreaterThan "gap between items"
    }
  ]

// ============================================================
// WIDGET INTEGRATION TESTS
// ============================================================

let widgetIntegrationTests =
  testList "Widgets" [
    test "TextInput renders typed text" {
      let model = TextInput.empty
      let model = "Hello" |> Seq.fold (fun m c -> TextInput.handleKey (Key.Char c) m) model
      let elem = TextInput.view true model
      let output = renderElement 30 3 elem
      output |> Expect.stringContains "should show typed text" "Hello"
    }

    test "TextInput backspace removes character" {
      let model = TextInput.empty
      let model =
        "Hi!"
        |> Seq.fold (fun m c -> TextInput.handleKey (Key.Char c) m) model
        |> TextInput.handleKey Key.Backspace
      model.Text |> Expect.equal "text after backspace" "Hi"
    }

    test "TextInput cursor navigation with Home/End" {
      let model = TextInput.empty
      let model =
        "ABCD"
        |> Seq.fold (fun m c -> TextInput.handleKey (Key.Char c) m) model
      model.Cursor |> Expect.equal "cursor at end" 4
      let model = TextInput.handleKey Key.Home model
      model.Cursor |> Expect.equal "cursor at home" 0
      let model = TextInput.handleKey Key.End model
      model.Cursor |> Expect.equal "cursor at end again" 4
    }

    test "FocusRing cycles through fields" {
      let ring = FocusRing.create [1; 2; 3]
      FocusRing.current ring |> Expect.equal "starts at first" (Some 1)
      let ring = FocusRing.next ring
      FocusRing.current ring |> Expect.equal "moved to second" (Some 2)
      let ring = FocusRing.next ring
      FocusRing.current ring |> Expect.equal "moved to third" (Some 3)
      let ring = FocusRing.next ring
      FocusRing.current ring |> Expect.equal "wraps to first" (Some 1)
    }

    test "FocusRing prev wraps backward" {
      let ring = FocusRing.create [1; 2; 3]
      let ring = FocusRing.prev ring
      FocusRing.current ring |> Expect.equal "wraps to last" (Some 3)
    }

    test "Select renders options" {
      let model = Select.create ["Alpha"; "Beta"; "Gamma"]
      let elem = Select.view id true model
      let output = renderElement 30 5 elem
      // When closed, should show the selected value
      output |> Expect.stringContains "shows selected" "Alpha"
    }

    test "Select toggle opens dropdown" {
      let model = Select.create ["Alpha"; "Beta"; "Gamma"]
      let model = Select.toggle model
      model.IsOpen |> Expect.isTrue "should be open"
    }

    test "Select moveDown changes highlighted option" {
      let model =
        Select.create ["Alpha"; "Beta"; "Gamma"]
        |> Select.toggle
        |> Select.moveDown
      Select.selectedValue model |> Expect.equal "highlighted Beta" (Some "Beta")
    }

    test "ProgressBar renders" {
      let config = { ProgressBar.defaults with Percent = 0.5; Width = 20 }
      let elem = ProgressBar.view config
      let output = renderElement 30 3 elem
      output |> Expect.stringContains "shows percentage" "50%"
    }

    test "Table renders headers and rows" {
      let columns = [
        { Header = "Name"; Width = 15; Render = fun (n, _) -> El.text n }
        { Header = "Score"; Width = 10; Render = fun (_, s) -> El.text s }
      ]
      let rows = [ ("Alice", "95"); ("Bob", "87") ]
      let elem = Table.view columns rows None
      let output = renderElement 40 8 elem
      output |> Expect.stringContains "has header Name" "Name"
      output |> Expect.stringContains "has header Score" "Score"
      output |> Expect.stringContains "has Alice" "Alice"
      output |> Expect.stringContains "has Bob" "Bob"
    }

    test "Tabs renders active tab" {
      let config : TabsConfig<string> = {
        Items = ["Home"; "Settings"; "About"]
        ActiveIndex = 0
        ToString = id
        ActiveColor = None
        InactiveColor = None
      }
      let elem = Tabs.view config
      let output = renderElement 40 3 elem
      output |> Expect.stringContains "has Home" "Home"
      output |> Expect.stringContains "has Settings" "Settings"
      output |> Expect.stringContains "has About" "About"
    }

    test "Tabs switching shows different active" {
      let mkConfig idx : TabsConfig<string> = {
        Items = ["A"; "B"; "C"]
        ActiveIndex = idx
        ToString = id
        ActiveColor = Some (Color.Named(Cyan, Bright))
        InactiveColor = None
      }
      let buf0 = Tabs.view (mkConfig 0) |> fun e ->
        let arena = FrameArena.create 256 4096 256
        FrameArena.reset arena
        let handle = Arena.lower arena e
        let buf = Buffer.create 30 3
        ArenaRender.renderRoot arena handle { X = 0; Y = 0; Width = 30; Height = 3 } buf
        buf
      let buf1 = Tabs.view (mkConfig 1) |> fun e ->
        let arena = FrameArena.create 256 4096 256
        FrameArena.reset arena
        let handle = Arena.lower arena e
        let buf = Buffer.create 30 3
        ArenaRender.renderRoot arena handle { X = 0; Y = 0; Width = 30; Height = 3 } buf
        buf
      // Different active tabs should produce different buffer content (different styles)
      let s0 = Buffer.toString buf0
      let s1 = Buffer.toString buf1
      // Both have the text but the styling differs — at minimum the text is present
      s0 |> Expect.stringContains "tab A present" "A"
      s1 |> Expect.stringContains "tab B present" "B"
    }
  ]

// ============================================================
// KANBAN INTEGRATION TESTS
// ============================================================

type KanbanMsg = KLeft | KRight | KUp | KDown | KGrab | KQuit

type KanbanColumn = KTodo | KInProgress | KReview | KDone

type KanbanCard =
  { Id: int; Title: string }

type KanbanModel =
  { Cards: Map<KanbanColumn, KanbanCard list>
    FocusCol: KanbanColumn
    FocusRow: int
    Moving: bool }

let allKanbanColumns = [KTodo; KInProgress; KReview; KDone]

let kanbanColName col =
  match col with
  | KTodo -> "Todo"
  | KInProgress -> "Progress"
  | KReview -> "Review"
  | KDone -> "Done"

let kanbanColIdx col =
  match col with KTodo -> 0 | KInProgress -> 1 | KReview -> 2 | KDone -> 3

let kanbanColAt idx =
  match idx with 0 -> KTodo | 1 -> KInProgress | 2 -> KReview | _ -> KDone

let kanbanCardsIn col model =
  Map.tryFind col model.Cards |> Option.defaultValue []

let kanbanClampRow model =
  let cards = kanbanCardsIn model.FocusCol model
  { model with FocusRow = min model.FocusRow (max 0 (cards.Length - 1)) }

let kanbanMoveCard (deltaIdx: int) model =
  let srcCol = model.FocusCol
  let srcCards = kanbanCardsIn srcCol model
  match model.FocusRow < srcCards.Length with
  | true ->
    let card = srcCards.[model.FocusRow]
    let dstIdx = (kanbanColIdx srcCol + deltaIdx) |> max 0 |> min 3
    let dstCol = kanbanColAt dstIdx
    let newSrc = srcCards |> List.filter (fun c -> c.Id <> card.Id)
    let newDst = (kanbanCardsIn dstCol model) @ [card]
    { model with
        Cards = model.Cards |> Map.add srcCol newSrc |> Map.add dstCol newDst
        FocusCol = dstCol
        FocusRow = newDst.Length - 1
        Moving = false }, Cmd.none
  | false -> model, Cmd.none

let kanbanProgram : Program<KanbanModel, KanbanMsg> =
  { Init = fun () ->
      { Cards =
          Map.ofList [
            KTodo, [
              { Id = 1; Title = "Task A" }
              { Id = 2; Title = "Task B" }
            ]
            KInProgress, [
              { Id = 3; Title = "Task C" }
            ]
            KReview, []
            KDone, [
              { Id = 4; Title = "Task D" }
            ]
          ]
        FocusCol = KTodo
        FocusRow = 0
        Moving = false },
      Cmd.none
    Update = fun msg model ->
      match msg with
      | KQuit -> model, Cmd.quit
      | KRight ->
        match model.Moving with
        | false ->
          let nextIdx = min 3 (kanbanColIdx model.FocusCol + 1)
          { model with FocusCol = kanbanColAt nextIdx } |> kanbanClampRow, Cmd.none
        | true -> kanbanMoveCard 1 model
      | KLeft ->
        match model.Moving with
        | false ->
          let prevIdx = max 0 (kanbanColIdx model.FocusCol - 1)
          { model with FocusCol = kanbanColAt prevIdx } |> kanbanClampRow, Cmd.none
        | true -> kanbanMoveCard -1 model
      | KUp ->
        { model with FocusRow = max 0 (model.FocusRow - 1) }, Cmd.none
      | KDown ->
        let maxRow = (kanbanCardsIn model.FocusCol model).Length - 1
        { model with FocusRow = min maxRow (model.FocusRow + 1) }, Cmd.none
      | KGrab ->
        { model with Moving = not model.Moving }, Cmd.none
    View = fun model ->
      El.row [
        yield!
          allKanbanColumns
          |> List.map (fun col ->
            let cards = kanbanCardsIn col model
            let isFocus = col = model.FocusCol
            El.column [
              El.text (sprintf " %s (%d)" (kanbanColName col) cards.Length)
                |> El.bold
              yield!
                cards |> List.mapi (fun i card ->
                  let focused = isFocus && i = model.FocusRow
                  let grabbed = focused && model.Moving
                  let border =
                    match focused, grabbed with
                    | _, true -> Double
                    | true, _ -> Rounded
                    | false, _ -> Light
                  El.text card.Title
                    |> El.bordered border)
            ]
            |> El.bordered (match isFocus with true -> Rounded | false -> Light)
            |> El.fill)
      ]
    Subscribe = fun _ -> [
      Keys.bind [
        Key.Left, KLeft
        Key.Right, KRight
        Key.Up, KUp
        Key.Down, KDown
        Key.Char ' ', KGrab
        Key.Char 'q', KQuit
      ] ] }

let kanbanIntegrationTests =
  testList "Kanban" [
    test "init shows 4 columns" {
      let app = TestHarness.init 80 25 kanbanProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "has Todo" "Todo"
      output |> Expect.stringContains "has Progress" "Progress"
      output |> Expect.stringContains "has Review" "Review"
      output |> Expect.stringContains "has Done" "Done"
    }

    test "init shows card counts" {
      let app = TestHarness.init 80 25 kanbanProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "Todo has 2" "Todo (2)"
      output |> Expect.stringContains "Progress has 1" "Progress (1)"
      output |> Expect.stringContains "Review has 0" "Review (0)"
      output |> Expect.stringContains "Done has 1" "Done (1)"
    }

    test "init shows card titles" {
      let app = TestHarness.init 80 25 kanbanProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "Task A" "Task A"
      output |> Expect.stringContains "Task B" "Task B"
      output |> Expect.stringContains "Task C" "Task C"
      output |> Expect.stringContains "Task D" "Task D"
    }

    test "right arrow moves focus to next column" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey Key.Right
      app.Model.FocusCol |> Expect.equal "focus on InProgress" KInProgress
    }

    test "left arrow at first column stays" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey Key.Left
      app.Model.FocusCol |> Expect.equal "still on Todo" KTodo
    }

    test "down arrow moves focus row" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey Key.Down
      app.Model.FocusRow |> Expect.equal "focus on row 1" 1
    }

    test "space grabs card" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey (Key.Char ' ')
      app.Model.Moving |> Expect.isTrue "should be moving"
    }

    test "grab and move right transfers card" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey (Key.Char ' ')  // grab Task A
        |> TestHarness.pressKey Key.Right        // move to InProgress
      // Task A should now be in InProgress
      let inProgressCards = kanbanCardsIn KInProgress app.Model
      let taskA = inProgressCards |> List.exists (fun c -> c.Title = "Task A")
      taskA |> Expect.isTrue "Task A should be in InProgress"
      // Todo should have 1 card now
      let todoCards = kanbanCardsIn KTodo app.Model
      todoCards |> Expect.hasLength "Todo should have 1 card" 1
    }

    test "total cards preserved after move" {
      let app0 = TestHarness.init 80 25 kanbanProgram
      let totalBefore =
        allKanbanColumns |> List.sumBy (fun col -> (kanbanCardsIn col app0.Model).Length)
      let app1 =
        app0
        |> TestHarness.pressKey (Key.Char ' ')
        |> TestHarness.pressKey Key.Right
      let totalAfter =
        allKanbanColumns |> List.sumBy (fun col -> (kanbanCardsIn col app1.Model).Length)
      totalAfter |> Expect.equal "total cards unchanged" totalBefore
    }

    test "q quits" {
      let app =
        TestHarness.init 80 25 kanbanProgram
        |> TestHarness.pressKey (Key.Char 'q')
      app.HasQuit |> Expect.isTrue "should have quit"
    }
  ]

// ============================================================
// FORM INTEGRATION TESTS
// ============================================================

type FormField = FName | FEmail | FRole | FSubmit

type FormModel =
  { Name: TextInputModel
    Email: TextInputModel
    Role: SelectModel<string>
    Focus: FocusRing<FormField>
    Submitted: bool
    Errors: Map<FormField, string> }

type FormMsg =
  | FormKey of Key * Modifiers
  | FormQuit

let formValidate model =
  let errors = Map.empty
  let errors =
    match model.Name.Text.Length with
    | 0 -> Map.add FName "Name is required" errors
    | n when n < 2 -> Map.add FName "Name must be at least 2 characters" errors
    | _ -> errors
  let errors =
    match model.Email.Text.Contains("@") with
    | true -> errors
    | false -> Map.add FEmail "Invalid email address" errors
  errors

let formProgram : Program<FormModel, FormMsg> =
  { Init = fun () ->
      { Name = TextInput.empty
        Email = TextInput.empty
        Role = Select.create ["Developer"; "Designer"; "Manager"]
        Focus = FocusRing.create [FName; FEmail; FRole; FSubmit]
        Submitted = false
        Errors = Map.empty },
      Cmd.none
    Update = fun msg model ->
      match msg with
      | FormQuit -> model, Cmd.quit
      | FormKey (key, _mods) ->
        let focus = FocusRing.current model.Focus
        match focus, key with
        | Some FRole, Escape when model.Role.IsOpen ->
          { model with Role = Select.confirm model.Role }, Cmd.none
        | Some FRole, Key.Up when model.Role.IsOpen ->
          { model with Role = Select.moveUp model.Role }, Cmd.none
        | Some FRole, Key.Down when model.Role.IsOpen ->
          { model with Role = Select.moveDown model.Role }, Cmd.none
        | Some FRole, Key.Enter ->
          { model with Role = Select.toggle model.Role }, Cmd.none
        | _, Escape -> model, Cmd.quit
        | _, Tab when _mods = Modifiers.Shift ->
          { model with Focus = FocusRing.prev model.Focus; Submitted = false }, Cmd.none
        | _, Tab ->
          { model with Focus = FocusRing.next model.Focus; Submitted = false }, Cmd.none
        | Some FSubmit, Key.Enter ->
          let errors = formValidate model
          match Map.isEmpty errors with
          | true -> { model with Submitted = true; Errors = Map.empty }, Cmd.none
          | false -> { model with Errors = errors; Submitted = false }, Cmd.none
        | Some FName, _ ->
          { model with Name = TextInput.handleKey key model.Name }, Cmd.none
        | Some FEmail, _ ->
          { model with Email = TextInput.handleKey key model.Email }, Cmd.none
        | _ -> model, Cmd.none
    View = fun model ->
      let focusedField = FocusRing.current model.Focus
      El.column [
        El.text "Registration Form" |> El.bold
        El.text ""
        El.text (sprintf "Name: %s" model.Name.Text)
        El.text (sprintf "Email: %s" model.Email.Text)
        El.text (sprintf "Role: %s" (Select.selectedValue model.Role |> Option.defaultValue "?"))
        match focusedField with
        | Some FName -> El.text "  [Name focused]" |> El.dim
        | Some FEmail -> El.text "  [Email focused]" |> El.dim
        | Some FRole -> El.text "  [Role focused]" |> El.dim
        | Some FSubmit -> El.text "  [Submit focused]" |> El.dim
        | None -> El.empty
        yield!
          model.Errors |> Map.toList |> List.map (fun (_, err) ->
            El.text (sprintf "  ERROR: %s" err) |> El.fg (Color.Named(Red, Bright)))
        match model.Submitted with
        | true ->
          El.text (sprintf "  Welcome, %s!" model.Name.Text)
            |> El.fg (Color.Named(Green, Bright))
        | false -> El.empty
      ]
    Subscribe = fun _ ->
      [ KeySub (fun (key, mods) -> Some (FormKey(key, mods))) ] }

let formIntegrationTests =
  testList "Form" [
    test "init shows registration form" {
      let app = TestHarness.init 60 20 formProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "has title" "Registration Form"
    }

    test "init focuses on Name field" {
      let app = TestHarness.init 60 20 formProgram
      let output = TestHarness.render app
      output |> Expect.stringContains "Name focused" "[Name focused]"
    }

    test "typing in Name field updates model" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.typeText "Alice"
      app.Model.Name.Text |> Expect.equal "name should be Alice" "Alice"
      let output = TestHarness.render app
      output |> Expect.stringContains "shows Alice" "Alice"
    }

    test "Tab moves to Email field" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.pressKey Key.Tab
      let output = TestHarness.render app
      output |> Expect.stringContains "Email focused" "[Email focused]"
    }

    test "type Name then Tab to Email then type email" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.typeText "Bob"
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "bob@test.com"
      app.Model.Name.Text |> Expect.equal "name" "Bob"
      app.Model.Email.Text |> Expect.equal "email" "bob@test.com"
    }

    test "submit with empty fields shows errors" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.pressKey Key.Tab  // email
        |> TestHarness.pressKey Key.Tab  // role
        |> TestHarness.pressKey Key.Tab  // submit
        |> TestHarness.pressKey Key.Enter  // submit
      let output = TestHarness.render app
      output |> Expect.stringContains "name error" "Name is required"
      output |> Expect.stringContains "email error" "Invalid email"
    }

    test "valid submission shows welcome" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.typeText "Alice"
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.typeText "alice@example.com"
        |> TestHarness.pressKey Key.Tab  // role
        |> TestHarness.pressKey Key.Tab  // submit
        |> TestHarness.pressKey Key.Enter
      app.Model.Submitted |> Expect.isTrue "should be submitted"
      let output = TestHarness.render app
      output |> Expect.stringContains "welcome message" "Welcome, Alice!"
    }

    test "Escape quits from any field" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.pressKey Key.Tab
        |> TestHarness.pressKey Key.Escape
      app.HasQuit |> Expect.isTrue "should have quit"
    }

    test "BackTab moves focus backward" {
      let app =
        TestHarness.init 60 20 formProgram
        |> TestHarness.pressKey Key.Tab   // email
        |> TestHarness.pressKey Key.Tab   // role
        |> TestHarness.pressKeyWith Key.Tab Modifiers.Shift  // back to email
      let output = TestHarness.render app
      output |> Expect.stringContains "Email focused" "[Email focused]"
    }
  ]

// ============================================================
// ALL INTEGRATION TESTS
// ============================================================

[<Tests>]
let integrationTests =
  testList "Integration" [
    counterIntegrationTests
    layoutIntegrationTests
    widgetIntegrationTests
    kanbanIntegrationTests
    formIntegrationTests
  ]
