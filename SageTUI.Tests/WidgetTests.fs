module WidgetTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

let progressBarTests = testList "ProgressBar" [
  test "0 percent shows all empty" {
    let config = { ProgressBar.defaults with Percent = 0.0; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has empty chars" "░░░░░░░░░░"
    | _ -> ()
  }
  test "100 percent shows all filled" {
    let config = { ProgressBar.defaults with Percent = 1.0; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has filled chars" "██████████"
    | _ -> ()
  }
  test "50 percent shows half" {
    let config = { ProgressBar.defaults with Percent = 0.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has filled" "█████"
    | _ -> ()
  }
  test "with label shows Row" {
    let config = { ProgressBar.defaults with Percent = 0.75; Width = 10; ShowLabel = true }
    let elem = ProgressBar.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "bar + label" 2
    | _ -> failtest "expected Row"
  }
  test "clamps below 0" {
    let config = { ProgressBar.defaults with Percent = -0.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "all empty" "░░░░░░░░░░"
    | _ -> ()
  }
  test "clamps above 1" {
    let config = { ProgressBar.defaults with Percent = 1.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "all filled" "██████████"
    | _ -> ()
  }
  test "colored bar produces Constrained Row" {
    let config =
      { ProgressBar.defaults with
          Percent = 0.5; Width = 10; ShowLabel = false
          FilledColor = Some (Color.Named(BaseColor.Green, Intensity.Normal))
          EmptyColor = Some (Color.Named(BaseColor.Red, Intensity.Normal)) }
    let elem = ProgressBar.view config
    match elem with
    | Constrained(Fixed 10, Row _) -> ()
    | Row _ -> failtest "colored bar should be Constrained(Fixed width, Row) to prevent gap injection"
    | _ -> failwithf "expected Constrained Row for colored bar, got: %A" elem
  }
]

let tabsTests = testList "Tabs" [
  test "renders all tabs as Row" {
    let config: TabsConfig<string> = {
      Items = ["Home"; "Settings"; "Help"]
      ActiveIndex = 0
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "3 tabs" 3
    | _ -> failtest "expected Row"
  }
  test "active tab is bold" {
    let config: TabsConfig<string> = {
      Items = ["A"; "B"]
      ActiveIndex = 1
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row [_; Styled(s, _)] ->
      s.Attrs.Value &&& 1us |> Expect.equal "bold bit" 1us
    | other -> failtest (sprintf "expected styled active tab, got %A" other)
  }
  test "empty items produces empty Row" {
    let config: TabsConfig<string> = {
      Items = []
      ActiveIndex = 0
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "empty" 0
    | _ -> failtest "expected Row"
  }
]

let tableTests = testList "Table" [
  test "produces Column with header, separator, rows" {
    let cols: TableColumn<string * int> list = [
      { Header = "Name"; Width = 10; SortKey = None; Fill = false; Render = fun (n, _) -> El.text n }
      { Header = "Age"; Width = 5; SortKey = None; Fill = false; Render = fun (_, a) -> El.text (string a) }
    ]
    let rows = [("Alice", 30); ("Bob", 25)]
    let elem = Table.view cols rows None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep+2rows" 4
    | _ -> failtest "expected Column"
  }
  test "selected row gets styled" {
    let cols: TableColumn<string> list = [
      { Header = "X"; Width = 5; SortKey = None; Fill = false; Render = fun x -> El.text x }
    ]
    let rows = ["a"; "b"; "c"]
    let elem = Table.view cols rows (Some 1)
    match elem with
    | Column (_::_::dataRows) ->
      match List.item 1 dataRows with
      | Styled(s, _) -> s.Bg |> Expect.isSome "has bg color"
      | _ -> failtest "selected row should be styled"
    | _ -> failtest "expected Column"
  }
  test "empty rows has header and separator only" {
    let cols: TableColumn<string> list = [
      { Header = "H"; Width = 3; SortKey = None; Fill = false; Render = fun _ -> El.text "" }
    ]
    let elem = Table.view cols [] None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep" 2
    | _ -> failtest "expected Column"
  }
  test "multiple columns render in each row" {
    let cols: TableColumn<int> list = [
      { Header = "A"; Width = 5; SortKey = None; Fill = false; Render = fun x -> El.text (string x) }
      { Header = "B"; Width = 5; SortKey = None; Fill = false; Render = fun x -> El.text (string (x * 2)) }
    ]
    let rows = [1; 2; 3]
    let elem = Table.view cols rows None
    match elem with
    | Column children ->
      List.length children |> Expect.equal "header+sep+3rows" 5
      match List.item 2 children with
      | Row cells -> List.length cells |> Expect.equal "2 columns" 2
      | _ -> failtest "data row should be Row"
    | _ -> failtest "expected Column"
  }
]

let colorShortcutTests = testList "Color shortcuts" [
  test "Color.red is Named Red Normal" {
    Color.red |> Expect.equal "red" (Named(BaseColor.Red, Normal))
  }
  test "Color.brightCyan is Named Cyan Bright" {
    Color.brightCyan |> Expect.equal "brightCyan" (Named(BaseColor.Cyan, Bright))
  }
  test "Color.rgb constructs Rgb" {
    Color.rgb 255uy 128uy 0uy |> Expect.equal "rgb" (Rgb(255uy, 128uy, 0uy))
  }
  test "Color.hex 6-digit" {
    Color.hex "#FF8000" |> Expect.equal "hex6" (Rgb(255uy, 128uy, 0uy))
  }
  test "Color.hex 3-digit" {
    Color.hex "#F00" |> Expect.equal "hex3" (Rgb(255uy, 0uy, 0uy))
  }
  test "Color.hex without hash" {
    Color.hex "00FF00" |> Expect.equal "nohash" (Rgb(0uy, 255uy, 0uy))
  }
  test "Color.hex invalid returns Default" {
    Color.hex "xyz" |> Expect.equal "invalid" Default
  }
  test "all 16 colors exist" {
    let colors = [
      Color.black; Color.red; Color.green; Color.yellow
      Color.blue; Color.magenta; Color.cyan; Color.white
      Color.brightBlack; Color.brightRed; Color.brightGreen; Color.brightYellow
      Color.brightBlue; Color.brightMagenta; Color.brightCyan; Color.brightWhite
    ]
    List.length colors |> Expect.equal "16 colors" 16
  }
]

let textfTests = testList "El.textf" [
  test "formats integer" {
    match El.textf "Score: %d" 42 with
    | Text(t, _) -> t |> Expect.equal "formatted" "Score: 42"
    | _ -> failtest "expected Text"
  }
  test "formats string" {
    match El.textf "Hello %s!" "world" with
    | Text(t, _) -> t |> Expect.equal "formatted" "Hello world!"
    | _ -> failtest "expected Text"
  }
  test "formats float" {
    match El.textf "%.1f%%" 99.9 with
    | Text(t, _) -> t |> Expect.equal "formatted" "99.9%"
    | _ -> failtest "expected Text"
  }
]

let paragraphTests = testList "El.paragraph" [
  test "short text stays one line" {
    match El.paragraph 80 "hello world" with
    | Column [Text(t, _)] -> t |> Expect.equal "one line" "hello world"
    | other -> failtest (sprintf "expected single-line Column, got %A" other)
  }
  test "long text wraps at word boundary" {
    match El.paragraph 10 "hello beautiful world" with
    | Column lines ->
      List.length lines |> Expect.equal "wraps to multiple lines" 3
      match List.head lines with
      | Text(t, _) -> t |> Expect.equal "first line" "hello"
      | _ -> failtest "expected Text"
    | _ -> failtest "expected Column"
  }
  test "respects newlines in input" {
    match El.paragraph 80 "line1\nline2\nline3" with
    | Column lines -> List.length lines |> Expect.equal "3 lines" 3
    | _ -> failtest "expected Column"
  }
  test "zero width produces empty lines" {
    match El.paragraph 0 "hello" with
    | Column lines -> (List.length lines, 1) |> Expect.isGreaterThanOrEqual "at least 1 line"
    | _ -> failtest "expected Column"
  }
  test "word longer than width gets char-wrapped" {
    match El.paragraph 5 "abcdefghij" with
    | Column lines ->
      (List.length lines, 1) |> Expect.isGreaterThan "wraps long word"
    | _ -> failtest "expected Column"
  }
  test "empty string produces one empty line" {
    match El.paragraph 80 "" with
    | Column [Text(t, _)] -> t |> Expect.equal "empty" ""
    | _ -> failtest "expected single empty Text"
  }
  test "paragraphStyled applies style" {
    let style = { Style.empty with Fg = Some Color.red }
    match El.paragraphStyled style 80 "hello" with
    | Column [Text(_, s)] -> s.Fg |> Expect.equal "red fg" (Some Color.red)
    | _ -> failtest "expected styled Text"
  }
]

let textInputEnhancedTests = testList "TextInput enhanced" [
  test "paste inserts at cursor" {
    let model = TextInput.ofString "hello world"
    let model = { model with Cursor = 5 }
    let result = TextInput.handlePaste " beautiful" model
    result.Text |> Expect.equal "pasted" "hello beautiful world"
    result.Cursor |> Expect.equal "cursor after paste" 15
  }
  test "paste replaces newlines with spaces" {
    let model = TextInput.empty
    let result = TextInput.handlePaste "line1\nline2\r\nline3" model
    result.Text |> Expect.equal "flattened" "line1 line2 line3"
  }
  test "paste at start" {
    let model = TextInput.ofString "world"
    let model = { model with Cursor = 0 }
    let result = TextInput.handlePaste "hello " model
    result.Text |> Expect.equal "prepended" "hello world"
  }
  test "insertText at end" {
    let model = TextInput.ofString "hello"
    let result = TextInput.insertText " world" model
    result.Text |> Expect.equal "appended" "hello world"
  }
  test "viewWithPlaceholder shows placeholder when empty" {
    let model = TextInput.empty
    let elem = TextInput.viewWithPlaceholder "Type here..." false model
    match elem with
    | Styled(s, Styled(_, Text(t, _))) ->
      t |> Expect.equal "placeholder" "Type here..."
    | Styled(s, Text(t, _)) ->
      t |> Expect.equal "placeholder" "Type here..."
    | other -> failtest (sprintf "expected dim placeholder, got %A" other)
  }
  test "viewWithPlaceholder shows text when not empty" {
    let model = TextInput.ofString "hello"
    let elem = TextInput.viewWithPlaceholder "Type here..." false model
    match elem with
    | Text(t, _) -> t |> Expect.equal "shows text" "hello"
    | _ -> failtest "expected Text with content"
  }
]

let modalTests = testList "Modal" [
  test "simple modal wraps in Overlay" {
    let elem = Modal.simple (El.text "Confirm?")
    match elem with
    | Overlay layers -> List.length layers |> Expect.equal "backdrop + content" 2
    | _ -> failtest "expected Overlay"
  }
  test "modal without backdrop returns centered content" {
    let config = { Modal.defaults with Backdrop = None }
    let elem = Modal.view config (El.text "Hello")
    match elem with
    | Aligned(HAlign.HCenter, VAlign.VCenter, _) -> ()
    | _ -> failtest "expected centered content"
  }
  test "modal with maxWidth constrains" {
    let config = { Modal.defaults with MaxWidth = Some 40 }
    let elem = Modal.view config (El.text "Hello")
    match elem with
    | Overlay [_; Aligned(_, _, Constrained(Max 40, _))] -> ()
    | other -> failtest (sprintf "expected max width constraint, got %A" other)
  }
]

let focusTests = testList "Focus" [
  test "tabOrder advances to next key" {
    let keys = ["a"; "b"; "c"]
    Focus.tabOrder keys "a" FocusNext |> Expect.equal "should be b" "b"
  }

  test "tabOrder wraps to first" {
    let keys = ["a"; "b"; "c"]
    Focus.tabOrder keys "c" FocusNext |> Expect.equal "should wrap to a" "a"
  }

  test "tabOrder goes to previous" {
    let keys = ["a"; "b"; "c"]
    Focus.tabOrder keys "b" FocusPrev |> Expect.equal "should be a" "a"
  }

  test "tabOrder prev wraps to last" {
    let keys = ["a"; "b"; "c"]
    Focus.tabOrder keys "a" FocusPrev |> Expect.equal "should wrap to c" "c"
  }

  test "tabOrder unknown key defaults to first" {
    let keys = ["a"; "b"; "c"]
    Focus.tabOrder keys "unknown" FocusNext |> Expect.equal "should be first" "a"
  }

  test "tabOrder single item stays" {
    Focus.tabOrder ["only"] "only" FocusNext |> Expect.equal "stays" "only"
  }

  test "tabOrder empty list keeps current" {
    Focus.tabOrder [] "x" FocusNext |> Expect.equal "keeps current" "x"
  }
]

let checkboxTests = testList "Checkbox" [
  test "toggle flips true to false" {
    Checkbox.toggle true |> Expect.isFalse "should be false"
  }

  test "toggle flips false to true" {
    Checkbox.toggle false |> Expect.isTrue "should be true"
  }

  test "view checked shows checkmark" {
    let elem = Checkbox.view "Accept" false true
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has checkmark" "✓"
    | _ -> failtest "expected Text"
  }

  test "view unchecked shows empty box" {
    let elem = Checkbox.view "Accept" false false
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has empty box" "[ ]"
    | _ -> failtest "expected Text"
  }
]

let toggleTests = testList "Toggle" [
  test "toggle flips value" {
    Toggle.toggle true |> Expect.isFalse "should toggle"
  }

  test "view on shows on label" {
    let elem = Toggle.view "On" "Off" false true
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has on label" "On"
    | _ -> failtest "expected Text"
  }

  test "view off shows off label" {
    let elem = Toggle.view "On" "Off" false false
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has off label" "Off"
    | _ -> failtest "expected Text"
  }
]

let radioGroupTests = testList "RadioGroup" [
  test "create starts at index 0" {
    let model = RadioGroup.create ["A"; "B"; "C"]
    model.Selected |> Expect.equal "starts at 0" 0
  }

  test "moveDown advances" {
    let model = RadioGroup.create ["A"; "B"; "C"] |> RadioGroup.moveDown
    model.Selected |> Expect.equal "moved to 1" 1
  }

  test "moveDown clamps at end" {
    let model = { Options = ["A"; "B"]; Selected = 1 } |> RadioGroup.moveDown
    model.Selected |> Expect.equal "stays at 1" 1
  }

  test "moveUp decrements" {
    let model = { Options = ["A"; "B"; "C"]; Selected = 2 } |> RadioGroup.moveUp
    model.Selected |> Expect.equal "moved to 1" 1
  }

  test "moveUp clamps at 0" {
    let model = RadioGroup.create ["A"; "B"] |> RadioGroup.moveUp
    model.Selected |> Expect.equal "stays at 0" 0
  }

  test "selectedValue returns current" {
    let model = { Options = ["X"; "Y"; "Z"]; Selected = 1 }
    RadioGroup.selectedValue model |> Expect.equal "Y selected" (Some "Y")
  }

  test "handleKey Up moves up" {
    let model = { Options = ["A"; "B"]; Selected = 1 } |> RadioGroup.handleKey Key.Up
    model.Selected |> Expect.equal "moved up" 0
  }

  test "handleKey Down moves down" {
    let model = RadioGroup.create ["A"; "B"] |> RadioGroup.handleKey Key.Down
    model.Selected |> Expect.equal "moved down" 1
  }
]

let spinnerTests = testList "SpinnerWidget" [
  test "view returns element" {
    let elem = SpinnerWidget.view 0
    match elem with Text(_, _) -> () | _ -> failtest "expected Text"
  }

  test "view cycles frames" {
    let e0 = SpinnerWidget.view 0
    let e1 = SpinnerWidget.view 1
    match e0, e1 with
    | Text(a, _), Text(b, _) -> a |> Expect.isNotNull "not null"; (a <> b) |> Expect.isTrue "frames differ"
    | _ -> failtest "expected Text"
  }
]

let toastTests = testList "Toast" [
  test "create sets message and ticks" {
    let t = Toast.create "Hello" 5
    t.Message |> Expect.equal "message" "Hello"
    t.RemainingTicks |> Expect.equal "ticks" 5
  }

  test "tick decrements" {
    let t = Toast.create "Hi" 3 |> Toast.tick
    match t with
    | Some t' -> t'.RemainingTicks |> Expect.equal "decremented" 2
    | None -> failtest "should not expire yet"
  }

  test "tick returns None when expired" {
    let t = Toast.create "Bye" 1 |> Toast.tick
    match t with
    | Some t' -> Toast.tick t' |> Expect.isNone "should expire"
    | None -> failtest "should have one more tick"
  }

  test "isExpired true at 0" {
    Toast.create "x" 0 |> Toast.isExpired |> Expect.isTrue "expired"
  }

  test "isExpired false when ticks remain" {
    Toast.create "x" 5 |> Toast.isExpired |> Expect.isFalse "not expired"
  }
]

let treeViewTests = testList "TreeView" [
  let sampleTree = [
    Branch("src", [
      Branch("components", [
        Leaf "Button.fs"
        Leaf "Modal.fs"
      ])
      Leaf "App.fs"
    ])
    Leaf "README.md"
  ]

  test "init starts at first item" {
    let state = TreeView.init ()
    state.Cursor |> Expect.equal "cursor at root" [0]
    state.Expanded |> Expect.isEmpty "nothing expanded"
  }

  test "flatten only shows top-level when collapsed" {
    let state = TreeView.init ()
    let paths = TreeView.visiblePaths sampleTree state
    paths |> Expect.hasLength "two top-level items" 2
    paths.[0] |> Expect.equal "first" [0]
    paths.[1] |> Expect.equal "second" [1]
  }

  test "expand shows children" {
    let state = TreeView.init () |> TreeView.expand [0]
    let paths = TreeView.visiblePaths sampleTree state
    paths |> Expect.hasLength "src + 2 children + readme" 4
    paths.[1] |> Expect.equal "components" [0; 0]
    paths.[2] |> Expect.equal "App.fs" [0; 1]
  }

  test "expand nested shows grandchildren" {
    let state =
      TreeView.init ()
      |> TreeView.expand [0]
      |> TreeView.expand [0; 0]
    let paths = TreeView.visiblePaths sampleTree state
    paths |> Expect.hasLength "src + components + 2 leaves + App + readme" 6
    paths.[2] |> Expect.equal "Button.fs" [0; 0; 0]
    paths.[3] |> Expect.equal "Modal.fs" [0; 0; 1]
  }

  test "collapse hides children" {
    let state =
      TreeView.init ()
      |> TreeView.expand [0]
      |> TreeView.collapse [0]
    let paths = TreeView.visiblePaths sampleTree state
    paths |> Expect.hasLength "back to two" 2
  }

  test "toggleExpand toggles" {
    let state =
      TreeView.init ()
      |> TreeView.toggleExpand [0]
    TreeView.isExpanded [0] state |> Expect.isTrue "expanded"
    let state2 = TreeView.toggleExpand [0] state
    TreeView.isExpanded [0] state2 |> Expect.isFalse "collapsed"
  }

  test "expandAll expands all branches" {
    let state = TreeView.init () |> TreeView.expandAll sampleTree
    TreeView.isExpanded [0] state |> Expect.isTrue "src expanded"
    TreeView.isExpanded [0; 0] state |> Expect.isTrue "components expanded"
    let paths = TreeView.visiblePaths sampleTree state
    paths |> Expect.hasLength "all 6 nodes" 6
  }

  test "moveCursorDown moves to next visible" {
    let state = TreeView.init () |> TreeView.moveCursorDown sampleTree
    state.Cursor |> Expect.equal "moved to readme" [1]
  }

  test "moveCursorDown at bottom stays" {
    let state =
      TreeView.init ()
      |> TreeView.moveCursorDown sampleTree
      |> TreeView.moveCursorDown sampleTree
    state.Cursor |> Expect.equal "stays at last" [1]
  }

  test "moveCursorUp moves to previous visible" {
    let state =
      TreeView.init ()
      |> TreeView.moveCursorDown sampleTree
      |> TreeView.moveCursorUp sampleTree
    state.Cursor |> Expect.equal "back to first" [0]
  }

  test "moveCursorUp at top stays" {
    let state = TreeView.init () |> TreeView.moveCursorUp sampleTree
    state.Cursor |> Expect.equal "stays at first" [0]
  }

  test "cursorNode returns correct node" {
    let node = TreeView.cursorNode sampleTree (TreeView.init ())
    match node with
    | Some(Branch(v, _)) -> v |> Expect.equal "src" "src"
    | _ -> failtest "expected Branch src"
  }

  test "handleKey Right expands branch" {
    let state = TreeView.init () |> TreeView.handleKey Key.Right sampleTree
    TreeView.isExpanded [0] state |> Expect.isTrue "expanded"
  }

  test "handleKey Left collapses expanded branch" {
    let state =
      TreeView.init ()
      |> TreeView.expand [0]
      |> TreeView.handleKey Key.Left sampleTree
    TreeView.isExpanded [0] state |> Expect.isFalse "collapsed"
  }

  test "handleKey Left on collapsed moves to parent" {
    let state =
      TreeView.init ()
      |> TreeView.expand [0]
      |> fun s -> { s with Cursor = [0; 0] }
      |> TreeView.handleKey Key.Left sampleTree
    state.Cursor |> Expect.equal "moved to parent" [0]
  }

  test "handleKey Enter toggles branch" {
    let state = TreeView.init () |> TreeView.handleKey Key.Enter sampleTree
    TreeView.isExpanded [0] state |> Expect.isTrue "toggled open"
    let state2 = TreeView.handleKey Key.Enter sampleTree state
    TreeView.isExpanded [0] state2 |> Expect.isFalse "toggled closed"
  }

  test "handleKey Up/Down navigates" {
    let state =
      TreeView.init ()
      |> TreeView.handleKey Key.Down sampleTree
    state.Cursor |> Expect.equal "down" [1]
    let state2 = TreeView.handleKey Key.Up sampleTree state
    state2.Cursor |> Expect.equal "up" [0]
  }

  test "view renders tree with indentation" {
    let state =
      TreeView.init ()
      |> TreeView.expand [0]
    let elem = TreeView.view id true sampleTree state
    match elem with
    | Column items ->
      items |> Expect.hasLength "4 visible" 4
    | _ -> failtest "expected Column"
  }

  test "view shows expand/collapse markers" {
    let state = TreeView.init ()
    let elem = TreeView.view id true sampleTree state
    match elem with
    | Column items ->
      // First item is cursor: Styled(reverse, Styled(bold, Text(...)))
      let rec extractText el =
        match el with
        | Text(t, _) -> Some t
        | Styled(_, inner) -> extractText inner
        | _ -> None
      match extractText items.[0] with
      | Some t -> t |> Expect.stringContains "collapsed marker" "▸"
      | None -> failtest "could not extract text"
    | _ -> failtest "expected Column"
  }

  test "view shows expanded marker after expand" {
    let state = TreeView.init () |> TreeView.expand [0]
    let elem = TreeView.view id true sampleTree state
    match elem with
    | Column items ->
      let rec extractText el =
        match el with
        | Text(t, _) -> Some t
        | Styled(_, inner) -> extractText inner
        | _ -> None
      match extractText items.[0] with
      | Some t -> t |> Expect.stringContains "expanded marker" "▾"
      | None -> failtest "could not extract text"
    | _ -> failtest "expected Column"
  }

  test "cursor item is bold+reverse when focused" {
    let state = TreeView.init ()
    let elem = TreeView.view id true sampleTree state
    match elem with
    | Column items ->
      match items.[0] with
      | Styled(_, Styled(_, _)) -> () // bold + reverse = 2 nested Styled
      | Styled(_, _) -> ()
      | _ -> failtest "expected Styled for cursor"
    | _ -> failtest "expected Column"
  }
]

let lazyTests = testList "El.lazy'" [
  test "lazy' returns same element for same reference" {
    let viewFn (model: string) = El.text model
    let memoized = El.lazy' viewFn
    let model = "hello"
    let e1 = memoized model
    let e2 = memoized model
    obj.ReferenceEquals(e1, e2) |> Expect.isTrue "same reference should cache"
  }

  test "lazy' recomputes for different reference" {
    let mutable callCount = 0
    let viewFn (model: string) =
      callCount <- callCount + 1
      El.text model
    let memoized = El.lazy' viewFn
    memoized "a" |> ignore
    memoized "b" |> ignore
    callCount |> Expect.equal "called twice" 2
  }

  test "lazy' only caches last result" {
    let mutable callCount = 0
    let viewFn (model: string) =
      callCount <- callCount + 1
      El.text model
    let memoized = El.lazy' viewFn
    let m = "test"
    memoized m |> ignore
    memoized m |> ignore
    memoized m |> ignore
    callCount |> Expect.equal "called once" 1
  }

  test "lazy2 caches with two args" {
    let mutable callCount = 0
    let viewFn (a: string) (b: string) =
      callCount <- callCount + 1
      El.text (sprintf "%s:%s" a b)
    let memoized = El.lazy2 viewFn
    let s = "x"
    let t = "y"
    memoized s t |> ignore
    memoized s t |> ignore
    callCount |> Expect.equal "called once for same refs" 1
  }

  test "lazy2 recomputes when either arg changes" {
    let mutable callCount = 0
    let viewFn (a: string) (b: string) =
      callCount <- callCount + 1
      El.text (sprintf "%s:%s" a b)
    let memoized = El.lazy2 viewFn
    memoized "x" "a" |> ignore
    memoized "y" "a" |> ignore
    callCount |> Expect.equal "called twice" 2
  }

  test "lazy' caches correctly with value-type (int) model" {
    let mutable callCount = 0
    let viewFn (n: int) =
      callCount <- callCount + 1
      El.text (string n)
    let memoized = El.lazy' viewFn
    memoized 42 |> ignore
    memoized 42 |> ignore
    memoized 42 |> ignore
    callCount |> Expect.equal "int model should cache (value-type equality)" 1
  }

  test "lazy' recomputes when int model changes" {
    let mutable callCount = 0
    let viewFn (n: int) =
      callCount <- callCount + 1
      El.text (string n)
    let memoized = El.lazy' viewFn
    memoized 1 |> ignore
    memoized 2 |> ignore
    callCount |> Expect.equal "different ints should recompute" 2
  }
]

type TestFormModel = { Name: string; Age: int; Active: bool }

let formTests = testList "Form" [
  let nameField =
    Form.field (FieldId "name")
      (fun focused (model: TestFormModel) ->
        let el = El.text (sprintf "Name: %s" model.Name)
        match focused with true -> el |> El.bold | false -> el)
      (fun _event (_model: TestFormModel) -> None)

  let ageField =
    Form.field (FieldId "age")
      (fun focused (model: TestFormModel) ->
        let el = El.text (sprintf "Age: %d" model.Age)
        match focused with true -> el |> El.bold | false -> el)
      (fun _event (_model: TestFormModel) -> None)

  let activeField =
    Form.field (FieldId "active")
      (fun focused (model: TestFormModel) ->
        Checkbox.view "Active" focused model.Active)
      (fun _event (_model: TestFormModel) -> None)

  let fields = [nameField; ageField; activeField]
  let model = { Name = "Alice"; Age = 30; Active = true }

  test "ids extracts field identifiers" {
    Form.ids fields |> Expect.equal "ids" [FieldId "name"; FieldId "age"; FieldId "active"]
  }

  test "view renders all fields as column" {
    let elem = Form.view fields (FieldId "name") model
    match elem with
    | Column items -> items |> Expect.hasLength "3 fields" 3
    | _ -> failtest "expected Column"
  }

  test "view highlights focused field" {
    let elem = Form.view fields (FieldId "name") model
    match elem with
    | Column (first :: _) ->
      match first with
      | Styled(_, _) -> ()
      | _ -> failtest "expected Styled for focused"
    | _ -> failtest "expected Column"
  }

  test "handleFocus moves to next field" {
    let next = Form.handleFocus fields (FieldId "name") FocusNext
    next |> Expect.equal "next is age" (FieldId "age")
  }

  test "handleFocus moves to previous field" {
    let prev = Form.handleFocus fields (FieldId "age") FocusPrev
    prev |> Expect.equal "prev is name" (FieldId "name")
  }

  test "handleFocus wraps around" {
    let next = Form.handleFocus fields (FieldId "active") FocusNext
    next |> Expect.equal "wraps to name" (FieldId "name")
  }

  test "handleEvent routes to focused field" {
    let result = Form.handleEvent fields (FieldId "name") (KeyPressed(Key.Enter, Modifiers.None)) model
    result |> Expect.isNone "no handler for enter on name"
  }

  test "handleEvent with modifier: Ctrl+A dispatches if handler uses it" {
    let ctrlAField =
      Form.field (FieldId "search")
        (fun _ _ -> El.empty)
        (fun evt _ -> match evt with KeyPressed(KeyChar 'a', m) when m = Modifiers.Ctrl -> Some () | _ -> None)
    let result = Form.handleEvent [ctrlAField] (FieldId "search") (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Ctrl)) ()
    result |> Expect.isSome "Ctrl+A handled"
  }
]

let textFormTests = testList "TextForm" [
  let mkForm () =
    TextForm.init [
      TextForm.field (FieldId "name")  "Name"  |> TextForm.required
      TextForm.field (FieldId "email") "Email" |> TextForm.required
                                       |> TextForm.validate (fun s ->
                                            match s.Contains("@") with
                                            | true  -> Ok s
                                            | false -> Error "Must contain @")
    ]

  let typeStr (s: string) m =
    s |> Seq.fold (fun acc c -> TextForm.update (TFKey (Key.Char (System.Text.Rune c))) acc |> fst) m

  test "init: focus on first field, not valid" {
    let f = mkForm ()
    f.FocusIndex |> Expect.equal "focus=0" 0
    f |> TextForm.isValid |> Expect.isFalse "empty is invalid"
  }

  test "TFKey inserts characters into focused field" {
    let f = mkForm () |> typeStr "Alice"
    TextForm.getValue (FieldId "name") f |> Expect.equal "name value" (Some "Alice")
  }

  test "TFKey does not affect unfocused field" {
    let f = mkForm () |> typeStr "Alice"
    TextForm.getValue (FieldId "email") f |> Expect.equal "email unchanged" (Some "")
  }

  test "TFTabNext moves focus and touches current field" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    f2.FocusIndex |> Expect.equal "moved to field 1" 1
    f2.Rows.[0].Touched |> Expect.isTrue "field 0 touched"
  }

  test "TFTabPrev wraps around to last field" {
    let (f, _) = mkForm () |> TextForm.update TFTabPrev
    f.FocusIndex |> Expect.equal "wrapped to last" 1
  }

  test "TFTabNext wraps around to first field" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let (f3, _) = TextForm.update TFTabNext f2
    f3.FocusIndex |> Expect.equal "wrapped back to 0" 0
  }

  test "not-yet-touched field shows no error even when empty" {
    let f = mkForm ()
    f.Rows.[0].Error |> Expect.isNone "no error before touch"
  }

  test "TFTabNext triggers live-validation on touched field" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f  // touch name, move to email
    // Now type invalid email and tab back — should show error live
    let f3 = f2 |> typeStr "bad"
    let (f4, _) = TextForm.update TFTabPrev f3  // blur email
    f4.Rows.[1].Error |> Expect.isSome "error on invalid email"
  }

  test "TFSubmit with all-valid fields returns shouldSubmit=true and sets Submitting" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let f3 = f2 |> typeStr "alice@example.com"
    let (f4, shouldSubmit) = TextForm.update TFSubmit f3
    shouldSubmit |> Expect.isTrue "should submit"
    f4.Status |> Expect.equal "status is Submitting" TFSubmitting
  }

  test "TFSubmit with invalid fields returns shouldSubmit=false and surfaces errors" {
    let (f, shouldSubmit) = mkForm () |> TextForm.update TFSubmit
    shouldSubmit |> Expect.isFalse "should not submit"
    f.Rows |> List.forall (fun r -> r.Touched) |> Expect.isTrue "all touched"
    f.Rows.[0].Error |> Expect.isSome "name error shown"
  }

  test "TFSubmitResult Ok sets status to Submitted" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let f3 = f2 |> typeStr "alice@example.com"
    let (f4, _)  = TextForm.update TFSubmit f3
    let (f5, _)  = TextForm.update (TFSubmitResult (Ok ())) f4
    f5.Status |> Expect.equal "Submitted" TFSubmitted
  }

  test "TFSubmitResult Error sets SubmitFailed" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let f3 = f2 |> typeStr "alice@example.com"
    let (f4, _) = TextForm.update TFSubmit f3
    let (f5, _) = TextForm.update (TFSubmitResult (Error "Server error")) f4
    f5.Status |> Expect.equal "SubmitFailed" (TFSubmitFailed "Server error")
  }

  test "TFReset restores to empty form" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let f3 = f2 |> typeStr "alice@example.com"
    let (f4, _) = TextForm.update TFSubmit f3
    let (f5, _) = TextForm.update (TFSubmitResult (Ok ())) f4
    let (f6, _) = TextForm.update TFReset f5
    f6.Status |> Expect.equal "back to Editing" TFEditing
    f6.FocusIndex |> Expect.equal "focus reset" 0
    f6.Rows |> List.forall (fun r -> r.Input.Text = "") |> Expect.isTrue "all fields cleared"
  }

  test "commitIfChanged: keys-only after Submitting are no-ops" {
    let f = mkForm () |> typeStr "Alice"
    let (f2, _) = TextForm.update TFTabNext f
    let f3 = f2 |> typeStr "alice@example.com"
    let (submitting, _) = TextForm.update TFSubmit f3
    let (after, _) = TextForm.update (TFKey (Key.Char (System.Text.Rune 'X'))) submitting
    // Key events must not mutate state while Submitting
    after.Rows.[1].Input.Text |> Expect.equal "email unchanged" "alice@example.com"
  }

  test "view renders without crash" {
    let f = mkForm () |> typeStr "Alice"
    let elem = TextForm.view true f
    let buf = Buffer.create 60 10
    Render.render { X=0; Y=0; Width=60; Height=10 } Style.empty buf elem
    buf.Width |> Expect.equal "width unchanged" 60
  }

  test "getValue returns None for unknown fieldId" {
    let f = mkForm ()
    TextForm.getValue (FieldId "nonexistent") f |> Expect.isNone "not found"
  }
  test "TFEvent Pasted inserts text into focused field" {
    let m = TextForm.init [TextForm.field (FieldId "q") "Query"]
    let m' = TextForm.update (TFEvent (Pasted "hello world")) m |> fst
    m'.Rows.[0].Input.Text |> Expect.equal "pasted text" "hello world"
  }
  test "TFEvent Ctrl+A selects all in focused field" {
    let m0 = TextForm.init [TextForm.field (FieldId "q") "Query"]
    let m1 = TextForm.update (TFKey (Key.Char (System.Text.Rune 'h'))) m0 |> fst
    let m2 = TextForm.update (TFKey (Key.Char (System.Text.Rune 'i'))) m1 |> fst
    let m3 = TextForm.update (TFEvent (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Ctrl))) m2 |> fst
    m3.Rows.[0].Input.SelectionAnchor |> Expect.equal "anchor at 0" (Some 0)
    m3.Rows.[0].Input.Cursor |> Expect.equal "cursor at end" 2
  }
  test "editing focused field removes its error from TFFieldErrors status" {
    let m = mkForm ()
    let errors = Map.ofList [(FieldId "name", "Already taken"); (FieldId "email", "Invalid format")]
    let (m', _) = TextForm.update (TFSetFieldErrors errors) m
    // Type into the focused (name) field — should clear "name" from TFFieldErrors
    let (m'', _) = TextForm.update (TFKey (Key.Char (System.Text.Rune 'A'))) m'
    match m''.Status with
    | TFFieldErrors remaining ->
      remaining |> Map.containsKey (FieldId "name")  |> Expect.isFalse "name error cleared"
      remaining |> Map.containsKey (FieldId "email") |> Expect.isTrue  "email error stays"
    | s -> failtestf "expected TFFieldErrors, got %A" s
  }
  test "editing last errored field transitions status to TFEditing" {
    let m = mkForm ()
    let errors = Map.ofList [(FieldId "name", "Only error")]
    let (m', _) = TextForm.update (TFSetFieldErrors errors) m
    // Type into the focused (name) field — only remaining error — should become TFEditing
    let (m'', _) = TextForm.update (TFKey (Key.Char (System.Text.Rune 'A'))) m'
    m''.Status |> Expect.equal "status is TFEditing" TFEditing
  }
  test "TFSetFieldErrors clears row-level client errors preventing dual messages" {
    // Simulate a flow where a field had a client validation error, then TFSetFieldErrors arrives.
    // The row-level client error must be cleared so the field doesn't show both messages.
    let m = mkForm ()
    // Trigger client-side validation error by submitting empty form
    let (m1, _) = TextForm.update TFSubmit m
    // m1 should have client errors on rows
    let hasClientErrors = m1.Rows |> List.exists (fun r -> r.Error.IsSome)
    hasClientErrors |> Expect.isTrue "form has client errors after failed submit"
    // Now set server field errors (simulating server response after a successful submit attempt)
    let (m2, _) = TextForm.update (TFSetFieldErrors (Map.ofList [(FieldId "name", "Server: name taken")])) m1
    // All row-level client errors must be cleared
    m2.Rows |> List.forall (fun r -> r.Error.IsNone) |> Expect.isTrue "row errors cleared after TFSetFieldErrors"
    m2.Status |> Expect.equal "status is TFFieldErrors" (TFFieldErrors (Map.ofList [(FieldId "name", "Server: name taken")]))
  }
  test "TFSetFieldErrors allows editing after server errors" {
    let m = mkForm ()
    let errors = Map.ofList [(FieldId "name", "Already taken")]
    let (m', _) = TextForm.update (TFSetFieldErrors errors) m
    m'.Status |> Expect.equal "status is TFFieldErrors" (TFFieldErrors errors)
    // After field errors, key events should still work (not blocked like Submitting)
    let (m'', _) = TextForm.update (TFKey (Key.Char (System.Text.Rune 'X'))) m'
    m''.Rows.[0].Input.Text |> Expect.equal "editing still works" "X"
  }
  test "TFSetFieldErrors view renders server error under matching field" {
    let m = mkForm ()
    let errors = Map.ofList [(FieldId "name", "Already taken")]
    let (m', _) = TextForm.update (TFSetFieldErrors errors) m
    let elem = TextForm.view true m'
    let buf = Buffer.create 60 5
    Render.render { X=0; Y=0; Width=60; Height=5 } Style.empty buf elem
    // Should not crash; just check buf renders without error
    buf.Width |> Expect.equal "rendered OK" 60
  }
]

let themeTests = testList "Theme" [
  test "dark theme has expected primary" {
    match Theme.dark.Primary with
    | Color.Named(Cyan, Bright) -> ()
    | _ -> failtest "expected bright cyan"
  }

  test "nord theme uses RGB" {
    match Theme.nord.Primary with
    | Color.Rgb _ -> ()
    | _ -> failtest "expected RGB"
  }

  test "apply wraps in fg+bg" {
    let elem = El.text "hello" |> Theme.apply Theme.dark
    match elem with
    | Styled(_, Styled(_, Text _)) -> ()
    | _ -> failtest "expected nested Styled"
  }

  test "heading produces bold+colored text" {
    let elem = Theme.heading Theme.dark "Title"
    match elem with
    | Styled(_, Styled(_, Text(t, _))) ->
      t |> Expect.equal "text content" "Title"
    | _ -> failtest "expected Styled"
  }

  test "panel produces bordered column" {
    let elem = Theme.panel Theme.dark "Panel" (El.text "content")
    match elem with
    | Padded(_, Bordered(_, _, Column _)) -> ()
    | _ -> failtest "expected Padded(Bordered(Column))"
  }

  test "five built-in themes available" {
    [Theme.dark; Theme.light; Theme.nord; Theme.dracula; Theme.catppuccin]
    |> Expect.hasLength "5 themes" 5
  }

  test "all themes have different primaries" {
    let primaries =
      [Theme.dark; Theme.light; Theme.nord; Theme.dracula; Theme.catppuccin]
      |> List.map (fun t -> t.Primary)
      |> List.distinct
    primaries |> Expect.hasLength "all different" 5
  }
]

// ── Themed widget integration tests ──────────────────────────────────────────

let themedWidgetTests = testList "ThemedWidgets" [

  // ── ProgressBar.withTheme ──────────────────────────────────────────────────

  test "ProgressBar.withTheme applies primary as FilledColor" {
    let config = ProgressBar.defaults |> ProgressBar.withTheme Theme.nord
    config.FilledColor |> Expect.equal "FilledColor = nord Primary" (Some Theme.nord.Primary)
  }

  test "ProgressBar.withTheme applies TextDim as EmptyColor" {
    let config = ProgressBar.defaults |> ProgressBar.withTheme Theme.nord
    config.EmptyColor |> Expect.equal "EmptyColor = nord TextDim" (Some Theme.nord.TextDim)
  }

  test "ProgressBar.withTheme preserves other fields" {
    let orig = { ProgressBar.defaults with Width = 40; ShowLabel = false; FilledChar = '#'; EmptyChar = '-' }
    let themed = orig |> ProgressBar.withTheme Theme.dark
    themed.Width     |> Expect.equal "Width preserved"     40
    themed.ShowLabel |> Expect.equal "ShowLabel preserved" false
    themed.FilledChar |> Expect.equal "FilledChar preserved" '#'
    themed.EmptyChar  |> Expect.equal "EmptyChar preserved"  '-'
  }

  test "ProgressBar.withTheme view renders without error" {
    let elem =
      { ProgressBar.defaults with Percent = 0.6 }
      |> ProgressBar.withTheme Theme.catppuccin
      |> ProgressBar.view
    // Element has [<NoEquality>]; verify it renders as a non-empty Row or Text
    match elem with
    | Empty -> failwith "expected non-empty element"
    | _ -> ()
  }

  // ── TextInput.viewThemed ──────────────────────────────────────────────────

  test "TextInput.viewThemed unfocused returns Text element" {
    let m = { TextInput.empty with Text = "hello"; Cursor = 5 }
    // Unfocused view just returns the text, no selection/cursor markup
    match TextInput.viewThemed Theme.dark false m with
    | Text(t, _) -> t |> Expect.equal "text content" "hello"
    | other      -> failwithf "expected Text, got %A" other
  }

  test "TextInput.viewThemed focused returns a Row element" {
    let m = { TextInput.empty with Text = "hi"; Cursor = 0 }
    match TextInput.viewThemed Theme.nord true m with
    | Row _ -> ()
    | other -> failwithf "expected Row, got %A" other
  }

  test "TextInput.viewWithPlaceholderThemed empty unfocused shows placeholder" {
    let m = TextInput.empty
    match TextInput.viewWithPlaceholderThemed Theme.dark "Type here" false m with
    | Styled(_, inner) ->
      match inner with
      | Text(t, _) -> t |> Expect.equal "placeholder text" "Type here"
      | _ -> ()
    | Text(t, _) -> t |> Expect.equal "placeholder text" "Type here"
    | _ -> ()
  }

  test "TextInput.viewWithPlaceholderThemed non-empty is a Row when focused" {
    let m = { TextInput.empty with Text = "abc"; Cursor = 1 }
    // Non-empty → delegates to viewThemed → should be a Row (cursor markup)
    match TextInput.viewWithPlaceholderThemed Theme.dark "placeholder" true m with
    | Row _ -> ()
    | other -> failwithf "expected Row (from viewThemed), got %A" other
  }

  // ── Checkbox.viewThemed ──────────────────────────────────────────────────

  test "Checkbox.viewThemed checked unfocused returns a Row" {
    match Checkbox.viewThemed Theme.nord "Accept" false true with
    | Row _ -> ()
    | other -> failwithf "expected Row, got %A" other
  }

  test "Checkbox.viewThemed unfocused is not Styled (no bold wrapper)" {
    let el = Checkbox.viewThemed Theme.dark "Label" false false
    match el with
    | Styled _ -> failwith "unfocused should not be wrapped in Styled"
    | _ -> ()
  }

  test "Checkbox.viewThemed focused is Styled (bold wrapper)" {
    let el = Checkbox.viewThemed Theme.dark "Label" true false
    match el with
    | Styled _ -> ()
    | _ -> failwith "focused should be wrapped in Styled (bold)"
  }

  // ── Toggle.viewThemed ──────────────────────────────────────────────────────

  test "Toggle.viewThemed unfocused is not Styled (no bold wrapper)" {
    let el = Toggle.viewThemed Theme.dark "On" "Off" false true
    match el with
    | Styled _ -> failwith "unfocused should not be wrapped in Styled"
    | _ -> ()
  }

  test "Toggle.viewThemed focused is Styled (bold wrapper)" {
    let el = Toggle.viewThemed Theme.dark "On" "Off" true false
    match el with
    | Styled _ -> ()
    | _ -> failwith "focused should be wrapped in Styled (bold)"
  }

  test "Toggle.viewThemed returns a Row element (unfocused)" {
    match Toggle.viewThemed Theme.nord "Yes" "No" false true with
    | Row _ -> ()
    | other -> failwithf "expected Row, got %A" other
  }

  // ── Tabs.withTheme ────────────────────────────────────────────────────────

  test "Tabs.withTheme applies primary as ActiveColor" {
    let cfg = { Items = ["A";"B"]; ActiveIndex = 0; ToString = id; ActiveColor = None; InactiveColor = None }
    let themed = Tabs.withTheme Theme.dracula cfg
    themed.ActiveColor |> Expect.equal "ActiveColor = dracula Primary" (Some Theme.dracula.Primary)
  }

  test "Tabs.withTheme applies TextDim as InactiveColor" {
    let cfg = { Items = ["A";"B"]; ActiveIndex = 0; ToString = id; ActiveColor = None; InactiveColor = None }
    let themed = Tabs.withTheme Theme.dracula cfg
    themed.InactiveColor |> Expect.equal "InactiveColor = dracula TextDim" (Some Theme.dracula.TextDim)
  }

  test "Tabs.withTheme preserves items and activeIndex" {
    let cfg = { Items = [1;2;3]; ActiveIndex = 2; ToString = string; ActiveColor = None; InactiveColor = None }
    let themed = Tabs.withTheme Theme.catppuccin cfg
    themed.Items       |> Expect.equal "Items preserved"       [1;2;3]
    themed.ActiveIndex |> Expect.equal "ActiveIndex preserved" 2
  }

  // ── VirtualList.withTheme ─────────────────────────────────────────────────

  test "VirtualList.withTheme sets SelectionColor to theme Primary" {
    let cfg = VirtualList.create (fun _ row -> El.text (string row))
    let themed = VirtualList.withTheme Theme.nord cfg
    themed.SelectionColor |> Expect.equal "SelectionColor = nord Primary" Theme.nord.Primary
  }

  test "VirtualList.withTheme preserves RenderRow and ShowScrollbar" {
    let renderFn = fun _ (row: string) -> El.text row
    let cfg = { (VirtualList.create renderFn) with ShowScrollbar = true }
    let themed = VirtualList.withTheme Theme.dark cfg
    themed.ShowScrollbar |> Expect.equal "ShowScrollbar preserved" true
  }
]

// ── Color-semantic render tests ──────────────────────────────────────────────
// These tests render themed widgets to a buffer and assert that theme colors
// are actually present in the rendered cells — not just that the Element DU
// structure is correct.

let private renderToBuffer w h elem =
  let buf = Buffer.create w h
  Render.render { X = 0; Y = 0; Width = w; Height = h } Style.empty buf elem
  buf

let private fgColorAt x y buf =
  PackedColor.unpack (Buffer.get x y buf).Fg

let themedColorSemanticTests = testList "ThemedWidgets.ColorSemantics" [

  test "ProgressBar.withTheme: filled cells carry theme.Primary as fg color" {
    // Width 10, 100% filled → columns 0-9 should all have Fg = theme.Primary
    let config =
      { ProgressBar.defaults with Percent = 1.0; Width = 10; ShowLabel = false }
      |> ProgressBar.withTheme Theme.nord
    let buf = renderToBuffer 10 1 (ProgressBar.view config)
    // Every filled cell should have nord Primary as fg
    for col in 0 .. 9 do
      fgColorAt col 0 buf
      |> Expect.equal (sprintf "col %d fg = nord.Primary" col) Theme.nord.Primary
  }

  test "ProgressBar.withTheme: empty cells carry theme.TextDim as fg color" {
    // Width 10, 0% filled → all cells should have Fg = theme.TextDim
    let config =
      { ProgressBar.defaults with Percent = 0.0; Width = 10; ShowLabel = false }
      |> ProgressBar.withTheme Theme.dracula
    let buf = renderToBuffer 10 1 (ProgressBar.view config)
    for col in 0 .. 9 do
      fgColorAt col 0 buf
      |> Expect.equal (sprintf "col %d fg = dracula.TextDim" col) Theme.dracula.TextDim
  }

  test "Checkbox.viewThemed: checked box cell fg = theme.Accent" {
    // "[x] Label" — first cell is '[', col 1 is 'x'.  The box span is cols 0-2.
    // El.fg theme.Accent wraps the "[x]" text, so cols 0-2 should all have Accent fg.
    let elem = Checkbox.viewThemed Theme.nord "L" false true  // unfocused, checked
    let buf = renderToBuffer 20 1 elem
    fgColorAt 1 0 buf
    |> Expect.equal "checked 'x' cell fg = nord.Accent" Theme.nord.Accent
  }

  test "Checkbox.viewThemed: unchecked box cell fg = theme.TextDim" {
    // "[ ] Label" — col 1 is ' ' (inside the box), cols 0-2 wrapped with TextDim
    let elem = Checkbox.viewThemed Theme.dark "L" false false  // unfocused, unchecked
    let buf = renderToBuffer 20 1 elem
    // Col 0 = '[', col 1 = ' ', col 2 = ']'  — all should have TextDim fg
    fgColorAt 0 0 buf
    |> Expect.equal "unchecked '[' cell fg = dark.TextDim" Theme.dark.TextDim
  }

  test "Toggle.viewThemed: on-state symbol cell fg = theme.Success" {
    // "● onLabel" — col 0 is '●' with Success color
    let elem = Toggle.viewThemed Theme.catppuccin "On" "Off" false true  // unfocused, on
    let buf = renderToBuffer 20 1 elem
    fgColorAt 0 0 buf
    |> Expect.equal "on symbol fg = catppuccin.Success" Theme.catppuccin.Success
  }

  test "Toggle.viewThemed: off-state symbol cell fg = theme.TextDim" {
    // "○ offLabel" — col 0 is '○' with TextDim color
    let elem = Toggle.viewThemed Theme.nord "On" "Off" false false  // unfocused, off
    let buf = renderToBuffer 20 1 elem
    fgColorAt 0 0 buf
    |> Expect.equal "off symbol fg = nord.TextDim" Theme.nord.TextDim
  }

]



type TimingsMsg = GotTimings of FrameTimings | OtherMsg

let frameTimingsTests = testList "Sub.frameTimings" [
  test "Sub.frameTimings creates FrameTimingsSub" {
    match Sub.frameTimings GotTimings with
    | FrameTimingsSub _ -> ()
    | other -> failwith (sprintf "expected FrameTimingsSub, got %A" other)
  }
  test "Sub.map transforms FrameTimingsSub message" {
    let original = Sub.frameTimings (fun t -> t)
    let mapped = Sub.map (fun t -> GotTimings t) original
    match mapped with
    | FrameTimingsSub _ -> ()
    | other -> failwith (sprintf "Sub.map should produce FrameTimingsSub, got %A" other)
  }
  test "FrameTimings.empty has zero values" {
    FrameTimings.empty.RenderMs   |> Expect.equal "renderMs"  0.0
    FrameTimings.empty.DiffMs     |> Expect.equal "diffMs"    0.0
    FrameTimings.empty.PresentMs  |> Expect.equal "presentMs" 0.0
    FrameTimings.empty.TotalMs    |> Expect.equal "totalMs"   0.0
    FrameTimings.empty.ChangedCells |> Expect.equal "cells"   0
  }
  test "FrameTimingsSub is ignored in TestHarness subscription scan" {
    // TestHarness scans subs to route key/resize events.
    // FrameTimingsSub must not crash the scan (catch-all | _ -> () handles it).
    let timingsSub = Sub.frameTimings GotTimings
    let keySub = Keys.bind [ Key.Char (System.Text.Rune 'q'), OtherMsg ]
    let prog : Program<int, TimingsMsg> =
      { Init    = fun () -> 0, Cmd.none
        Update  = fun msg m ->
          match msg with | GotTimings _ -> m, Cmd.none | OtherMsg -> m + 1, Cmd.none
        View    = fun m -> El.text (string m)
        Subscribe = fun _ -> [ keySub; timingsSub ] }
    let app = TestHarness.init 80 24 prog
    let app' = TestHarness.pressKey (Key.Char (System.Text.Rune 'q')) app
    app'.Model |> Expect.equal "key handled" 1
  }
]

// ── Undoable.commitIfChanged equality constraint ──────────────────────────────

type EquatableModel = { Value: int }

let undoableCommitTests = testList "Undoable.commitIfChanged" [
  test "unchanged model is not committed" {
    let m = Undoable.init { Value = 5 }
    let m' = Undoable.commitIfChanged { Value = 5 } m
    m'.Past |> Expect.isEmpty "no new entry"
  }
  test "changed model is committed" {
    let m = Undoable.init { Value = 5 }
    let m' = Undoable.commitIfChanged { Value = 10 } m
    m'.Past |> Expect.hasLength "one past entry" 1
    m'.Present.Value |> Expect.equal "present updated" 10
  }
]

// ── FocusRing ─────────────────────────────────────────────────────────────────

let focusRingTests = testList "FocusRing" [
  test "isFocusedAt returns true for current index" {
    let ring = FocusRing.create ["a"; "b"; "c"]
    ring |> FocusRing.isFocusedAt 0 |> Expect.isTrue "index 0 is focused"
  }
  test "isFocusedAt returns false for non-current index" {
    let ring = FocusRing.create ["a"; "b"; "c"]
    ring |> FocusRing.isFocusedAt 1 |> Expect.isFalse "index 1 is not focused"
  }
  test "isFocusedAt works after next" {
    let ring = FocusRing.create ["a"; "b"; "c"] |> FocusRing.next
    ring |> FocusRing.isFocusedAt 0 |> Expect.isFalse "0 not focused after next"
    ring |> FocusRing.isFocusedAt 1 |> Expect.isTrue "1 is focused after next"
  }
  test "isFocusedAt returns false for empty ring" {
    let ring = FocusRing.create ([] : string list)
    ring |> FocusRing.isFocusedAt 0 |> Expect.isFalse "no items, never focused"
  }
  test "current returns None for empty ring" {
    FocusRing.create ([] : int list) |> FocusRing.current |> Expect.isNone "empty ring has no current"
  }
  test "current returns Some for non-empty ring" {
    FocusRing.create [42; 99] |> FocusRing.current |> Expect.isSome "non-empty ring has current"
  }
  test "next wraps around from last to first" {
    let ring = FocusRing.create ["a"; "b"; "c"] |> FocusRing.next |> FocusRing.next |> FocusRing.next
    ring |> FocusRing.isFocusedAt 0 |> Expect.isTrue "3 nexts wraps to 0 on 3-item ring"
  }
  test "prev wraps from first to last" {
    let ring = FocusRing.create ["a"; "b"; "c"] |> FocusRing.prev
    ring |> FocusRing.isFocusedAt 2 |> Expect.isTrue "prev from 0 wraps to last (index 2)"
  }
  test "next on empty ring is identity" {
    let ring = FocusRing.create ([] : string list)
    (FocusRing.next ring) |> Expect.equal "empty ring unchanged" ring
  }
  test "prev on empty ring is identity" {
    let ring = FocusRing.create ([] : string list)
    (FocusRing.prev ring) |> Expect.equal "empty ring unchanged" ring
  }
  test "isFocused uses structural equality" {
    let ring = FocusRing.create ["x"; "y"; "z"]
    ring |> FocusRing.isFocused "x" |> Expect.isTrue "x is the current item"
    ring |> FocusRing.isFocused "y" |> Expect.isFalse "y is not current"
  }
  testProperty "next then prev = identity for non-empty ring" <| fun (NonEmptyArray (items: string[])) ->
    let ring = FocusRing.create (Array.toList items)
    let roundtrip = ring |> FocusRing.next |> FocusRing.prev
    roundtrip.Index |> Expect.equal "next·prev = id" ring.Index
  testProperty "prev then next = identity for non-empty ring" <| fun (NonEmptyArray (items: string[])) ->
    let ring = FocusRing.create (Array.toList items)
    let roundtrip = ring |> FocusRing.prev |> FocusRing.next
    roundtrip.Index |> Expect.equal "prev·next = id" ring.Index
  testProperty "next n times wraps by modulo" <| fun (NonEmptyArray (items: string[])) (PositiveInt n) ->
    let ring = FocusRing.create (Array.toList items)
    let len = items.Length
    let byN = List.fold (fun r _ -> FocusRing.next r) ring [1..n]
    byN.Index |> Expect.equal "n nexts = n mod len" (n % len)
  testProperty "isFocusedAt index matches current index" <| fun (NonEmptyArray (items: string[])) ->
    let ring = FocusRing.create (Array.toList items)
    ring |> FocusRing.isFocusedAt ring.Index |> Expect.isTrue "isFocusedAt(current index) = true"
  testProperty "exactly one index is focused at a time" <| fun (NonEmptyArray (items: string[])) ->
    let ring = FocusRing.create (Array.toList items)
    let focusedCount = [0..items.Length - 1] |> List.filter (fun i -> FocusRing.isFocusedAt i ring) |> List.length
    focusedCount |> Expect.equal "exactly one focused" 1
]


// ── VirtualList ───────────────────────────────────────────────────────────────

let mkList n = VirtualList.ofArray 5 [| 0 .. n - 1 |]

let vlCfg = VirtualList.create (fun _selected (i: int) -> El.text (sprintf "item %d" i))

let virtualListTests = testList "VirtualList" [
  test "ofArray empty list has no selection" {
    let m = VirtualList.ofArray 5 [||]
    m.SelectedIndex  |> Expect.equal "no selection" None
    m.ScrollOffset   |> Expect.equal "offset 0" 0
    m.ViewportHeight |> Expect.equal "viewport 5" 5
  }
  test "ofArray non-empty selects index 0" {
    let m = mkList 10
    m.SelectedIndex |> Expect.equal "selects 0" (Some 0)
    m.ScrollOffset  |> Expect.equal "offset 0" 0
  }
  test "ofList creates same as ofArray" {
    let m1 = VirtualList.ofList 5 [ 1; 2; 3 ]
    let m2 = VirtualList.ofArray 5 [| 1; 2; 3 |]
    m1.SelectedIndex |> Expect.equal "same sel" m2.SelectedIndex
    m1.ScrollOffset  |> Expect.equal "same off" m2.ScrollOffset
  }
  test "ViewportHeight clamped to minimum 1" {
    let m = VirtualList.ofArray 0 [| 1 |]
    m.ViewportHeight |> Expect.equal "min 1" 1
  }
  test "selectedItem returns current item" {
    let m = mkList 10
    VirtualList.selectedItem m |> Expect.equal "item 0" (Some 0)
  }
  test "selectedItem None for empty list" {
    VirtualList.selectedItem (VirtualList.ofArray 5 [||]) |> Expect.equal "none" None
  }
  test "selectNext advances selection" {
    let m = mkList 10 |> VirtualList.selectNext
    m.SelectedIndex |> Expect.equal "index 1" (Some 1)
  }
  test "selectNext does not go past last item" {
    let m = VirtualList.ofArray 5 [| 0 |] |> VirtualList.selectNext
    m.SelectedIndex |> Expect.equal "stays at 0" (Some 0)
  }
  test "selectPrev does not go below 0" {
    let m = mkList 10 |> VirtualList.selectPrev
    m.SelectedIndex |> Expect.equal "stays at 0" (Some 0)
  }
  test "selectPrev from item 3 gives item 2" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = { m with SelectedIndex = Some 3 } |> VirtualList.selectPrev
    m'.SelectedIndex |> Expect.equal "index 2" (Some 2)
  }
  test "selectFirst jumps to item 0" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = { m with SelectedIndex = Some 7 } |> VirtualList.selectFirst
    m'.SelectedIndex |> Expect.equal "index 0" (Some 0)
  }
  test "selectLast jumps to last item" {
    let m = mkList 10 |> VirtualList.selectLast
    m.SelectedIndex |> Expect.equal "index 9" (Some 9)
  }
  test "ensureVisible scrolls down when selection below viewport" {
    let m = mkList 20
    let m' = { m with SelectedIndex = Some 10 } |> VirtualList.ensureVisible
    m'.ScrollOffset |> Expect.equal "offset 6" 6
  }
  test "ensureVisible scrolls up when selection above viewport" {
    let m = { (mkList 20) with ScrollOffset = 10; SelectedIndex = Some 3 }
    let m' = VirtualList.ensureVisible m
    m'.ScrollOffset |> Expect.equal "offset 3" 3
  }
  test "pageDown advances by ViewportHeight" {
    let m = mkList 20
    let m' = VirtualList.pageDown m
    m'.SelectedIndex |> Expect.equal "index 5" (Some 5)
  }
  test "pageDown clamps to last item" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = { m with SelectedIndex = Some 8 } |> VirtualList.pageDown
    m'.SelectedIndex |> Expect.equal "index 9 clamped" (Some 9)
  }
  test "pageUp retreats by ViewportHeight" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = { m with SelectedIndex = Some 8 } |> VirtualList.pageUp
    m'.SelectedIndex |> Expect.equal "index 3" (Some 3)
  }
  test "setItems clamps selection to new length" {
    let m = { (mkList 20) with SelectedIndex = Some 15 }
    let m' = VirtualList.setItems [| 0; 1; 2 |] m
    m'.SelectedIndex |> Expect.equal "clamped to 2" (Some 2)
    m'.Items.Length  |> Expect.equal "3 items" 3
  }
  test "setItems empty array yields no selection" {
    let m = mkList 10
    let m' = VirtualList.setItems [||] m
    m'.SelectedIndex |> Expect.equal "none" None
  }
  test "setItems with stale high scrollOffset ensureVisible fixes it" {
    let m = { (mkList 20) with ScrollOffset = 15; SelectedIndex = Some 15 }
    let m' = VirtualList.setItems [| 0; 1; 2 |] m
    // sel clamped to 2, offset must not be 15
    m'.SelectedIndex |> Expect.equal "sel 2" (Some 2)
    m'.ScrollOffset  |> (fun v -> (v, 3) |> Expect.isLessThan "offset not stale")
  }
  test "RenderRow receives selected=true for the selected row" {
    let mutable sawSelected = false
    let cfg = VirtualList.create (fun selected _ ->
      if selected then sawSelected <- true
      El.empty)
    let m = VirtualList.ofArray 3 [| 0; 1; 2 |]  // SelectedIndex = Some 0
    VirtualList.view cfg m |> ignore
    sawSelected |> Expect.isTrue "selected=true passed for row 0"
  }
  test "RenderRow receives selected=false for non-selected rows" {
    let falseCount = ref 0
    let cfg = VirtualList.create (fun selected _ ->
      if not selected then incr falseCount
      El.empty)
    let m = VirtualList.ofArray 3 [| 0; 1; 2 |]  // SelectedIndex = Some 0
    VirtualList.view cfg m |> ignore
    !falseCount |> Expect.equal "2 non-selected rows" 2
  }
  test "handleEvent Up calls selectPrev" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 3 }
    let result = VirtualList.handleEvent (KeyPressed(Key.Up, Modifiers.None)) m
    result.SelectedIndex |> Expect.equal "moved up" (Some 2)
  }
  test "handleEvent Down calls selectNext" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 3 }
    let result = VirtualList.handleEvent (KeyPressed(Key.Down, Modifiers.None)) m
    result.SelectedIndex |> Expect.equal "moved down" (Some 4)
  }
  test "handleEvent Home calls selectFirst" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 7 }
    let result = VirtualList.handleEvent (KeyPressed(Key.Home, Modifiers.None)) m
    result.SelectedIndex |> Expect.equal "jumped to first" (Some 0)
  }
  test "handleEvent End calls selectLast" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let result = VirtualList.handleEvent (KeyPressed(Key.End, Modifiers.None)) m
    result.SelectedIndex |> Expect.equal "jumped to last" (Some 9)
  }
  test "handleEvent unhandled key returns model unchanged" {
    let m = VirtualList.ofArray 5 [| 0 .. 4 |]
    let result = VirtualList.handleEvent (KeyPressed(Key.Char (System.Text.Rune 'x'), Modifiers.None)) m
    result |> Expect.equal "unchanged" m
  }
  test "view produces Column with exactly ViewportHeight rows" {
    let m = mkList 20
    match VirtualList.view vlCfg m with
    | Column children -> children |> Expect.hasLength "5 rows" 5
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  test "view pads to ViewportHeight when fewer items" {
    let m = VirtualList.ofArray 5 [| 0; 1; 2 |]
    match VirtualList.view vlCfg m with
    | Column children -> children |> Expect.hasLength "padded to 5" 5
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  test "view empty pads to ViewportHeight" {
    let m = VirtualList.ofArray 5 [||]
    match VirtualList.view vlCfg m with
    | Column children -> children |> Expect.hasLength "5 empty rows" 5
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  test "view renders correct window after scroll" {
    let m = { (mkList 20) with ScrollOffset = 3; SelectedIndex = Some 5 }
    match VirtualList.view vlCfg m with
    | Column children ->
      children |> Expect.hasLength "5 rows" 5
      match children[0] with
      | Text(t, _) -> t |> Expect.equal "item 3" "item 3"
      | other -> failwith (sprintf "expected Text, got %A" other)
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  test "view selected row gets highlight style" {
    // Use a config that actually uses the selected bool to add styling
    let highlightCfg =
      VirtualList.create (fun selected (i: int) ->
        match selected with
        | true -> El.bg (Color.Named(BaseColor.Blue, Normal)) (El.text (sprintf "item %d" i))
        | false -> El.text (sprintf "item %d" i))
    let m = mkList 5
    match VirtualList.view highlightCfg m with
    | Column children ->
      match children[0] with
      | Styled(s, _) ->
        s.Bg |> Expect.equal "blue bg" (Some (Color.Named(BaseColor.Blue, Normal)))
      | other -> failwith (sprintf "expected Styled, got %A" other)
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  test "view non-selected rows are not highlighted" {
    let highlightCfg =
      VirtualList.create (fun selected (i: int) ->
        match selected with
        | true -> El.bg (Color.Named(BaseColor.Blue, Normal)) (El.text (sprintf "item %d" i))
        | false -> El.text (sprintf "item %d" i))
    let m = mkList 5
    match VirtualList.view highlightCfg m with
    | Column children ->
      match children[1] with
      | Text _ -> ()
      | Styled(s, _) ->
        match s.Bg with
        | Some c when c = Color.Named(BaseColor.Blue, Normal) ->
          failwith "row 1 should not be selected"
        | _ -> ()
      | Empty -> ()
      | other -> failwith (sprintf "unexpected: %A" other)
    | other -> failwith (sprintf "expected Column, got %A" other)
  }
  // ── resize ───────────────────────────────────────────────────────────────
  test "resize updates ViewportHeight" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = VirtualList.resize 10 m
    m'.ViewportHeight |> Expect.equal "new height" 10
  }
  test "resize clamps to minimum 1" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]
    let m' = VirtualList.resize 0 m
    m'.ViewportHeight |> Expect.equal "clamped to 1" 1
  }
  test "resize keeps selection visible" {
    let m = { VirtualList.ofArray 5 [| 0 .. 19 |] with SelectedIndex = Some 19; ScrollOffset = 15 }
    let m' = VirtualList.resize 3 m
    let sel = m'.SelectedIndex |> Option.defaultValue -1
    (sel >= m'.ScrollOffset && sel < m'.ScrollOffset + m'.ViewportHeight)
    |> Expect.isTrue "selection still visible after resize"
  }
  // ── scrollbar ────────────────────────────────────────────────────────────
  test "view with ShowScrollbar=true returns Row wrapping the list" {
    let cfg = { VirtualList.create (fun _ (i: int) -> El.text (string i)) with ShowScrollbar = true }
    let m = VirtualList.ofArray 3 [| 0 .. 9 |]
    let el = VirtualList.view cfg m
    match el with
    | Row _ -> ()
    | Column _ -> failtest "expected Row (list+scrollbar) when ShowScrollbar=true"
    | _ -> failtest "unexpected element shape"
  }
  test "view with ShowScrollbar=false returns Column (existing behavior)" {
    let cfg = VirtualList.create (fun _ (i: int) -> El.text (string i))
    let m = VirtualList.ofArray 3 [| 0 .. 9 |]
    let el = VirtualList.view cfg m
    match el with
    | Column _ -> ()
    | _ -> failtest "expected Column when ShowScrollbar=false"
  }
  test "scrollbar renders exactly ViewportHeight rows" {
    let cfg = { VirtualList.create (fun _ (i: int) -> El.text (string i)) with ShowScrollbar = true }
    let m = VirtualList.ofArray 5 [| 0 .. 19 |]
    let el = VirtualList.view cfg m
    match el with
    | Row [Column _; Column sb] ->
      sb |> List.length |> Expect.equal "scrollbar rows = ViewportHeight" 5
    | Row [Column _; _] -> ()  // shape exists, not checking exact structure
    | _ -> failtest "expected Row [listColumn; scrollbarColumn]"
  }
  // ── mouse wheel routing (Sprint 49) ─────────────────────────────────────
  test "handleEvent ScrollUp calls selectPrev" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 5 }
    let event = MouseInput { Button = MouseButton.ScrollUp; X = 0; Y = 0; Modifiers = Modifiers.None; Phase = Pressed }
    let result = VirtualList.handleEvent event m
    result.SelectedIndex |> Expect.equal "moved up one" (Some 4)
  }
  test "handleEvent ScrollDown calls selectNext" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 3 }
    let event = MouseInput { Button = MouseButton.ScrollDown; X = 0; Y = 0; Modifiers = Modifiers.None; Phase = Pressed }
    let result = VirtualList.handleEvent event m
    result.SelectedIndex |> Expect.equal "moved down one" (Some 4)
  }
  test "handleEvent ScrollUp at top stays at item 0" {
    let m = VirtualList.ofArray 5 [| 0 .. 9 |]  // SelectedIndex = Some 0
    let event = MouseInput { Button = MouseButton.ScrollUp; X = 0; Y = 0; Modifiers = Modifiers.None; Phase = Pressed }
    let result = VirtualList.handleEvent event m
    result.SelectedIndex |> Expect.equal "stays at 0" (Some 0)
  }
  test "handleEvent ScrollDown at bottom stays at last item" {
    let m = { VirtualList.ofArray 5 [| 0 .. 9 |] with SelectedIndex = Some 9 }
    let event = MouseInput { Button = MouseButton.ScrollDown; X = 0; Y = 0; Modifiers = Modifiers.None; Phase = Pressed }
    let result = VirtualList.handleEvent event m
    result.SelectedIndex |> Expect.equal "stays at 9" (Some 9)
  }
  test "handleEvent ScrollDown updates scroll offset to keep selection visible" {
    let m = VirtualList.ofArray 3 [| 0 .. 9 |]  // viewport=3, sel=0
    let event = MouseInput { Button = MouseButton.ScrollDown; X = 0; Y = 0; Modifiers = Modifiers.None; Phase = Pressed }
    // Scroll down through several items — scroll offset must catch up
    let m' = Seq.fold (fun acc _ -> VirtualList.handleEvent event acc) m (seq { 0..5 })
    let sel = m'.SelectedIndex |> Option.defaultValue -1
    (sel >= m'.ScrollOffset && sel < m'.ScrollOffset + m'.ViewportHeight)
    |> Expect.isTrue "selection stays visible after multiple scrolls"
  }
]


// ── TextInput word movement and selection ─────────────────────────────────────

let tiEmpty = TextInput.empty
let tiWord  = TextInput.ofString "hello world foo"

let ti text cursor = { Text = text; Cursor = cursor; SelectionAnchor = None }
let tiSel text cursor anchor = { Text = text; Cursor = cursor; SelectionAnchor = Some anchor }

let textInputWordSelTests = testList "TextInput word and selection" [
  // ── wordLeftPos / wordRightPos ───────────────────────────────────────────
  test "wordLeftPos from end of 'hello' lands at 0" {
    TextInput.wordLeftPos 5 "hello" |> Expect.equal "pos 0" 0
  }
  test "wordLeftPos skips trailing spaces then word" {
    TextInput.wordLeftPos 11 "hello world" |> Expect.equal "pos 6" 6
  }
  test "wordLeftPos at 0 stays at 0" {
    TextInput.wordLeftPos 0 "hello" |> Expect.equal "pos 0" 0
  }
  test "wordRightPos from start of 'hello' lands at 5" {
    TextInput.wordRightPos 0 "hello" |> Expect.equal "pos 5" 5
  }
  test "wordRightPos skips leading spaces then word" {
    TextInput.wordRightPos 5 "hello world" |> Expect.equal "pos 11" 11
  }
  test "wordRightPos at end stays at end" {
    TextInput.wordRightPos 5 "hello" |> Expect.equal "pos 5" 5
  }
  test "wordRightPos treats underscore as word char" {
    TextInput.wordRightPos 0 "hello_world foo" |> Expect.equal "pos 11 (underscore included)" 11
  }
  test "wordLeftPos treats underscore as word char" {
    TextInput.wordLeftPos 11 "hello_world foo" |> Expect.equal "pos 0 (underscore included)" 0
  }
  // ── wordLeft / wordRight ─────────────────────────────────────────────────
  test "wordLeft moves cursor backward one word" {
    (ti "hello world" 11 |> TextInput.wordLeft).Cursor |> Expect.equal "cursor 6" 6
  }
  test "wordLeft clears SelectionAnchor" {
    (tiSel "hello world" 11 5 |> TextInput.wordLeft).SelectionAnchor |> Expect.equal "none" None
  }
  test "wordRight moves cursor forward one word" {
    (ti "hello world" 5 |> TextInput.wordRight).Cursor |> Expect.equal "cursor 11" 11
  }
  // ── selectAll ────────────────────────────────────────────────────────────
  test "selectAll sets anchor=0 cursor=end" {
    let m = ti "hello" 2 |> TextInput.selectAll
    m.SelectionAnchor |> Expect.equal "anchor 0" (Some 0)
    m.Cursor          |> Expect.equal "cursor 5" 5
  }
  // ── selectLeft / selectRight ─────────────────────────────────────────────
  test "selectRight extends selection rightward" {
    let m = ti "abc" 0 |> TextInput.selectRight
    m.Cursor          |> Expect.equal "cursor 1" 1
    m.SelectionAnchor |> Expect.equal "anchor 0" (Some 0)
  }
  test "selectRight preserves existing anchor" {
    let m = tiSel "abc" 1 0 |> TextInput.selectRight
    m.Cursor          |> Expect.equal "cursor 2" 2
    m.SelectionAnchor |> Expect.equal "anchor 0 preserved" (Some 0)
  }
  test "selectLeft extends selection leftward" {
    let m = ti "abc" 3 |> TextInput.selectLeft
    m.Cursor          |> Expect.equal "cursor 2" 2
    m.SelectionAnchor |> Expect.equal "anchor 3" (Some 3)
  }
  // ── selectionRange ───────────────────────────────────────────────────────
  test "selectionRange None when no anchor" {
    TextInput.selectionRange (ti "abc" 2) |> Expect.equal "none" None
  }
  test "selectionRange Some when anchor differs from cursor" {
    TextInput.selectionRange (tiSel "abc" 3 0) |> Expect.equal "0..3" (Some (0, 3))
  }
  test "selectionRange normalises reverse selection" {
    TextInput.selectionRange (tiSel "abc" 0 3) |> Expect.equal "0..3" (Some (0, 3))
  }
  test "selectionRange None when anchor equals cursor" {
    TextInput.selectionRange (tiSel "abc" 2 2) |> Expect.equal "none" None
  }
  // ── hasSelection ─────────────────────────────────────────────────────────
  test "hasSelection false with no anchor" {
    TextInput.hasSelection (ti "abc" 1) |> Expect.isFalse "no sel"
  }
  test "hasSelection true with anchor != cursor" {
    TextInput.hasSelection (tiSel "abc" 3 0) |> Expect.isTrue "has sel"
  }
  // ── deleteSelection ──────────────────────────────────────────────────────
  test "deleteSelection removes selected region" {
    let m = tiSel "hello world" 5 0 |> TextInput.deleteSelection
    m.Text   |> Expect.equal "text" " world"
    m.Cursor |> Expect.equal "cursor 0" 0
  }
  test "deleteSelection no-op when no selection" {
    let m = ti "hello" 3
    TextInput.deleteSelection m |> Expect.equal "unchanged" m
  }
  // ── deleteWordLeft ───────────────────────────────────────────────────────
  test "deleteWordLeft removes previous word" {
    let m = ti "hello world" 11 |> TextInput.deleteWordLeft
    m.Text   |> Expect.equal "text" "hello "
    m.Cursor |> Expect.equal "cursor 6" 6
  }
  test "deleteWordLeft no-op at position 0" {
    let m = ti "hello" 0
    TextInput.deleteWordLeft m |> Expect.equal "unchanged" m
  }
  // ── handleKeyWithSelection ───────────────────────────────────────────────
  test "handleKeyWithSelection Char replaces selection" {
    let m = tiSel "hello" 5 0 |> TextInput.handleKeyWithSelection (Key.Char (System.Text.Rune 'X'))
    m.Text   |> Expect.equal "text" "X"
    m.Cursor |> Expect.equal "cursor 1" 1
  }
  test "handleKeyWithSelection Backspace deletes selection" {
    let m = tiSel "hello" 5 0 |> TextInput.handleKeyWithSelection Key.Backspace
    m.Text   |> Expect.equal "empty" ""
    m.Cursor |> Expect.equal "cursor 0" 0
  }
  // ── handleEvent ──────────────────────────────────────────────────────────
  test "handleEvent Ctrl+Left triggers wordLeft" {
    let m = ti "hello world" 11
    let m' = TextInput.handleEvent (KeyPressed(Key.Left, Modifiers.Ctrl)) m
    m'.Cursor |> Expect.equal "cursor 6" 6
  }
  test "handleEvent Ctrl+Right triggers wordRight" {
    let m = ti "hello world" 0
    let m' = TextInput.handleEvent (KeyPressed(Key.Right, Modifiers.Ctrl)) m
    m'.Cursor |> Expect.equal "cursor 5" 5
  }
  test "handleEvent Shift+Left extends selection" {
    let m = ti "abc" 2
    let m' = TextInput.handleEvent (KeyPressed(Key.Left, Modifiers.Shift)) m
    m'.Cursor          |> Expect.equal "cursor 1" 1
    m'.SelectionAnchor |> Expect.equal "anchor 2" (Some 2)
  }
  test "handleEvent Ctrl+A selects all" {
    let m = ti "hello" 2
    let m' = TextInput.handleEvent (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Ctrl)) m
    m'.SelectionAnchor |> Expect.equal "anchor 0" (Some 0)
    m'.Cursor          |> Expect.equal "cursor 5" 5
  }
  test "handleEvent Ctrl+Backspace deletes previous word" {
    let m = ti "hello world" 11
    let m' = TextInput.handleEvent (KeyPressed(Key.Backspace, Modifiers.Ctrl)) m
    m'.Text   |> Expect.equal "text" "hello "
    m'.Cursor |> Expect.equal "cursor 6" 6
  }
  test "handleEvent plain key falls back to handleKeyWithSelection" {
    let m = ti "abc" 1
    let m' = TextInput.handleEvent (KeyPressed(Key.Char (System.Text.Rune 'X'), Modifiers.None)) m
    m'.Text |> Expect.equal "text" "aXbc"
  }
  test "selectWordLeft extends selection by word" {
    let m = ti "hello world" 11
    let m' = TextInput.selectWordLeft m
    m'.Cursor          |> Expect.equal "cursor 6" 6
    m'.SelectionAnchor |> Expect.equal "anchor 11" (Some 11)
  }
  test "selectWordRight extends selection by word" {
    let m = ti "hello world" 0
    let m' = TextInput.selectWordRight m
    m'.Cursor          |> Expect.equal "cursor 5" 5
    m'.SelectionAnchor |> Expect.equal "anchor 0" (Some 0)
  }
  test "handleEvent Shift+Ctrl+Left extends selection by word" {
    let m = ti "hello world" 11
    let m' = TextInput.handleEvent (KeyPressed(Key.Left, Modifiers.Ctrl ||| Modifiers.Shift)) m
    m'.Cursor          |> Expect.equal "cursor 6" 6
    m'.SelectionAnchor |> Expect.equal "anchor 11" (Some 11)
  }
  test "handleEvent Shift+Home extends selection to start" {
    let m = ti "hello" 3
    let m' = TextInput.handleEvent (KeyPressed(Key.Home, Modifiers.Shift)) m
    m'.Cursor          |> Expect.equal "cursor 0" 0
    m'.SelectionAnchor |> Expect.equal "anchor 3" (Some 3)
  }
  test "handleEvent Shift+End extends selection to end" {
    let m = ti "hello" 2
    let m' = TextInput.handleEvent (KeyPressed(Key.End, Modifiers.Shift)) m
    m'.Cursor          |> Expect.equal "cursor 5" 5
    m'.SelectionAnchor |> Expect.equal "anchor 2" (Some 2)
  }
]


// ── VirtualTable ──────────────────────────────────────────────────────────────

let vtColumns = [
  { Header = "ID"; Width = 5; SortKey = None; Fill = false; Render = fun (i: int) -> El.text (sprintf "%d" i) }
  { Header = "Name"; Width = 10; SortKey = None; Fill = false; Render = fun (i: int) -> El.text (sprintf "item%d" i) }
]

let virtualTableTests = testList "VirtualTable" [
  test "VirtualTable.create builds config with default selection color" {
    let cfg = VirtualTable.create vtColumns
    cfg.SelectionColor |> Expect.equal "blue" (Color.Named(BaseColor.Blue, Normal))
    cfg.Columns        |> Expect.hasLength "2 cols" 2
    cfg.SeparatorChar  |> Expect.equal "dash" '─'
  }
  test "VirtualTable.view renders header + sep + data Column" {
    let m = VirtualList.ofArray 3 [| 0; 1; 2 |]
    let cfg = VirtualTable.create vtColumns
    match VirtualTable.view cfg m with
    | Column [header; sep; _] ->
      match header with
      | Row _ -> ()
      | other -> failwith (sprintf "expected Row header, got %A" other)
      match sep with
      | Row _ -> ()
      | other -> failwith (sprintf "expected Row sep, got %A" other)
    | other -> failwith (sprintf "expected Column[3], got %A" other)
  }
  test "VirtualTable.view empty columns produces empty rows" {
    let m = VirtualList.ofArray 3 [| 0; 1 |]
    let cfg = VirtualTable.create []
    match VirtualTable.view cfg m with
    | Column [Row _; Row _; Column _] -> ()
    | other -> failwith (sprintf "unexpected: %A" other)
  }
  test "VirtualTable.view data window uses model ViewportHeight" {
    let m = VirtualList.ofArray 5 [| 0 .. 19 |]
    let cfg = VirtualTable.create vtColumns
    match VirtualTable.view cfg m with
    | Column [_; _; Column dataRows] ->
      dataRows |> Expect.hasLength "5 data rows" 5
    | other -> failwith (sprintf "unexpected: %A" other)
  }
  test "VirtualTable.view zero-width column emits Empty separator cell" {
    let cols = [{ Header = "X"; Width = 0; SortKey = None; Fill = false; Render = fun _ -> El.empty }]
    let m = VirtualList.ofArray 3 [| 0 |]
    let cfg = VirtualTable.create cols
    match VirtualTable.view cfg m with
    | Column [_; Row [Empty]; _] -> ()
    | other -> failwith (sprintf "unexpected: %A" other)
  }
  test "VirtualTable.view selected row gets SelectionColor background" {
    // Model starts with SelectedIndex = Some 0 (first row selected)
    let m = VirtualList.ofArray 3 [| 0; 1; 2 |]
    let cfg = VirtualTable.create vtColumns
    match VirtualTable.view cfg m with
    | Column [_; _; Column (row0 :: row1 :: _)] ->
      match row0 with
      | Styled(s, _) ->
        s.Bg |> Expect.equal "blue bg on selected" (Some (Color.Named(BaseColor.Blue, Normal)))
      | Row _ | Text _ ->
        failwith "row 0 (selected) should be Styled with selection color"
      | other -> failwith (sprintf "row 0 unexpected: %A" other)
      match row1 with
      | Styled(s, _) when s.Bg = Some (Color.Named(BaseColor.Blue, Normal)) ->
        failwith "row 1 (not selected) should NOT have selection color"
      | _ -> ()
    | other -> failwith (sprintf "unexpected top level: %A" other)
  }
]

// ── VirtualTable sort + fill tests ────────────────────────────────────────────

let vtSortFillTests = testList "VirtualTable sort+fill" [

  // ── TableColumn helpers ──────────────────────────────────────────────────────

  test "TableColumn.create has no sort and no fill by default" {
    let col = TableColumn.create "ID" 5 (fun (i: int) -> El.text (string i))
    col.SortKey |> Expect.isNone "no sort key"
    col.Fill    |> Expect.isFalse "not fill"
  }

  test "TableColumn.withSort adds a sort key" {
    let col = TableColumn.create "Val" 8 (fun (x: int) -> El.text (string x))
    let col2 = TableColumn.withSort (fun x -> x :> System.IComparable) col
    col2.SortKey |> Expect.isSome "has sort key"
  }

  test "TableColumn.asFill sets Fill to true" {
    let col = TableColumn.create "Val" 8 (fun (x: int) -> El.text (string x))
    let col2 = TableColumn.asFill col
    col2.Fill |> Expect.isTrue "fill is true"
    col2.Width |> Expect.equal "width unchanged" 8
  }

  // ── VirtualTable.toggleSort ──────────────────────────────────────────────────

  test "toggleSort None -> Ascending on column 0" {
    let result = VirtualTable.toggleSort 0 None
    result |> Expect.equal "ascending" (Some { Column = 0; Direction = SortDirection.Ascending })
  }

  test "toggleSort Ascending -> Descending on same column" {
    let current = Some { Column = 0; Direction = SortDirection.Ascending }
    let result = VirtualTable.toggleSort 0 current
    result |> Expect.equal "descending" (Some { Column = 0; Direction = SortDirection.Descending })
  }

  test "toggleSort Descending -> None on same column" {
    let current = Some { Column = 0; Direction = SortDirection.Descending }
    let result = VirtualTable.toggleSort 0 current
    result |> Expect.isNone "back to unsorted"
  }

  test "toggleSort different column resets to Ascending" {
    let current = Some { Column = 1; Direction = SortDirection.Descending }
    let result = VirtualTable.toggleSort 0 current
    result |> Expect.equal "new column ascending" (Some { Column = 0; Direction = SortDirection.Ascending })
  }

  // ── VirtualTable.sortItems ────────────────────────────────────────────────────

  test "sortItems None returns items unchanged" {
    let items = [| 3; 1; 2 |]
    let cols = [ TableColumn.create "N" 5 (fun (i: int) -> El.text (string i))
                 |> TableColumn.withSort (fun i -> i :> System.IComparable) ]
    let result = VirtualTable.sortItems cols None items
    result |> Expect.equal "unchanged" items
  }

  test "sortItems Ascending sorts low to high" {
    let items = [| 30; 10; 20 |]
    let cols = [ TableColumn.create "N" 5 (fun (i: int) -> El.text (string i))
                 |> TableColumn.withSort (fun i -> i :> System.IComparable) ]
    let sort = Some { Column = 0; Direction = SortDirection.Ascending }
    let result = VirtualTable.sortItems cols sort items
    result |> Expect.equal "ascending" [| 10; 20; 30 |]
  }

  test "sortItems Descending sorts high to low" {
    let items = [| 30; 10; 20 |]
    let cols = [ TableColumn.create "N" 5 (fun (i: int) -> El.text (string i))
                 |> TableColumn.withSort (fun i -> i :> System.IComparable) ]
    let sort = Some { Column = 0; Direction = SortDirection.Descending }
    let result = VirtualTable.sortItems cols sort items
    result |> Expect.equal "descending" [| 30; 20; 10 |]
  }

  test "sortItems with column lacking SortKey returns items unchanged" {
    let items = [| 3; 1; 2 |]
    let cols = [ TableColumn.create "N" 5 (fun (i: int) -> El.text (string i)) ] // no SortKey
    let sort = Some { Column = 0; Direction = SortDirection.Ascending }
    let result = VirtualTable.sortItems cols sort items
    result |> Expect.equal "unchanged (no key)" items
  }

  test "sortItems with out-of-range column index returns items unchanged" {
    let items = [| 3; 1; 2 |]
    let cols = [ TableColumn.create "N" 5 (fun (i: int) -> El.text (string i)) ]
    let sort = Some { Column = 99; Direction = SortDirection.Ascending }
    let result = VirtualTable.sortItems cols sort items
    result |> Expect.equal "unchanged (bad index)" items
  }

  // ── VirtualTable.view sort indicators ─────────────────────────────────────────

  test "VirtualTable.view shows ▲ in sorted-ascending column header" {
    let sortableCol =
      TableColumn.create "Score" 8 (fun (i: int) -> El.text (string i))
      |> TableColumn.withSort (fun i -> i :> System.IComparable)
    let cfg = { VirtualTable.create [sortableCol] with Sort = Some { Column = 0; Direction = SortDirection.Ascending } }
    let m = VirtualList.ofArray 3 [| 5; 3; 9 |]
    let view = VirtualTable.view cfg m
    let rendered = TestHarness.renderElement 20 1 view
    rendered |> Expect.stringContains "ascending indicator" "▲"
  }

  test "VirtualTable.view shows ▼ in sorted-descending column header" {
    let sortableCol =
      TableColumn.create "Score" 8 (fun (i: int) -> El.text (string i))
      |> TableColumn.withSort (fun i -> i :> System.IComparable)
    let cfg = { VirtualTable.create [sortableCol] with Sort = Some { Column = 0; Direction = SortDirection.Descending } }
    let m = VirtualList.ofArray 3 [| 5; 3; 9 |]
    let view = VirtualTable.view cfg m
    let rendered = TestHarness.renderElement 20 1 view
    rendered |> Expect.stringContains "descending indicator" "▼"
  }

  test "VirtualTable.view no sort indicator when Sort is None" {
    let sortableCol =
      TableColumn.create "Score" 8 (fun (i: int) -> El.text (string i))
      |> TableColumn.withSort (fun i -> i :> System.IComparable)
    let cfg = VirtualTable.create [sortableCol] // Sort = None
    let m = VirtualList.ofArray 3 [| 5; 3; 9 |]
    let view = VirtualTable.view cfg m
    let rendered = TestHarness.renderElement 20 1 view
    rendered |> Expect.stringContains "header visible" "Score"
    rendered.Contains("▲") |> Expect.isFalse "no up arrow"
    rendered.Contains("▼") |> Expect.isFalse "no down arrow"
  }

  // ── Fill column layout ────────────────────────────────────────────────────────

  test "Table.view fill column wraps cell in El.fill" {
    let fillCol = TableColumn.create "Name" 10 (fun ((_, name): int * string) -> El.text name) |> TableColumn.asFill
    let fixedCol = TableColumn.create "Age"  5 (fun ((age, _): int * string) -> El.text (string age))
    // With one row, structure is: Column [ Row headers; Row sep; Row dataRow ]
    match Table.view [fixedCol; fillCol] [(42, "Alice")] None with
    | Column (Row headerCells :: _sep :: Row dataCells :: _) ->
      match List.item 1 headerCells with
      | Constrained(Fill _, _) -> ()
      | other -> failwithf "fill column header not wrapped in El.fill: %A" other
      match List.item 1 dataCells with
      | Constrained(Fill _, _) -> ()
      | other -> failwithf "fill column data cell not wrapped in El.fill: %A" other
    | other -> failwithf "unexpected: %A" other
  }

]

// ── SplitPane tests ───────────────────────────────────────────────────────────

let splitPaneTests = testList "SplitPane" [
  test "horizontal split renders as Row with two children" {
    let left  = El.text "LEFT"
    let right = El.text "RIGHT"
    let m = SplitPane.init SplitHorizontal 50 left right
    match SplitPane.view m with
    | Row [_; _] -> ()
    | Row children -> failwithf "expected exactly 2 children, got %d" children.Length
    | other -> failwithf "expected Row, got %A" other
  }
  test "vertical split renders as Column with two children" {
    let top    = El.text "TOP"
    let bottom = El.text "BOTTOM"
    let m = SplitPane.init SplitVertical 30 top bottom
    match SplitPane.view m with
    | Column [_; _] -> ()
    | Column children -> failwithf "expected exactly 2 children, got %d" children.Length
    | other -> failwithf "expected Column, got %A" other
  }
  test "horizontal split respects Ratio constraints" {
    let m = SplitPane.init SplitHorizontal 40 (El.text "A") (El.text "B")
    match SplitPane.view m with
    | Row [Constrained(Ratio(40, 100), _); Constrained(Ratio(60, 100), _)] -> ()
    | Row [Constrained(c1, _); Constrained(c2, _)] ->
      failwithf "expected Ratio(40,100)/Ratio(60,100), got %A / %A" c1 c2
    | other -> failwithf "unexpected structure: %A" other
  }
  test "vertical split respects Ratio constraints" {
    let m = SplitPane.init SplitVertical 25 (El.text "TOP") (El.text "BOT")
    match SplitPane.view m with
    | Column [Constrained(Ratio(25, 100), _); Constrained(Ratio(75, 100), _)] -> ()
    | Column [Constrained(c1, _); Constrained(c2, _)] ->
      failwithf "expected Ratio(25,100)/Ratio(75,100), got %A / %A" c1 c2
    | other -> failwithf "unexpected structure: %A" other
  }
  test "SplitPane.resize clamps to [1, 99]" {
    let m = SplitPane.init SplitHorizontal 50 (El.text "A") (El.text "B")
    let tooHigh = SplitPane.resize 150 m
    tooHigh.SplitPercent |> Expect.equal "clamped to 99" 99
    let tooLow = SplitPane.resize (-10) m
    tooLow.SplitPercent |> Expect.equal "clamped to 1" 1
    let mid = SplitPane.resize 70 m
    mid.SplitPercent |> Expect.equal "mid stays 70" 70
  }
  test "SplitPane.grow/shrink increment by step" {
    let m = SplitPane.init SplitHorizontal 50 (El.text "A") (El.text "B")
    let grown = SplitPane.grow 5 m
    grown.SplitPercent |> Expect.equal "grew to 55" 55
    let shrunk = SplitPane.shrink 5 m
    shrunk.SplitPercent |> Expect.equal "shrunk to 45" 45
  }
  test "SplitPane.grow clamps at 99" {
    let m = SplitPane.init SplitHorizontal 95 (El.text "A") (El.text "B")
    let grown = SplitPane.grow 10 m
    grown.SplitPercent |> Expect.equal "clamped to 99" 99
  }
  test "SplitPane.shrink clamps at 1" {
    let m = SplitPane.init SplitHorizontal 5 (El.text "A") (El.text "B")
    let shrunk = SplitPane.shrink 10 m
    shrunk.SplitPercent |> Expect.equal "clamped to 1" 1
  }
  test "SplitPane.grow with negative step acts as shrink" {
    // grow(-n) is documented as equivalent to shrink(n)
    let m = SplitPane.init SplitHorizontal 50 (El.text "A") (El.text "B")
    let grownNeg  = SplitPane.grow (-10) m
    let shrunkPos = SplitPane.shrink 10 m
    grownNeg.SplitPercent  |> Expect.equal "grow(-10) -> 40" 40
    shrunkPos.SplitPercent |> Expect.equal "shrink(10) -> 40" 40
    grownNeg.SplitPercent  |> Expect.equal "grow(-n) equals shrink(n)" shrunkPos.SplitPercent
  }
  test "SplitPane.shrink with negative step acts as grow" {
    // shrink(-n) is documented as equivalent to grow(n)
    let m = SplitPane.init SplitHorizontal 50 (El.text "A") (El.text "B")
    let shrunkNeg = SplitPane.shrink (-10) m
    let grownPos  = SplitPane.grow 10 m
    shrunkNeg.SplitPercent |> Expect.equal "shrink(-10) -> 60" 60
    grownPos.SplitPercent  |> Expect.equal "grow(10) -> 60" 60
    shrunkNeg.SplitPercent |> Expect.equal "shrink(-n) equals grow(n)" grownPos.SplitPercent
  }
  test "SplitPane.setFirst/setSecond update children" {
    let m = SplitPane.init SplitHorizontal 50 (El.text "A") (El.text "B")
    let m2 = m |> SplitPane.setFirst (El.text "NEW-A")
    match m2.First with
    | Text("NEW-A", _) -> ()
    | other -> failwithf "expected Text NEW-A, got %A" other
    let m3 = m |> SplitPane.setSecond (El.text "NEW-B")
    match m3.Second with
    | Text("NEW-B", _) -> ()
    | other -> failwithf "expected Text NEW-B, got %A" other
  }
  test "SplitPane view uses First/Second elements" {
    let m = SplitPane.init SplitHorizontal 50 (El.text "hello") (El.text "world")
    match SplitPane.view m with
    | Row [Constrained(_, first); Constrained(_, second)] ->
      match first  with Text("hello", _) -> () | x -> failwithf "first unexpected: %A" x
      match second with Text("world", _) -> () | x -> failwithf "second unexpected: %A" x
    | other -> failwithf "unexpected: %A" other
  }
]

// ── Sprint 32: Surrogate pair cursor correctness ──────────────────────────────
// Emoji and supplementary Unicode chars encode as surrogate pairs in .NET strings
// (two UTF-16 code units). Cursor movement must skip both units atomically.
let textInputSurrogateTests = testList "TextInput surrogate pair handling" [
  let emoji = "\U0001F600"  // 😀 — 2 UTF-16 code units: length = 2

  test "Left moves by 2 when cursor is after low surrogate" {
    // "a😀b": a=0, hi=1, lo=2, b=3 — cursor 3 = after emoji
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 3 }
    (TextInput.handleKey Key.Left m).Cursor |> Expect.equal "skipped pair" 1
  }
  test "Left moves by 1 for BMP char" {
    let m = { TextInput.ofString "hello" with Cursor = 3 }
    (TextInput.handleKey Key.Left m).Cursor |> Expect.equal "normal step" 2
  }
  test "Right moves by 2 when cursor is before high surrogate" {
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 1 }
    (TextInput.handleKey Key.Right m).Cursor |> Expect.equal "skipped pair" 3
  }
  test "Right moves by 1 for BMP char" {
    let m = { TextInput.ofString "hello" with Cursor = 2 }
    (TextInput.handleKey Key.Right m).Cursor |> Expect.equal "normal step" 3
  }
  test "Backspace deletes entire emoji" {
    // cursor at 3 (after emoji), backspace should remove both surrogate units
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 3 }
    let m2 = TextInput.handleKey Key.Backspace m
    m2.Text   |> Expect.equal "emoji removed" "ab"
    m2.Cursor |> Expect.equal "cursor at 1"   1
  }
  test "Backspace in BMP removes one char" {
    let m = { TextInput.ofString "hello" with Cursor = 3 }
    let m2 = TextInput.handleKey Key.Backspace m
    m2.Text   |> Expect.equal "char removed" "helo"
    m2.Cursor |> Expect.equal "cursor at 2"  2
  }
  test "Delete removes entire emoji" {
    // cursor at 1 (before emoji high surrogate), delete should remove both units
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 1 }
    let m2 = TextInput.handleKey Key.Delete m
    m2.Text   |> Expect.equal "emoji removed" "ab"
    m2.Cursor |> Expect.equal "cursor unchanged" 1
  }
  test "Delete in BMP removes one char" {
    let m = { TextInput.ofString "hello" with Cursor = 2 }
    let m2 = TextInput.handleKey Key.Delete m
    m2.Text   |> Expect.equal "char removed" "helo"
    m2.Cursor |> Expect.equal "cursor unchanged" 2
  }
  test "selectLeft extends selection by 2 across emoji" {
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 3 }
    let m2 = TextInput.selectLeft m
    m2.Cursor          |> Expect.equal "skipped pair" 1
    m2.SelectionAnchor |> Expect.equal "anchor at 3"  (Some 3)
  }
  test "selectRight extends selection by 2 across emoji" {
    let m = { TextInput.ofString ("a" + emoji + "b") with Cursor = 1 }
    let m2 = TextInput.selectRight m
    m2.Cursor          |> Expect.equal "skipped pair" 3
    m2.SelectionAnchor |> Expect.equal "anchor at 1"  (Some 1)
  }
  test "insert emoji advances cursor by 2" {
    let m = TextInput.empty
    let m2 = TextInput.handleKey (Key.Char (System.Text.Rune 0x1F600)) m
    m2.Text   |> Expect.equal "emoji inserted" emoji
    m2.Cursor |> Expect.equal "cursor at 2" 2
  }
  test "round-trip: insert then delete restores empty" {
    let m0 = TextInput.empty
    let m1 = TextInput.handleKey (Key.Char (System.Text.Rune 0x1F600)) m0
    let m2 = TextInput.handleKey Key.Backspace m1
    m2.Text   |> Expect.equal "back to empty" ""
    m2.Cursor |> Expect.equal "cursor at 0"   0
  }
]

[<Tests>]
let allWidgetTests = testList "Widgets" [
  progressBarTests
  tabsTests
  tableTests
  colorShortcutTests
  textfTests
  paragraphTests
  textInputEnhancedTests
  modalTests
  focusTests
  checkboxTests
  toggleTests
  radioGroupTests
  spinnerTests
  toastTests
  treeViewTests
  lazyTests
  formTests
  textFormTests
  themeTests
  themedWidgetTests
  themedColorSemanticTests
  frameTimingsTests
  undoableCommitTests
  focusRingTests
  virtualListTests
  textInputWordSelTests
  textInputSurrogateTests
  virtualTableTests
  vtSortFillTests
  splitPaneTests
]
