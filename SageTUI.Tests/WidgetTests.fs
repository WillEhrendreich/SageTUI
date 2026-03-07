module WidgetTests

open Expecto
open Expecto.Flip
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
  test "colored bar produces Row" {
    let config =
      { ProgressBar.defaults with
          Percent = 0.5; Width = 10; ShowLabel = false
          FilledColor = Some (Color.Named(BaseColor.Green, Intensity.Normal))
          EmptyColor = Some (Color.Named(BaseColor.Red, Intensity.Normal)) }
    let elem = ProgressBar.view config
    match elem with
    | Row _ -> ()
    | _ -> failtest "expected Row for colored bar"
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
      { Header = "Name"; Width = 10; Render = fun (n, _) -> El.text n }
      { Header = "Age"; Width = 5; Render = fun (_, a) -> El.text (string a) }
    ]
    let rows = [("Alice", 30); ("Bob", 25)]
    let elem = Table.view cols rows None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep+2rows" 4
    | _ -> failtest "expected Column"
  }
  test "selected row gets styled" {
    let cols: TableColumn<string> list = [
      { Header = "X"; Width = 5; Render = fun x -> El.text x }
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
      { Header = "H"; Width = 3; Render = fun _ -> El.text "" }
    ]
    let elem = Table.view cols [] None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep" 2
    | _ -> failtest "expected Column"
  }
  test "multiple columns render in each row" {
    let cols: TableColumn<int> list = [
      { Header = "A"; Width = 5; Render = fun x -> El.text (string x) }
      { Header = "B"; Width = 5; Render = fun x -> El.text (string (x * 2)) }
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
]

type TestFormModel = { Name: string; Age: int; Active: bool }

let formTests = testList "Form" [
  let nameField =
    Form.field "name"
      (fun focused (model: TestFormModel) ->
        let el = El.text (sprintf "Name: %s" model.Name)
        match focused with true -> el |> El.bold | false -> el)
      (fun _key (_model: TestFormModel) -> None)

  let ageField =
    Form.field "age"
      (fun focused (model: TestFormModel) ->
        let el = El.text (sprintf "Age: %d" model.Age)
        match focused with true -> el |> El.bold | false -> el)
      (fun _key (_model: TestFormModel) -> None)

  let activeField =
    Form.field "active"
      (fun focused (model: TestFormModel) ->
        Checkbox.view "Active" focused model.Active)
      (fun _key (_model: TestFormModel) -> None)

  let fields = [nameField; ageField; activeField]
  let model = { Name = "Alice"; Age = 30; Active = true }

  test "keys extracts field keys" {
    Form.keys fields |> Expect.equal "keys" ["name"; "age"; "active"]
  }

  test "view renders all fields as column" {
    let elem = Form.view fields "name" model
    match elem with
    | Column items -> items |> Expect.hasLength "3 fields" 3
    | _ -> failtest "expected Column"
  }

  test "view highlights focused field" {
    let elem = Form.view fields "name" model
    match elem with
    | Column (first :: _) ->
      match first with
      | Styled(_, _) -> ()
      | _ -> failtest "expected Styled for focused"
    | _ -> failtest "expected Column"
  }

  test "handleFocus moves to next field" {
    let next = Form.handleFocus fields "name" FocusNext
    next |> Expect.equal "next is age" "age"
  }

  test "handleFocus moves to previous field" {
    let prev = Form.handleFocus fields "age" FocusPrev
    prev |> Expect.equal "prev is name" "name"
  }

  test "handleFocus wraps around" {
    let next = Form.handleFocus fields "active" FocusNext
    next |> Expect.equal "wraps to name" "name"
  }

  test "handleKey routes to focused field" {
    let result = Form.handleKey fields "name" Key.Enter model
    result |> Expect.isNone "no handler for enter on name"
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
    | Padded(_, Bordered(_, Column _)) -> ()
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
  themeTests
]
