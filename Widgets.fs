namespace SageTUI

type Theme = {
  Primary: Color
  Secondary: Color
  Accent: Color
  Success: Color
  Warning: Color
  Error: Color
  TextFg: Color
  TextDim: Color
  Background: Color
  Border: BorderStyle
}

module Theme =
  let dark : Theme =
    { Primary = Color.Named(Cyan, Bright)
      Secondary = Color.Named(Blue, Bright)
      Accent = Color.Named(Magenta, Bright)
      Success = Color.Named(Green, Bright)
      Warning = Color.Named(Yellow, Bright)
      Error = Color.Named(Red, Bright)
      TextFg = Color.Named(White, Normal)
      TextDim = Color.Named(White, Normal)
      Background = Color.Named(Black, Normal)
      Border = Rounded }

  let light : Theme =
    { Primary = Color.Named(Blue, Normal)
      Secondary = Color.Named(Cyan, Normal)
      Accent = Color.Named(Magenta, Normal)
      Success = Color.Named(Green, Normal)
      Warning = Color.Named(Yellow, Normal)
      Error = Color.Named(Red, Normal)
      TextFg = Color.Named(Black, Normal)
      TextDim = Color.Named(Black, Normal)
      Background = Color.Named(White, Normal)
      Border = Light }

  let nord : Theme =
    { Primary = Color.Rgb(136uy, 192uy, 208uy)
      Secondary = Color.Rgb(129uy, 161uy, 193uy)
      Accent = Color.Rgb(180uy, 142uy, 173uy)
      Success = Color.Rgb(163uy, 190uy, 140uy)
      Warning = Color.Rgb(235uy, 203uy, 139uy)
      Error = Color.Rgb(191uy, 97uy, 106uy)
      TextFg = Color.Rgb(216uy, 222uy, 233uy)
      TextDim = Color.Rgb(76uy, 86uy, 106uy)
      Background = Color.Rgb(46uy, 52uy, 64uy)
      Border = Rounded }

  let dracula : Theme =
    { Primary = Color.Rgb(139uy, 233uy, 253uy)
      Secondary = Color.Rgb(189uy, 147uy, 249uy)
      Accent = Color.Rgb(255uy, 121uy, 198uy)
      Success = Color.Rgb(80uy, 250uy, 123uy)
      Warning = Color.Rgb(241uy, 250uy, 140uy)
      Error = Color.Rgb(255uy, 85uy, 85uy)
      TextFg = Color.Rgb(248uy, 248uy, 242uy)
      TextDim = Color.Rgb(98uy, 114uy, 164uy)
      Background = Color.Rgb(40uy, 42uy, 54uy)
      Border = Rounded }

  let catppuccin : Theme =
    { Primary = Color.Rgb(137uy, 180uy, 250uy)
      Secondary = Color.Rgb(116uy, 199uy, 236uy)
      Accent = Color.Rgb(245uy, 194uy, 231uy)
      Success = Color.Rgb(166uy, 218uy, 149uy)
      Warning = Color.Rgb(249uy, 226uy, 175uy)
      Error = Color.Rgb(243uy, 139uy, 168uy)
      TextFg = Color.Rgb(205uy, 214uy, 244uy)
      TextDim = Color.Rgb(108uy, 112uy, 134uy)
      Background = Color.Rgb(30uy, 30uy, 46uy)
      Border = Rounded }

  /// Apply theme colors to an element tree
  let apply (theme: Theme) (elem: Element) : Element =
    elem |> El.fg theme.TextFg |> El.bg theme.Background

  /// Styled heading using theme primary color
  let heading (theme: Theme) (text: string) : Element =
    El.text text |> El.bold |> El.fg theme.Primary

  /// Styled subheading using theme secondary color
  let subheading (theme: Theme) (text: string) : Element =
    El.text text |> El.fg theme.Secondary

  /// Styled success text
  let success (theme: Theme) (text: string) : Element =
    El.text text |> El.fg theme.Success

  /// Styled warning text
  let warning (theme: Theme) (text: string) : Element =
    El.text text |> El.fg theme.Warning

  /// Styled error text
  let error (theme: Theme) (text: string) : Element =
    El.text text |> El.fg theme.Error

  /// Themed border
  let bordered (theme: Theme) (elem: Element) : Element =
    elem |> El.bordered theme.Border

  /// Create a themed panel (bordered + padded + title)
  let panel (theme: Theme) (title: string) (content: Element) : Element =
    El.column [
      El.text title |> El.bold |> El.fg theme.Primary
      content
    ] |> El.bordered theme.Border |> El.padHV 1 0

type TextInputModel = {
  Text: string
  Cursor: int
}

module TextInput =
  let empty = { Text = ""; Cursor = 0 }

  let ofString s = { Text = s; Cursor = String.length s }

  let insertText (s: string) (model: TextInputModel) =
    let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
    let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
    { Text = before + s + after; Cursor = model.Cursor + s.Length }

  let handleKey (key: Key) (model: TextInputModel) =
    match key with
    | Key.Char c ->
      let before = model.Text.[..model.Cursor - 1]
      let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
      { Text = before + string c + after; Cursor = model.Cursor + 1 }
    | Key.Backspace ->
      match model.Cursor > 0 with
      | true ->
        let before = model.Text.[..model.Cursor - 2]
        let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
        { Text = before + after; Cursor = model.Cursor - 1 }
      | false -> model
    | Key.Delete ->
      match model.Cursor < model.Text.Length with
      | true ->
        let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
        let after = match model.Cursor + 1 < model.Text.Length with true -> model.Text.[model.Cursor + 1..] | false -> ""
        { Text = before + after; Cursor = model.Cursor }
      | false -> model
    | Key.Left ->
      { model with Cursor = max 0 (model.Cursor - 1) }
    | Key.Right ->
      { model with Cursor = min model.Text.Length (model.Cursor + 1) }
    | Key.Home ->
      { model with Cursor = 0 }
    | Key.End ->
      { model with Cursor = model.Text.Length }
    | _ -> model

  let handlePaste (text: string) (model: TextInputModel) =
    let clean = text.Replace("\r\n", " ").Replace("\n", " ").Replace("\r", " ")
    insertText clean model

  let view (focused: bool) (model: TextInputModel) =
    let displayText =
      match model.Text.Length = 0 with
      | true -> " "
      | false -> model.Text
    match focused with
    | true -> El.text displayText |> El.underline
    | false -> El.text displayText

  let viewWithPlaceholder (placeholder: string) (focused: bool) (model: TextInputModel) =
    match model.Text.Length = 0 with
    | true ->
      match focused with
      | true -> El.text placeholder |> El.dim |> El.underline
      | false -> El.text placeholder |> El.dim
    | false ->
      view focused model

type FocusRing<'a> = {
  Items: 'a list
  Index: int
}

module FocusRing =
  let create (items: 'a list) = { Items = items; Index = 0 }

  let current (ring: FocusRing<'a>) =
    match ring.Items.Length > 0 with
    | true -> Some ring.Items.[ring.Index]
    | false -> None

  let next (ring: FocusRing<'a>) =
    match ring.Items.Length with
    | 0 -> ring
    | n -> { ring with Index = (ring.Index + 1) % n }

  let prev (ring: FocusRing<'a>) =
    match ring.Items.Length with
    | 0 -> ring
    | n -> { ring with Index = (ring.Index - 1 + n) % n }

  let isFocused (item: 'a) (ring: FocusRing<'a>) =
    match current ring with
    | Some c -> c = item
    | None -> false

type SelectModel<'a> = {
  Options: 'a list
  Selected: int
  IsOpen: bool
}

module Select =
  let create (options: 'a list) = { Options = options; Selected = 0; IsOpen = false }

  let toggle (model: SelectModel<'a>) = { model with IsOpen = not model.IsOpen }

  let moveUp (model: SelectModel<'a>) =
    { model with Selected = max 0 (model.Selected - 1) }

  let moveDown (model: SelectModel<'a>) =
    { model with Selected = min (model.Options.Length - 1) (model.Selected + 1) }

  let confirm (model: SelectModel<'a>) =
    { model with IsOpen = false }

  let selectedValue (model: SelectModel<'a>) =
    match model.Options.Length > 0 with
    | true -> Some model.Options.[model.Selected]
    | false -> None

  let view (toString: 'a -> string) (focused: bool) (model: SelectModel<'a>) =
    match model.IsOpen with
    | true ->
      model.Options
      |> List.mapi (fun i opt ->
        let prefix = match i = model.Selected with true -> "▸ " | false -> "  "
        El.text (prefix + toString opt)
        |> (match i = model.Selected with true -> El.bold | false -> id))
      |> El.column
    | false ->
      let display =
        match selectedValue model with
        | Some v -> toString v
        | None -> "(none)"
      El.text (sprintf "▾ %s" display)
      |> (match focused with true -> El.underline | false -> id)

type ProgressBarConfig = {
  Percent: float
  Width: int
  FilledChar: char
  EmptyChar: char
  FilledColor: Color option
  EmptyColor: Color option
  ShowLabel: bool
}

module ProgressBar =
  let defaults = {
    Percent = 0.0
    Width = 20
    FilledChar = '█'
    EmptyChar = '░'
    FilledColor = None
    EmptyColor = None
    ShowLabel = true
  }

  let view (config: ProgressBarConfig) =
    let pct = config.Percent |> max 0.0 |> min 1.0
    let filled = int (float config.Width * pct)
    let empty = config.Width - filled
    let bar =
      System.String(config.FilledChar, filled) +
      System.String(config.EmptyChar, empty)
    let barEl =
      match config.FilledColor, config.EmptyColor with
      | Some fc, Some ec ->
        let filledEl =
          El.text (System.String(config.FilledChar, filled)) |> El.fg fc
        let emptyEl =
          El.text (System.String(config.EmptyChar, empty)) |> El.fg ec
        El.row [ filledEl; emptyEl ]
      | Some fc, None -> El.text bar |> El.fg fc
      | None, Some ec -> El.text bar |> El.fg ec
      | None, None -> El.text bar
    match config.ShowLabel with
    | true ->
      let label = sprintf " %d%%" (int (pct * 100.0))
      El.row [ barEl; El.text label ]
    | false -> barEl

type TabsConfig<'a> = {
  Items: 'a list
  ActiveIndex: int
  ToString: 'a -> string
  ActiveColor: Color option
  InactiveColor: Color option
}

module Tabs =
  let view (config: TabsConfig<'a>) =
    config.Items
    |> List.mapi (fun i item ->
      let label = sprintf " %s " (config.ToString item)
      let isActive = i = config.ActiveIndex
      match isActive with
      | true ->
        let el = El.text label |> El.bold
        match config.ActiveColor with
        | Some c -> el |> El.fg c
        | None -> el
      | false ->
        let el = El.text label
        match config.InactiveColor with
        | Some c -> el |> El.fg c
        | None -> el)
    |> El.row

type TableColumn<'a> = {
  Header: string
  Width: int
  Render: 'a -> Element
}

module Table =
  let view (columns: TableColumn<'a> list) (rows: 'a list) (selectedRow: int option) =
    let header =
      columns
      |> List.map (fun col ->
        El.text col.Header |> El.bold |> El.width col.Width)
      |> El.row
    let separator =
      columns
      |> List.map (fun col ->
        El.text (System.String('─', col.Width)) |> El.width col.Width)
      |> El.row
    let dataRows =
      rows
      |> List.mapi (fun i row ->
        let rowEl =
          columns
          |> List.map (fun col -> col.Render row |> El.width col.Width)
          |> El.row
        match selectedRow with
        | Some si when si = i ->
          rowEl |> El.bg (Color.Named(BaseColor.Blue, Intensity.Normal))
        | _ -> rowEl)
    El.column (header :: separator :: dataRows)

type ModalConfig = {
  Backdrop: Color option
  BorderStyle: BorderStyle
  MaxWidth: int option
  MaxHeight: int option
}

module Modal =
  let defaults = {
    Backdrop = Some (Color.Named(BaseColor.Black, Normal))
    BorderStyle = Light
    MaxWidth = None
    MaxHeight = None
  }

  let view (config: ModalConfig) (content: Element) =
    let inner =
      content
      |> El.bordered config.BorderStyle
      |> (fun el ->
        match config.MaxWidth with
        | Some w -> el |> El.maxWidth w
        | None -> el)
      |> (fun el ->
        match config.MaxHeight with
        | Some h -> el |> El.maxHeight h
        | None -> el)
      |> El.center
    match config.Backdrop with
    | Some color ->
      El.overlay [
        El.text " " |> El.bg color |> El.fill
        inner
      ]
    | None -> inner

  let simple (content: Element) =
    view defaults content

module Focus =
  let tabOrder (keys: string list) (focused: string) (dir: FocusDirection) =
    match keys with
    | [] -> focused
    | _ ->
      match List.tryFindIndex ((=) focused) keys with
      | None -> List.head keys
      | Some idx ->
        match dir with
        | FocusNext -> keys.[(idx + 1) % keys.Length]
        | FocusPrev -> keys.[(idx - 1 + keys.Length) % keys.Length]

  let focusSub (toMsg: FocusDirection -> 'msg) : Sub<'msg> =
    FocusSub (fun dir -> Some (toMsg dir))

module Checkbox =
  let toggle (value: bool) = not value

  let view (label: string) (focused: bool) (checked': bool) =
    let box = match checked' with true -> "[✓]" | false -> "[ ]"
    let el = El.text (sprintf "%s %s" box label)
    match focused with true -> el |> El.bold | false -> el

module Toggle =
  let toggle (value: bool) = not value

  let view (onLabel: string) (offLabel: string) (focused: bool) (value: bool) =
    let display = match value with true -> sprintf "● %s" onLabel | false -> sprintf "○ %s" offLabel
    let el = El.text display
    match focused with true -> el |> El.bold | false -> el

type RadioGroupModel<'a> = {
  Options: 'a list
  Selected: int
}

module RadioGroup =
  let create (options: 'a list) = { Options = options; Selected = 0 }

  let moveUp (model: RadioGroupModel<'a>) =
    { model with Selected = max 0 (model.Selected - 1) }

  let moveDown (model: RadioGroupModel<'a>) =
    { model with Selected = min (model.Options.Length - 1) (model.Selected + 1) }

  let selectedValue (model: RadioGroupModel<'a>) =
    match model.Options.Length > 0 with
    | true -> Some model.Options.[model.Selected]
    | false -> None

  let handleKey (key: Key) (model: RadioGroupModel<'a>) =
    match key with
    | Key.Up -> moveUp model
    | Key.Down -> moveDown model
    | _ -> model

  let view (toString: 'a -> string) (focused: bool) (model: RadioGroupModel<'a>) =
    model.Options
    |> List.mapi (fun i opt ->
      let prefix = match i = model.Selected with true -> "◉ " | false -> "○ "
      let el = El.text (prefix + toString opt)
      match i = model.Selected && focused with true -> el |> El.bold | false -> el)
    |> El.column

module SpinnerWidget =
  let private frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
  let private dots = [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]
  let private bars = [| "▉"; "▊"; "▋"; "▌"; "▍"; "▎"; "▏"; "▎"; "▍"; "▌"; "▋"; "▊" |]

  let view (tick: int) =
    El.text frames.[tick % frames.Length]

  let viewDots (tick: int) =
    El.text dots.[tick % dots.Length]

  let viewBars (tick: int) =
    El.text bars.[tick % bars.Length]

module Toast =
  type ToastModel = {
    Message: string
    RemainingTicks: int
    Style: Style
  }

  let create (message: string) (ticks: int) =
    { Message = message; RemainingTicks = ticks; Style = Style.empty }

  let createStyled (message: string) (ticks: int) (style: Style) =
    { Message = message; RemainingTicks = ticks; Style = style }

  let tick (model: ToastModel) =
    match model.RemainingTicks > 0 with
    | true -> Some { model with RemainingTicks = model.RemainingTicks - 1 }
    | false -> None

  let isExpired (model: ToastModel) = model.RemainingTicks <= 0

  let view (model: ToastModel) =
    El.text model.Message
    |> El.styled model.Style
    |> El.bordered Light
    |> El.padHV 1 0

type TreeNode<'a> =
  | Leaf of 'a
  | Branch of 'a * children: TreeNode<'a> list

module TreeNode =
  let value node =
    match node with
    | Leaf v -> v
    | Branch(v, _) -> v

  let children node =
    match node with
    | Leaf _ -> []
    | Branch(_, cs) -> cs

  let isLeaf node =
    match node with
    | Leaf _ -> true
    | Branch _ -> false

type TreeState = {
  Expanded: Set<int list>
  Cursor: int list
}

module TreeView =
  let init () =
    { Expanded = Set.empty; Cursor = [0] }

  let isExpanded (path: int list) (state: TreeState) =
    state.Expanded.Contains path

  let toggleExpand (path: int list) (state: TreeState) =
    match state.Expanded.Contains path with
    | true -> { state with Expanded = state.Expanded.Remove path }
    | false -> { state with Expanded = state.Expanded.Add path }

  let expand (path: int list) (state: TreeState) =
    { state with Expanded = state.Expanded.Add path }

  let collapse (path: int list) (state: TreeState) =
    { state with Expanded = state.Expanded.Remove path }

  let expandAll (nodes: TreeNode<'a> list) (state: TreeState) =
    let rec paths prefix nodes =
      nodes |> List.mapi (fun i node ->
        let p = prefix @ [i]
        match node with
        | Leaf _ -> []
        | Branch(_, cs) -> p :: paths p cs)
      |> List.concat
    { state with Expanded = paths [] nodes |> Set.ofList }

  // Flatten tree into (path, node, depth) list respecting expanded state
  let private flatten (nodes: TreeNode<'a> list) (state: TreeState) =
    let result = ResizeArray<int list * TreeNode<'a> * int>()
    let rec walk prefix depth items =
      items |> List.iteri (fun i node ->
        let path = prefix @ [i]
        result.Add(path, node, depth)
        match node with
        | Branch(_, cs) when state.Expanded.Contains path ->
          walk path (depth + 1) cs
        | _ -> ())
    walk [] 0 nodes
    result |> Seq.toList

  let visiblePaths (nodes: TreeNode<'a> list) (state: TreeState) =
    flatten nodes state |> List.map (fun (p, _, _) -> p)

  let moveCursorUp (nodes: TreeNode<'a> list) (state: TreeState) =
    let visible = visiblePaths nodes state
    match List.tryFindIndex ((=) state.Cursor) visible with
    | Some idx when idx > 0 -> { state with Cursor = visible.[idx - 1] }
    | _ -> state

  let moveCursorDown (nodes: TreeNode<'a> list) (state: TreeState) =
    let visible = visiblePaths nodes state
    match List.tryFindIndex ((=) state.Cursor) visible with
    | Some idx when idx < visible.Length - 1 -> { state with Cursor = visible.[idx + 1] }
    | _ -> state

  let cursorNode (nodes: TreeNode<'a> list) (state: TreeState) =
    let rec find prefix items =
      items |> List.mapi (fun i node ->
        let path = prefix @ [i]
        match path = state.Cursor with
        | true -> Some node
        | false ->
          match node with
          | Branch(_, cs) -> find path cs
          | Leaf _ -> None)
      |> List.tryPick id
    find [] nodes

  let handleKey (key: Key) (nodes: TreeNode<'a> list) (state: TreeState) =
    match key with
    | Key.Up -> moveCursorUp nodes state
    | Key.Down -> moveCursorDown nodes state
    | Key.Right ->
      match cursorNode nodes state with
      | Some(Branch _) -> expand state.Cursor state
      | _ -> state
    | Key.Left ->
      match isExpanded state.Cursor state with
      | true -> collapse state.Cursor state
      | false ->
        // Move to parent
        match state.Cursor with
        | _ :: _ when state.Cursor.Length > 1 ->
          { state with Cursor = state.Cursor |> List.take (state.Cursor.Length - 1) }
        | _ -> state
    | Key.Enter ->
      match cursorNode nodes state with
      | Some(Branch _) -> toggleExpand state.Cursor state
      | _ -> state
    | _ -> state

  let view (toString: 'a -> string) (focused: bool) (nodes: TreeNode<'a> list) (state: TreeState) =
    let items = flatten nodes state
    items
    |> List.map (fun (path, node, depth) ->
      let isCursor = path = state.Cursor
      let prefix =
        match node with
        | Leaf _ -> "  "
        | Branch(_, cs) ->
          match isExpanded path state with
          | true -> "▾ "
          | false -> "▸ "
      let indent = String.replicate (depth * 2) " "
      let label = toString (TreeNode.value node)
      let el = El.text (sprintf "%s%s%s" indent prefix label)
      match isCursor && focused with
      | true -> el |> El.bold |> El.reverse
      | false -> el)
    |> El.column

type FormField<'model, 'msg> = {
  Key: string
  View: bool -> 'model -> Element
  HandleKey: Key -> 'model -> 'msg option
}

module Form =
  let field (key: string) (view: bool -> 'model -> Element) (handleKey: Key -> 'model -> 'msg option) : FormField<'model, 'msg> =
    { Key = key; View = view; HandleKey = handleKey }

  let view (fields: FormField<'model, 'msg> list) (focusedKey: string) (model: 'model) : Element =
    fields
    |> List.map (fun f -> f.View (f.Key = focusedKey) model)
    |> El.column

  let handleKey (fields: FormField<'model, 'msg> list) (focusedKey: string) (key: Key) (model: 'model) : 'msg option =
    fields
    |> List.tryFind (fun f -> f.Key = focusedKey)
    |> Option.bind (fun f -> f.HandleKey key model)

  let keys (fields: FormField<'model, 'msg> list) : string list =
    fields |> List.map (fun f -> f.Key)

  let handleFocus (fields: FormField<'model, 'msg> list) (focusedKey: string) (dir: FocusDirection) : string =
    Focus.tabOrder (keys fields) focusedKey dir
