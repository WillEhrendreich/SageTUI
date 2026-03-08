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

/// Single-line text input widget with cursor movement and editing operations.
type TextInputModel = {
  /// The current text content.
  Text: string
  /// Zero-based cursor position (0 = before first char).
  Cursor: int
}

module TextInput =
  /// Empty input with cursor at position 0.
  let empty = { Text = ""; Cursor = 0 }

  /// Create an input pre-filled with a string, cursor placed at the end.
  let ofString s = { Text = s; Cursor = String.length s }

  /// Insert a string at the current cursor position, advancing the cursor.
  let insertText (s: string) (model: TextInputModel) =
    let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
    let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
    { Text = before + s + after; Cursor = model.Cursor + s.Length }

  /// Handle a key press: Char inserts, Backspace/Delete remove, arrows/Home/End move cursor.
  let handleKey (key: Key) (model: TextInputModel) =
    match key with
    | Key.Char c ->
      let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
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

  /// Handle a paste event: strips newlines (replaced with spaces) then inserts at cursor.
  let handlePaste (text: string) (model: TextInputModel) =
    let clean = text.Replace("\r\n", " ").Replace("\n", " ").Replace("\r", " ")
    insertText clean model

  /// Render the text input. Shows underline when focused. Empty input renders as a single space.
  let view (focused: bool) (model: TextInputModel) =
    let displayText =
      match model.Text.Length = 0 with
      | true -> " "
      | false -> model.Text
    match focused with
    | true -> El.text displayText |> El.underline
    | false -> El.text displayText

  /// Render with a dimmed placeholder shown when the input is empty.
  let viewWithPlaceholder (placeholder: string) (focused: bool) (model: TextInputModel) =
    match model.Text.Length = 0 with
    | true ->
      match focused with
      | true -> El.text placeholder |> El.dim |> El.underline
      | false -> El.text placeholder |> El.dim
    | false ->
      view focused model

/// Tracks which item in a list currently has keyboard focus, with wrap-around navigation.
type FocusRing<'a> = {
  Items: 'a list
  Index: int
}

module FocusRing =
  /// Create a focus ring from a list of items, starting at index 0.
  let create (items: 'a list) = { Items = items; Index = 0 }

  /// Return the currently focused item, or None if the ring is empty.
  let current (ring: FocusRing<'a>) =
    match ring.Items.Length > 0 with
    | true -> Some ring.Items.[ring.Index]
    | false -> None

  /// Advance focus to the next item (wraps around to the start).
  let next (ring: FocusRing<'a>) =
    match ring.Items.Length with
    | 0 -> ring
    | n -> { ring with Index = (ring.Index + 1) % n }

  /// Move focus to the previous item (wraps around to the end).
  let prev (ring: FocusRing<'a>) =
    match ring.Items.Length with
    | 0 -> ring
    | n -> { ring with Index = (ring.Index - 1 + n) % n }

  /// Return true if the given item is the currently focused one.
  let isFocused (item: 'a) (ring: FocusRing<'a>) =
    match current ring with
    | Some c -> c = item
    | None -> false

/// Dropdown selector widget with open/closed state and keyboard navigation.
type SelectModel<'a> = {
  Options: 'a list
  Selected: int
  IsOpen: bool
}

module Select =
  /// Create a closed select with the first option highlighted.
  let create (options: 'a list) = { Options = options; Selected = 0; IsOpen = false }

  /// Toggle the dropdown open/closed.
  let toggle (model: SelectModel<'a>) = { model with IsOpen = not model.IsOpen }

  /// Move selection up one item (clamped at 0).
  let moveUp (model: SelectModel<'a>) =
    { model with Selected = max 0 (model.Selected - 1) }

  /// Move selection down one item (clamped at last option).
  let moveDown (model: SelectModel<'a>) =
    { model with Selected = min (model.Options.Length - 1) (model.Selected + 1) }

  /// Close the dropdown, confirming the current selection.
  let confirm (model: SelectModel<'a>) =
    { model with IsOpen = false }

  /// Return the currently selected value, or None if the options list is empty.
  let selectedValue (model: SelectModel<'a>) =
    match model.Options.Length > 0 with
    | true -> Some model.Options.[model.Selected]
    | false -> None

  /// Render the select. When open, shows all options with a `▸` cursor. When closed,
  /// shows the selected value with a `▾` arrow. Apply underline when focused.
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

/// Configuration for the ProgressBar widget.
type ProgressBarConfig = {
  /// Completion fraction, clamped to [0.0, 1.0].
  Percent: float
  /// Total width in characters.
  Width: int
  /// Character for filled portion (default `█`).
  FilledChar: char
  /// Character for empty portion (default `░`).
  EmptyChar: char
  /// Optional color for the filled segment.
  FilledColor: Color option
  /// Optional color for the empty segment.
  EmptyColor: Color option
  /// Whether to render the `%` label after the bar.
  ShowLabel: bool
}

module ProgressBar =
  /// Default config: 0%, width 20, block/shade chars, no colors, label shown.
  let defaults = {
    Percent = 0.0
    Width = 20
    FilledChar = '█'
    EmptyChar = '░'
    FilledColor = None
    EmptyColor = None
    ShowLabel = true
  }

  /// Render the progress bar from config. Start with `ProgressBar.defaults` and override fields.
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

/// Configuration for the Tabs widget.
type TabsConfig<'a> = {
  Items: 'a list
  ActiveIndex: int
  ToString: 'a -> string
  /// Color applied to the active tab label.
  ActiveColor: Color option
  /// Color applied to inactive tab labels.
  InactiveColor: Color option
}

module Tabs =
  /// Render a horizontal tab bar. Active tab is bold; colors are optional.
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

/// A single column definition for the Table widget.
type TableColumn<'a> = {
  /// Column header text.
  Header: string
  /// Fixed column width in characters.
  Width: int
  /// Render function for each data cell.
  Render: 'a -> Element
}

module Table =
  /// Render a table with a header row, a separator, and data rows.
  /// `selectedRow` highlights the row at that index with a blue background.
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

/// Configuration for the Modal overlay widget.
type ModalConfig = {
  /// Optional backdrop color (fill behind the modal). None = no backdrop.
  Backdrop: Color option
  /// Border style for the modal box.
  BorderStyle: BorderStyle
  /// Optional maximum width in characters.
  MaxWidth: int option
  /// Optional maximum height in lines.
  MaxHeight: int option
}

module Modal =
  /// Default config: black backdrop, Light border, no size constraints.
  let defaults = {
    Backdrop = Some (Color.Named(BaseColor.Black, Normal))
    BorderStyle = Light
    MaxWidth = None
    MaxHeight = None
  }

  /// Render the modal: content is centered, optionally constrained, with optional backdrop.
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

  /// Convenience shortcut: render a modal with default config (black backdrop, Light border).
  let simple (content: Element) =
    view defaults content

module Focus =
  /// Compute the next focused key given an ordered list, a current key, and a direction.
  /// Tab goes forward, Shift+Tab goes backward. Wraps around at either end.
  /// Note: `FocusSub` intercepts Tab/Shift+Tab before `KeySub`, so bind other keys with `Keys.bind`.
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

  /// Create a subscription that dispatches a message when Tab or Shift+Tab is pressed.
  /// Wire this to `Focus.tabOrder` in your update function to drive keyboard navigation.
  let focusSub (toMsg: FocusDirection -> 'msg) : Sub<'msg> =
    FocusSub (fun dir -> Some (toMsg dir))

module Checkbox =
  /// Toggle a boolean value. Convenience helper equivalent to `not value`.
  let toggle (value: bool) = not value

  /// Render a checkbox as `[✓]` or `[ ]` followed by the label.
  /// Bold when focused.
  let view (label: string) (focused: bool) (checked': bool) =
    let box = match checked' with true -> "[✓]" | false -> "[ ]"
    let el = El.text (sprintf "%s %s" box label)
    match focused with true -> el |> El.bold | false -> el

module Toggle =
  /// Toggle a boolean value. Convenience helper equivalent to `not value`.
  let toggle (value: bool) = not value

  /// Render a toggle as `● onLabel` when true or `○ offLabel` when false. Bold when focused.
  let view (onLabel: string) (offLabel: string) (focused: bool) (value: bool) =
    let display = match value with true -> sprintf "● %s" onLabel | false -> sprintf "○ %s" offLabel
    let el = El.text display
    match focused with true -> el |> El.bold | false -> el

/// State model for the RadioGroup widget.
type RadioGroupModel<'a> = {
  /// All available options.
  Options: 'a list
  /// Index of the currently selected option.
  Selected: int
}

module RadioGroup =
  /// Create a radio group with the first option selected.
  let create (options: 'a list) = { Options = options; Selected = 0 }

  /// Move selection one step up (clamped to 0).
  let moveUp (model: RadioGroupModel<'a>) =
    { model with Selected = max 0 (model.Selected - 1) }

  /// Move selection one step down (clamped to last option).
  let moveDown (model: RadioGroupModel<'a>) =
    { model with Selected = min (model.Options.Length - 1) (model.Selected + 1) }

  /// Return the currently selected value, or None if the options list is empty.
  let selectedValue (model: RadioGroupModel<'a>) =
    match model.Options.Length > 0 with
    | true -> Some model.Options.[model.Selected]
    | false -> None

  /// Handle Up/Down keys to move the selection. All other keys are no-ops.
  let handleKey (key: Key) (model: RadioGroupModel<'a>) =
    match key with
    | Key.Up -> moveUp model
    | Key.Down -> moveDown model
    | _ -> model

  /// Render all options as a column. Selected option gets `◉`, others get `○`.
  /// The selected row is bold when focused.
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

  /// Braille spinner (10 frames). Pass any incrementing int — e.g. `model.Tick % 10`.
  let view (tick: int) =
    El.text frames.[tick % frames.Length]

  /// Braille dot spinner (8 frames, rounder feel than `view`).
  let viewDots (tick: int) =
    El.text dots.[tick % dots.Length]

  /// Block-width pulse bar (12 frames).
  let viewBars (tick: int) =
    El.text bars.[tick % bars.Length]

module Toast =
  /// A temporary notification with a message, lifetime, and optional style.
  type ToastModel = {
    /// The message text to display.
    Message: string
    /// Number of timer ticks before expiry. Decrement with `Toast.tick` each update cycle.
    RemainingTicks: int
    /// Optional styling (foreground/background color, bold, etc.).
    Style: Style
  }

  /// Create a plain toast with default styling.
  let create (message: string) (ticks: int) =
    { Message = message; RemainingTicks = ticks; Style = Style.empty }

  /// Create a toast with explicit style (e.g. colored background for error/success toasts).
  let createStyled (message: string) (ticks: int) (style: Style) =
    { Message = message; RemainingTicks = ticks; Style = style }

  /// Decrement the tick counter. Returns `Some model` while active, `None` when expired.
  /// Call once per timer update in your `update` function.
  let tick (model: ToastModel) =
    match model.RemainingTicks > 0 with
    | true -> Some { model with RemainingTicks = model.RemainingTicks - 1 }
    | false -> None

  /// True when `RemainingTicks <= 0`. Equivalent to checking `Toast.tick` returning None.
  let isExpired (model: ToastModel) = model.RemainingTicks <= 0

  /// Render the toast as a horizontally padded, Light-bordered text box.
  let view (model: ToastModel) =
    El.text model.Message
    |> El.styled model.Style
    |> El.bordered Light
    |> El.padHV 1 0

/// A recursive tree node. `Leaf` holds a value; `Branch` holds a value and child nodes.
type TreeNode<'a> =
  | Leaf of 'a
  | Branch of 'a * children: TreeNode<'a> list

module TreeNode =
  /// Extract the value from either a `Leaf` or a `Branch`.
  let value node =
    match node with
    | Leaf v -> v
    | Branch(v, _) -> v

  /// Return the immediate children of a node. `Leaf` returns `[]`.
  let children node =
    match node with
    | Leaf _ -> []
    | Branch(_, cs) -> cs

  /// True if this node is a `Leaf` (no children).
  let isLeaf node =
    match node with
    | Leaf _ -> true
    | Branch _ -> false

/// Expansion and cursor state for a TreeView widget.
type TreeState = {
  /// Set of node paths (index lists from root) that are currently expanded.
  Expanded: Set<int list>
  /// Path of the currently focused node.
  Cursor: int list
}

module TreeView =
  /// Initial state: nothing expanded, cursor at first root node.
  let init () =
    { Expanded = Set.empty; Cursor = [0] }

  /// True if the node at `path` is currently expanded.
  let isExpanded (path: int list) (state: TreeState) =
    state.Expanded.Contains path

  /// Toggle expand/collapse for the node at `path`.
  let toggleExpand (path: int list) (state: TreeState) =
    match state.Expanded.Contains path with
    | true -> { state with Expanded = state.Expanded.Remove path }
    | false -> { state with Expanded = state.Expanded.Add path }

  /// Expand the node at `path` (no-op if already expanded).
  let expand (path: int list) (state: TreeState) =
    { state with Expanded = state.Expanded.Add path }

  /// Collapse the node at `path` (no-op if already collapsed).
  let collapse (path: int list) (state: TreeState) =
    { state with Expanded = state.Expanded.Remove path }

  /// Expand every branch in the tree at once.
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

  /// Return the list of visible node paths in display order (respects expanded state).
  let visiblePaths (nodes: TreeNode<'a> list) (state: TreeState) =
    flatten nodes state |> List.map (fun (p, _, _) -> p)

  /// Move the cursor one step up in the visible node list (clamped to top).
  let moveCursorUp (nodes: TreeNode<'a> list) (state: TreeState) =
    let visible = visiblePaths nodes state
    match List.tryFindIndex ((=) state.Cursor) visible with
    | Some idx when idx > 0 -> { state with Cursor = visible.[idx - 1] }
    | _ -> state

  /// Move the cursor one step down in the visible node list (clamped to bottom).
  let moveCursorDown (nodes: TreeNode<'a> list) (state: TreeState) =
    let visible = visiblePaths nodes state
    match List.tryFindIndex ((=) state.Cursor) visible with
    | Some idx when idx < visible.Length - 1 -> { state with Cursor = visible.[idx + 1] }
    | _ -> state

  /// Return the `TreeNode` at the current cursor position, or None if the tree is empty.
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

  /// Handle Up/Down/Left/Right/Enter keys for navigation and expand/collapse.
  /// Left on a collapsed node moves the cursor to the parent.
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

  /// Render the tree. Branches show `▾`/`▸` indicators; leaves are indented 2 spaces per depth.
  /// The cursor row is bold+reversed when focused.
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

/// A single field descriptor for the Form widget.
/// Binds a string key, a view function, and a key-handler function.
type FormField<'model, 'msg> = {
  /// Unique key used for focus routing.
  Key: string
  /// Render the field — receives `focused: bool` and the current model.
  View: bool -> 'model -> Element
  /// Handle a keypress while this field is focused. Return Some msg to dispatch, None to ignore.
  HandleKey: Key -> 'model -> 'msg option
}

module Form =
  /// Create a `FormField` from a key, view function, and key handler.
  let field (key: string) (view: bool -> 'model -> Element) (handleKey: Key -> 'model -> 'msg option) : FormField<'model, 'msg> =
    { Key = key; View = view; HandleKey = handleKey }

  /// Render all fields as a column, passing `focused = true` to the field whose key matches `focusedKey`.
  let view (fields: FormField<'model, 'msg> list) (focusedKey: string) (model: 'model) : Element =
    fields
    |> List.map (fun f -> f.View (f.Key = focusedKey) model)
    |> El.column

  /// Dispatch a keypress to the currently focused field. Returns None if no field handles the key.
  let handleKey (fields: FormField<'model, 'msg> list) (focusedKey: string) (key: Key) (model: 'model) : 'msg option =
    fields
    |> List.tryFind (fun f -> f.Key = focusedKey)
    |> Option.bind (fun f -> f.HandleKey key model)

  /// Extract all field keys in order. Pass to `Focus.tabOrder` for Tab navigation.
  let keys (fields: FormField<'model, 'msg> list) : string list =
    fields |> List.map (fun f -> f.Key)

  /// Move focus in the given direction through the form's field list. Convenience over `Focus.tabOrder (Form.keys fields)`.
  let handleFocus (fields: FormField<'model, 'msg> list) (focusedKey: string) (dir: FocusDirection) : string =
    Focus.tabOrder (keys fields) focusedKey dir
