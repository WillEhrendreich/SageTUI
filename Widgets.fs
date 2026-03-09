namespace SageTUI

open InputHelpers

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

  /// Apply theme colors and border style to an element tree
  let apply (theme: Theme) (elem: Element) : Element =
    // Note: theme.Border is intentionally not applied here — use Theme.panel or
    // El.bordered theme.Border explicitly. Theme.apply is a color-only wrapper.
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

/// Single-line text input widget with cursor movement, word navigation, and selection.
type TextInputModel = {
  /// The current text content.
  Text: string
  /// Zero-based cursor position (0 = before first char).
  Cursor: int
  /// Anchor position for the current selection. None = no active selection.
  /// The selected region is between SelectionAnchor and Cursor (in either order).
  SelectionAnchor: int option
}

/// Shared character-classification helper for word-movement in text widgets.
[<AutoOpen>]
module private TextWidgetHelpers =
  /// Returns true for letters, digits, and underscore — the word-character set
  /// shared by TextInput and TextEditor word-movement logic.
  let inline isWordChar (text: string) (i: int) : bool =
    text.[i] = '_' || System.Char.IsLetterOrDigit(text, i)

  /// Number of UTF-16 code units to step backward to reach the previous Rune boundary.
  /// Returns 2 when the char before `cursor` is the low surrogate of a pair, else 1.
  let inline leftStep (text: string) (cursor: int) : int =
    match cursor > 1 && System.Char.IsLowSurrogate(text.[cursor - 1]) with
    | true  -> 2
    | false -> 1

  /// Number of UTF-16 code units to step forward to reach the next Rune boundary.
  /// Returns 2 when the char at `cursor` is the high surrogate of a pair, else 1.
  let inline rightStep (text: string) (cursor: int) : int =
    match cursor + 1 < text.Length && System.Char.IsHighSurrogate(text.[cursor]) with
    | true  -> 2
    | false -> 1

module TextInput =
  /// Empty input with cursor at position 0.
  let empty = { Text = ""; Cursor = 0; SelectionAnchor = None }

  /// Create an input pre-filled with a string, cursor placed at the end.
  let ofString s = { Text = s; Cursor = String.length s; SelectionAnchor = None }

  /// Clamp cursor to [0, text.Length]. Use after any external mutation of the model.
  let clamp (model: TextInputModel) =
    { model with Cursor = max 0 (min model.Text.Length model.Cursor) }

  /// Replace the text content, clamping the cursor to the new text length.
  /// Use when you need to update text programmatically without losing cursor position.
  let setText (text: string) (model: TextInputModel) =
    { model with Text = text; Cursor = max 0 (min text.Length model.Cursor) }

  /// Insert a string at the current cursor position, advancing the cursor.
  let insertText (s: string) (model: TextInputModel) =
    let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
    let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
    { Text = before + s + after; Cursor = model.Cursor + s.Length; SelectionAnchor = None }

  /// Handle a key press: Char inserts, Backspace/Delete remove, arrows/Home/End move cursor.
  /// All mutations clear any active selection. Plain movement clears selection anchor.
  let handleKey (key: Key) (model: TextInputModel) =
    match key with
    | Key.Char r ->
      let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
      let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
      let s = r.ToString()
      { Text = before + s + after; Cursor = model.Cursor + s.Length; SelectionAnchor = None }
    | Key.Backspace ->
      match model.Cursor > 0 with
      | true ->
        let step = leftStep model.Text model.Cursor
        let before = match model.Cursor - step > 0 with true -> model.Text.[..model.Cursor - step - 1] | false -> ""
        let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
        { Text = before + after; Cursor = model.Cursor - step; SelectionAnchor = None }
      | false -> model
    | Key.Delete ->
      match model.Cursor < model.Text.Length with
      | true ->
        let step = rightStep model.Text model.Cursor
        let before = match model.Cursor > 0 with true -> model.Text.[..model.Cursor - 1] | false -> ""
        let after = match model.Cursor + step < model.Text.Length with true -> model.Text.[model.Cursor + step..] | false -> ""
        { Text = before + after; Cursor = model.Cursor; SelectionAnchor = None }
      | false -> model
    | Key.Left  -> { model with Cursor = max 0 (model.Cursor - leftStep model.Text model.Cursor); SelectionAnchor = None }
    | Key.Right -> { model with Cursor = min model.Text.Length (model.Cursor + rightStep model.Text model.Cursor); SelectionAnchor = None }
    | Key.Home  -> { model with Cursor = 0; SelectionAnchor = None }
    | Key.End   -> { model with Cursor = model.Text.Length; SelectionAnchor = None }
    | _ -> model

  // ── Word helpers ────────────────────────────────────────────────────────────

  /// Returns true if the character at index `i` in `text` is a word character.
  /// Word characters: letters, digits, and underscore.
  /// Uses the two-arg overload of Char.IsLetterOrDigit to handle surrogate pairs.
  // isWordChar is defined in TextWidgetHelpers (AutoOpen) above.

  /// Find the cursor position after jumping one word to the left (Ctrl+Left).
  let wordLeftPos (pos: int) (text: string) : int =
    let rec skipNonWord i =
      match i > 0 && not (isWordChar text (i - 1)) with
      | true -> skipNonWord (i - 1)
      | false -> i
    let rec skipWord i =
      match i > 0 && isWordChar text (i - 1) with
      | true -> skipWord (i - 1)
      | false -> i
    pos |> skipNonWord |> skipWord

  /// Find the cursor position after jumping one word to the right (Ctrl+Right).
  let wordRightPos (pos: int) (text: string) : int =
    let len = text.Length
    let rec skipNonWord i =
      match i < len && not (isWordChar text i) with
      | true -> skipNonWord (i + 1)
      | false -> i
    let rec skipWord i =
      match i < len && isWordChar text i with
      | true -> skipWord (i + 1)
      | false -> i
    pos |> skipNonWord |> skipWord

  /// Move cursor one word to the left, clearing selection anchor.
  let wordLeft (model: TextInputModel) =
    { model with Cursor = wordLeftPos model.Cursor model.Text; SelectionAnchor = None }

  /// Move cursor one word to the right, clearing selection anchor.
  let wordRight (model: TextInputModel) =
    { model with Cursor = wordRightPos model.Cursor model.Text; SelectionAnchor = None }

  // ── Selection helpers ────────────────────────────────────────────────────────

  /// The (start, end) of the current selection, or None if no selection.
  let selectionRange (model: TextInputModel) =
    match model.SelectionAnchor with
    | None -> None
    | Some anchor ->
      let lo = min anchor model.Cursor
      let hi = max anchor model.Cursor
      match lo = hi with
      | true -> None
      | false -> Some (lo, hi)

  /// True when there is a non-empty selection.
  let hasSelection (model: TextInputModel) =
    selectionRange model |> Option.isSome

  /// Select all text (anchor = 0, cursor = end).
  let selectAll (model: TextInputModel) =
    { model with SelectionAnchor = Some 0; Cursor = model.Text.Length }

  /// Extend selection one character to the left (Shift+Left).
  let selectLeft (model: TextInputModel) =
    let anchor = match model.SelectionAnchor with Some a -> a | None -> model.Cursor
    { model with Cursor = max 0 (model.Cursor - leftStep model.Text model.Cursor); SelectionAnchor = Some anchor }

  /// Extend selection one character to the right (Shift+Right).
  let selectRight (model: TextInputModel) =
    let anchor = match model.SelectionAnchor with Some a -> a | None -> model.Cursor
    { model with Cursor = min model.Text.Length (model.Cursor + rightStep model.Text model.Cursor); SelectionAnchor = Some anchor }

  /// Extend selection one word to the left (Shift+Ctrl+Left).
  let selectWordLeft (model: TextInputModel) =
    let anchor = match model.SelectionAnchor with Some a -> a | None -> model.Cursor
    { model with Cursor = wordLeftPos model.Cursor model.Text; SelectionAnchor = Some anchor }

  /// Extend selection one word to the right (Shift+Ctrl+Right).
  let selectWordRight (model: TextInputModel) =
    let anchor = match model.SelectionAnchor with Some a -> a | None -> model.Cursor
    { model with Cursor = wordRightPos model.Cursor model.Text; SelectionAnchor = Some anchor }

  /// Delete the selected region, placing cursor at the selection start.
  /// If no selection, this is a no-op.
  let deleteSelection (model: TextInputModel) =
    match selectionRange model with
    | None -> model
    | Some (lo, hi) ->
      let before = match lo > 0 with true -> model.Text.[..lo - 1] | false -> ""
      let after = match hi < model.Text.Length with true -> model.Text.[hi..] | false -> ""
      { model with Text = before + after; Cursor = lo; SelectionAnchor = None }

  /// If there is an active selection, delete it; otherwise apply the normal key action.
  /// Use this for Backspace/Delete/Char keys when selection awareness is needed.
  let handleKeyWithSelection (key: Key) (model: TextInputModel) =
    match hasSelection model with
    | true ->
      match key with
      | Key.Char _ | Key.Backspace | Key.Delete ->
        let afterDel = deleteSelection model
        match key with
        | Key.Char c -> handleKey (Key.Char c) afterDel
        | _ -> afterDel
      | _ -> handleKey key model
    | false -> handleKey key model

  /// Delete from cursor to start of previous word (Ctrl+Backspace).
  let deleteWordLeft (model: TextInputModel) =
    let newCursor = wordLeftPos model.Cursor model.Text
    match newCursor = model.Cursor with
    | true -> model
    | false ->
      let before = match newCursor > 0 with true -> model.Text.[..newCursor - 1] | false -> ""
      let after = match model.Cursor < model.Text.Length with true -> model.Text.[model.Cursor..] | false -> ""
      { model with Text = before + after; Cursor = newCursor; SelectionAnchor = None }

  /// Handle a paste event: strips newlines (replaced with spaces) then inserts at cursor.
  /// If there is an active selection, it is replaced by the pasted content.
  let handlePaste (text: string) (model: TextInputModel) =
    let clean = text.Replace("\r\n", " ").Replace("\n", " ").Replace("\r", " ")
    let afterDel = match hasSelection model with true -> deleteSelection model | false -> model
    insertText clean afterDel

  /// Handle a full TerminalEvent (key + modifiers) for rich editing support.
  /// Handles Ctrl+Left/Right (word jump), Shift+Left/Right/Ctrl+Left/Right (extend selection),
  /// Ctrl+A (select all), Ctrl+Backspace (delete word left).
  /// Falls back to handleKeyWithSelection for all other keys.
  let handleEvent (event: TerminalEvent) (model: TextInputModel) =
    match event with
    | KeyPressed(Key.Left,  m) when m.HasFlag(Modifiers.Ctrl) && m.HasFlag(Modifiers.Shift) -> selectWordLeft model
    | KeyPressed(Key.Right, m) when m.HasFlag(Modifiers.Ctrl) && m.HasFlag(Modifiers.Shift) -> selectWordRight model
    | KeyPressed(Key.Left,  m) when m.HasFlag(Modifiers.Ctrl)  -> wordLeft model
    | KeyPressed(Key.Right, m) when m.HasFlag(Modifiers.Ctrl)  -> wordRight model
    | KeyPressed(Key.Left,  m) when m.HasFlag(Modifiers.Shift) -> selectLeft model
    | KeyPressed(Key.Right, m) when m.HasFlag(Modifiers.Shift) -> selectRight model
    | KeyPressed(Key.Home,  m) when m.HasFlag(Modifiers.Shift) ->
      { model with Cursor = 0; SelectionAnchor = Some (match model.SelectionAnchor with Some a -> a | None -> model.Cursor) }
    | KeyPressed(Key.End,   m) when m.HasFlag(Modifiers.Shift) ->
      { model with Cursor = model.Text.Length; SelectionAnchor = Some (match model.SelectionAnchor with Some a -> a | None -> model.Cursor) }
    | KeyPressed(KeyChar 'a', m) when m.HasFlag(Modifiers.Ctrl) -> selectAll model
    | KeyPressed(Key.Backspace, m) when m.HasFlag(Modifiers.Ctrl) -> deleteWordLeft model
    | KeyPressed(key, _) -> handleKeyWithSelection key model
    | Pasted text -> handlePaste text model
    | _ -> model

  /// Render the text input with visual cursor (reversed char) and selection highlight.
  /// When unfocused, renders plain text or placeholder. Empty unfocused renders as a space.
  let view (focused: bool) (model: TextInputModel) =
    let text = model.Text
    match focused with
    | false ->
      match text.Length = 0 with
      | true -> El.text " "
      | false -> El.text text
    | true ->
      match selectionRange model with
      | Some (lo, hi) ->
        // Active selection: before | selected (highlighted) | after
        let before  = match lo > 0 with true -> text.[..lo - 1] | false -> ""
        let sel     = text.[lo..hi - 1]
        let after   = match hi < text.Length with true -> text.[hi..] | false -> ""
        El.row [
          if before <> "" then yield El.text before
          yield El.text sel |> El.reverse
          if after  <> "" then yield El.text after ]
      | None ->
        // No selection: show cursor as reversed char at cursor position
        let cur  = match model.Cursor < text.Length with true -> string text.[model.Cursor] | false -> " "
        let pre  = match model.Cursor > 0 with true -> text.[..model.Cursor - 1] | false -> ""
        let post = match model.Cursor + 1 < text.Length with true -> text.[model.Cursor + 1..] | false -> ""
        El.row [
          if pre  <> "" then yield El.text pre
          yield El.text cur |> El.reverse
          if post <> "" then yield El.text post ]

  /// Render with a dimmed placeholder shown when the input is empty.
  /// When focused and empty, a reversed space acts as the cursor placeholder.
  let viewWithPlaceholder (placeholder: string) (focused: bool) (model: TextInputModel) =
    match model.Text.Length = 0 with
    | true ->
      match focused with
      | true ->
        El.row [ El.text placeholder |> El.dim; El.text " " |> El.reverse ]
      | false -> El.text placeholder |> El.dim
    | false ->
      view focused model

  /// Render the text input with cursor and selection colors from a `Theme`.
  /// When focused, cursor is painted with `theme.Primary`; selected text with `theme.Accent`.
  /// Delegates to the standard view for all layout logic — only color is affected.
  let viewThemed (theme: Theme) (focused: bool) (model: TextInputModel) : Element =
    match focused with
    | false -> view false model
    | true  ->
      let text = model.Text
      match selectionRange model with
      | Some (lo, hi) ->
        let before = match lo > 0              with true -> text.[..lo - 1]       | false -> ""
        let sel    = text.[lo..hi - 1]
        let after  = match hi < text.Length    with true -> text.[hi..]           | false -> ""
        El.row [
          if before <> "" then yield El.text before
          yield El.text sel |> El.fg theme.Accent |> El.reverse
          if after  <> "" then yield El.text after ]
      | None ->
        let cur  = match model.Cursor < text.Length with true -> string text.[model.Cursor] | false -> " "
        let pre  = match model.Cursor > 0            with true -> text.[..model.Cursor - 1] | false -> ""
        let post = match model.Cursor + 1 < text.Length with true -> text.[model.Cursor + 1..] | false -> ""
        El.row [
          if pre  <> "" then yield El.text pre
          yield El.text cur |> El.fg theme.Primary |> El.reverse
          if post <> "" then yield El.text post ]

  /// Render with a themed placeholder and themed cursor/selection.
  let viewWithPlaceholderThemed (theme: Theme) (placeholder: string) (focused: bool) (model: TextInputModel) : Element =
    match model.Text.Length = 0 with
    | true ->
      match focused with
      | true  -> El.row [ El.text placeholder |> El.fg theme.TextDim; El.text " " |> El.fg theme.Primary |> El.reverse ]
      | false -> El.text placeholder |> El.fg theme.TextDim
    | false ->
      viewThemed theme focused model

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
  /// Note: requires structural equality on 'a. For [<NoEquality>] types (e.g. Element),
  /// use isFocusedAt instead.
  let isFocused (item: 'a) (ring: FocusRing<'a>) =
    match current ring with
    | Some c -> c = item
    | None -> false

  /// Return true if the item at the given 0-based index is currently focused.
  /// Safe for any type, including [<NoEquality>] types like Element.
  let isFocusedAt (index: int) (ring: FocusRing<'a>) =
    ring.Items.Length > 0 && ring.Index = index

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
    let barEl =
      match filled, empty with
      | 0, _ -> El.text (System.String(config.EmptyChar,  empty))  |> El.fgOpt config.EmptyColor
      | _, 0 -> El.text (System.String(config.FilledChar, filled)) |> El.fgOpt config.FilledColor
      | _ ->
        match config.FilledColor, config.EmptyColor with
        | None, None ->
          // No colors: single text element flex-distributes naturally, matching blank-cell overflow.
          El.text (System.String(config.FilledChar, filled) + System.String(config.EmptyChar, empty))
        | _ ->
          // Colored segments: pin width so flex distribution doesn't inject gaps between segments.
          El.width config.Width <|
            El.row [
              El.text (System.String(config.FilledChar, filled)) |> El.fgOpt config.FilledColor
              El.text (System.String(config.EmptyChar,  empty))  |> El.fgOpt config.EmptyColor
            ]
    match config.ShowLabel with
    | true ->
      let label = sprintf " %d%%" (int (pct * 100.0))
      El.row [ barEl; El.text label ]
    | false -> barEl

  /// Apply a `Theme` to a `ProgressBarConfig` — sets `FilledColor` to `theme.Primary`
  /// and `EmptyColor` to `theme.TextDim`. All other config fields are preserved.
  let withTheme (theme: Theme) (config: ProgressBarConfig) : ProgressBarConfig =
    { config with
        FilledColor = Some theme.Primary
        EmptyColor  = Some theme.TextDim }

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

  /// Apply a `Theme` to a `TabsConfig` — sets `ActiveColor` to `theme.Primary`
  /// and `InactiveColor` to `theme.TextDim`. All other fields are preserved.
  let withTheme (theme: Theme) (config: TabsConfig<'a>) : TabsConfig<'a> =
    { config with
        ActiveColor   = Some theme.Primary
        InactiveColor = Some theme.TextDim }
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
  ///
  /// NOTE: This renders all rows. For large datasets use VirtualList or VirtualTable.
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

// ── VirtualList ───────────────────────────────────────────────────────────────
// A stateful virtualizing list widget. Only renders the visible window of rows,
// enabling efficient display of thousands of items.

/// The model for a VirtualList. Tracks items, selection, scroll position, and viewport height.
type VirtualListModel<'row> = {
  /// All items in the list.
  Items: 'row array
  /// Currently selected item index (into Items). None if list is empty.
  SelectedIndex: int option
  /// Index of the first visible item (scroll offset).
  ScrollOffset: int
  /// Number of rows visible at once. Stored here so navigation is self-contained.
  ViewportHeight: int
}

/// Configuration for rendering a VirtualList.
type VirtualListConfig<'row> = {
  /// Color to highlight the selected row background. Default: Blue.
  SelectionColor: Color
  /// Render function for each row. The bool is true when the row is selected.
  RenderRow: bool -> 'row -> Element
  /// When true, renders a scrollbar column to the right of the list. Default: false.
  ShowScrollbar: bool
}

module VirtualList =
  /// Create a config with Blue selection and a typed render function.
  /// The render function receives `selected: bool` allowing custom selection styling.
  let create (renderRow: bool -> 'row -> Element) : VirtualListConfig<'row> =
    { SelectionColor = Color.Named(BaseColor.Blue, Intensity.Normal)
      RenderRow = renderRow
      ShowScrollbar = false }

  /// Apply a `Theme` to a `VirtualListConfig` — sets `SelectionColor` to `theme.Primary`.
  /// All other config fields are preserved.
  let withTheme (theme: Theme) (config: VirtualListConfig<'row>) : VirtualListConfig<'row> =
    { config with SelectionColor = theme.Primary }

  /// Create a VirtualList model from an array of items, with nothing selected.
  let ofArray (viewportHeight: int) (items: 'row array) : VirtualListModel<'row> =
    let sel = match items.Length with 0 -> None | _ -> Some 0
    { Items = items; SelectedIndex = sel; ScrollOffset = 0; ViewportHeight = max 1 viewportHeight }

  /// Create a VirtualList model from a list of items.
  let ofList (viewportHeight: int) (items: 'row list) : VirtualListModel<'row> =
    ofArray viewportHeight (List.toArray items)

  /// Ensure the selected row is visible within the model's viewport.
  let ensureVisible (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.SelectedIndex with
    | None -> m
    | Some i ->
      let offset =
        match i < m.ScrollOffset with
        | true -> i
        | false ->
          match i >= m.ScrollOffset + m.ViewportHeight with
          | true -> i - m.ViewportHeight + 1
          | false -> m.ScrollOffset
      { m with ScrollOffset = max 0 offset }

  /// Move selection up by one row, keeping selection visible.
  let selectPrev (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.SelectedIndex with
    | None -> m
    | Some i -> { m with SelectedIndex = Some (max 0 (i - 1)) } |> ensureVisible

  /// Move selection down by one row, keeping selection visible.
  let selectNext (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.SelectedIndex with
    | None -> m
    | Some i ->
      let next = min (m.Items.Length - 1) (i + 1)
      { m with SelectedIndex = Some next } |> ensureVisible

  /// Jump selection up by a page (ViewportHeight rows), keeping selection visible.
  let pageUp (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.SelectedIndex with
    | None -> m
    | Some i -> { m with SelectedIndex = Some (max 0 (i - m.ViewportHeight)) } |> ensureVisible

  /// Jump selection down by a page, keeping selection visible.
  let pageDown (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.SelectedIndex with
    | None -> m
    | Some i ->
      let next = min (m.Items.Length - 1) (i + m.ViewportHeight)
      { m with SelectedIndex = Some next } |> ensureVisible

  /// Jump to the first item.
  let selectFirst (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.Items.Length with
    | 0 -> m
    | _ -> { m with SelectedIndex = Some 0 } |> ensureVisible

  /// Jump to the last item.
  let selectLast (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match m.Items.Length with
    | 0 -> m
    | len -> { m with SelectedIndex = Some (len - 1) } |> ensureVisible

  /// Replace the items list, clamping selection and ensuring it remains visible.
  let setItems (items: 'row array) (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    let sel =
      match items.Length with
      | 0 -> None
      | len ->
        match m.SelectedIndex with
        | None -> Some 0
        | Some i -> Some (min (len - 1) i)
    { m with Items = items; SelectedIndex = sel } |> ensureVisible

  /// Get the currently selected item, if any.
  let selectedItem (m: VirtualListModel<'row>) : 'row option =
    match m.SelectedIndex with
    | None -> None
    | Some i ->
      match i >= 0 && i < m.Items.Length with
      | true -> Some m.Items[i]
      | false -> None

  /// Update the ViewportHeight, clamping to at least 1. Calls ensureVisible to keep selection on screen.
  let resize (newHeight: int) (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    { m with ViewportHeight = max 1 newHeight } |> ensureVisible

  // Render a scrollbar column for the given scroll state.
  let private renderScrollbar (total: int) (vh: int) (offset: int) : Element =
    if total <= vh then
      El.column [ for _ in 0 .. vh - 1 -> El.text " " ]
    else
      let thumbHeight = max 1 (vh * vh / total)
      let maxOffset = total - vh
      let thumbTop = (offset * (vh - thumbHeight)) / (max 1 maxOffset)
      El.column [
        for i in 0 .. vh - 1 ->
          match i >= thumbTop && i < thumbTop + thumbHeight with
          | true  -> El.text "▓"
          | false -> El.text "│" ]

  /// Render only the visible window of items, padded to ViewportHeight with empty rows.
  /// Always produces exactly ViewportHeight rows to prevent layout shifts.
  /// The RenderRow function receives the selection state as a bool.
  let view (config: VirtualListConfig<'row>) (m: VirtualListModel<'row>) : Element =
    let total = m.Items.Length
    let vh = max 1 m.ViewportHeight
    let listContent =
      match total with
      | 0 ->
        El.column [ for _ in 1 .. vh -> El.empty ]
      | _ ->
        let offset = max 0 (min m.ScrollOffset (total - 1))
        let visibleCount = min vh (total - offset)
        let rows =
          [ for vi in 0 .. visibleCount - 1 do
              let i = offset + vi
              let selected = m.SelectedIndex = Some i
              yield config.RenderRow selected m.Items[i]
            for _ in visibleCount .. vh - 1 do
              yield El.empty ]
        El.column rows
    match config.ShowScrollbar with
    | false -> listContent
    | true  ->
      let offset = max 0 m.ScrollOffset
      El.row [ listContent; renderScrollbar total vh offset ]

  /// Handle keyboard events for navigation: Up/Down/PageUp/PageDown/Home/End.
  /// Returns the updated model, or the same model for unhandled events.
  let handleEvent (event: TerminalEvent) (m: VirtualListModel<'row>) : VirtualListModel<'row> =
    match event with
    | KeyPressed(Key.Up,       _) -> selectPrev m
    | KeyPressed(Key.Down,     _) -> selectNext m
    | KeyPressed(Key.PageUp,   _) -> pageUp m
    | KeyPressed(Key.PageDown, _) -> pageDown m
    | KeyPressed(Key.Home,     _) -> selectFirst m
    | KeyPressed(Key.End,      _) -> selectLast m
    | _ -> m

// ── VirtualTable ─────────────────────────────────────────────────────────────
// Combines the column-header/separator structure of Table with VirtualList's
// efficient windowed rendering. Use for large datasets in a table layout.

/// Configuration for VirtualTable rendering.
type VirtualTableConfig<'a> = {
  /// Column definitions (header, width, render function).
  Columns: TableColumn<'a> list
  /// Color for the selected row. Default: Blue.
  SelectionColor: Color
  /// Border style character for the header separator.
  SeparatorChar: char
  /// Show a proportional scrollbar on the right side of the data area. Default: false.
  ShowScrollbar: bool
}

module VirtualTable =
  /// Default config: Blue selection, standard horizontal separator, no scrollbar.
  let create (columns: TableColumn<'a> list) : VirtualTableConfig<'a> =
    { Columns = columns
      SelectionColor = Color.Named(BaseColor.Blue, Intensity.Normal)
      SeparatorChar = '─'
      ShowScrollbar = false }

  /// Render a virtualised table: fixed header + separator + windowed data rows.
  /// The model's ViewportHeight controls how many data rows are rendered.
  let view (config: VirtualTableConfig<'a>) (model: VirtualListModel<'a>) : Element =
    let header =
      config.Columns
      |> List.map (fun col -> El.text col.Header |> El.bold |> El.width col.Width)
      |> El.row
    let separator =
      config.Columns
      |> List.map (fun col ->
        match col.Width with
        | 0 -> El.empty
        | w -> El.text (System.String(config.SeparatorChar, w)) |> El.width w)
      |> El.row
    let listCfg : VirtualListConfig<'a> = {
      SelectionColor = config.SelectionColor
      ShowScrollbar = config.ShowScrollbar
      RenderRow = fun selected row ->
        let cols =
          config.Columns
          |> List.map (fun col -> col.Render row |> El.width col.Width)
          |> El.row
        match selected with
        | true  -> El.bg config.SelectionColor cols
        | false -> cols
    }
    let dataRows = VirtualList.view listCfg model
    El.column [ header; separator; dataRows ]

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
  /// Works with any equality-comparable key type, including `FieldId` and `string`.
  /// Note: `FocusSub` intercepts Tab/Shift+Tab before `KeySub`, so bind other keys with `Keys.bind`.
  let tabOrder (keys: 'a list) (focused: 'a) (dir: FocusDirection) : 'a =
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

  /// Render a themed checkbox. The box character is colored using `theme.Accent` when
  /// checked, `theme.TextDim` when unchecked. Label uses `theme.TextFg`. Bold when focused.
  let viewThemed (theme: Theme) (label: string) (focused: bool) (checked': bool) : Element =
    let boxStr = match checked' with true -> "[✓]" | false -> "[ ]"
    let boxColor = match checked' with true -> theme.Accent | false -> theme.TextDim
    let boxEl  = El.text boxStr |> El.fg boxColor
    let lblEl  = El.text (sprintf " %s" label) |> El.fg theme.TextFg
    let el = El.row [ boxEl; lblEl ]
    match focused with true -> el |> El.bold | false -> el

module Toggle =
  /// Toggle a boolean value. Convenience helper equivalent to `not value`.
  let toggle (value: bool) = not value

  /// Render a toggle as `● onLabel` when true or `○ offLabel` when false. Bold when focused.
  let view (onLabel: string) (offLabel: string) (focused: bool) (value: bool) =
    let display = match value with true -> sprintf "● %s" onLabel | false -> sprintf "○ %s" offLabel
    let el = El.text display
    match focused with true -> el |> El.bold | false -> el

  /// Render a themed toggle. `●` (on) uses `theme.Success`; `○` (off) uses `theme.TextDim`. Bold when focused.
  let viewThemed (theme: Theme) (onLabel: string) (offLabel: string) (focused: bool) (value: bool) : Element =
    let symbol, color, lbl =
      match value with
      | true  -> "●", theme.Success, onLabel
      | false -> "○", theme.TextDim, offLabel
    let el = El.row [ El.text symbol |> El.fg color; El.text (sprintf " %s" lbl) |> El.fg theme.TextFg ]
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

/// Type-safe field identifier. Prevents mixing up field IDs with arbitrary strings.
/// Use `FieldId "myField"` at the definition site; compare with structural equality.
/// NOTE: Prefer `FieldId.create` over the `FieldId` constructor directly when the value
/// originates from external input — `create` validates against null.
type FieldId = FieldId of string

module FieldId =
  /// Construct a `FieldId` from a raw string.
  /// Raises <see cref="System.ArgumentException"/> (via <c>invalidArg</c>) for null input.
  /// Prefer this over the raw <c>FieldId</c> constructor when the value originates from external input.
  let create (s: string) =
    if isNull s then invalidArg (nameof s) "FieldId cannot wrap null"
    FieldId s
  /// Extract the raw string value from a `FieldId`.
  let value (FieldId s) = s

/// A single field descriptor for the Form widget.
/// Binds a `FieldId`, a view function, and a terminal-event handler.
type FormField<'model, 'msg> = {
  /// Unique identifier used for focus routing. Use `FieldId "name"` at definition.
  Id: FieldId
  /// Render the field — receives `focused: bool` and the current model.
  View: bool -> 'model -> Element
  /// Handle a terminal event while this field is focused. Return Some msg to dispatch, None to ignore.
  HandleEvent: TerminalEvent -> 'model -> 'msg option
}

module Form =
  /// Create a `FormField` from a `FieldId`, view function, and terminal event handler.
  /// Use this to support modifier keys (Ctrl, Shift) inside form fields.
  let field (id: FieldId) (view: bool -> 'model -> Element) (handleEvent: TerminalEvent -> 'model -> 'msg option) : FormField<'model, 'msg> =
    { Id = id; View = view; HandleEvent = handleEvent }

  /// Create a `FormField` that only handles plain keypresses (no modifiers).
  /// Wraps the key-only handler into a TerminalEvent handler.
  [<System.Obsolete("Use Form.field with a TerminalEvent handler to support modifier keys (Ctrl, Shift, etc.). Will be removed in v1.0.")>]
  let fieldFromKey (id: FieldId) (view: bool -> 'model -> Element) (handleKey: Key -> 'model -> 'msg option) : FormField<'model, 'msg> =
    // Match only unmodified keypresses — Ctrl/Shift combos are NOT forwarded to the legacy handler.
    { Id = id; View = view; HandleEvent = fun evt model -> match evt with KeyPressed(k, Modifiers.None) -> handleKey k model | _ -> None }

  /// Render all fields as a column, passing `focused = true` to the field whose id matches `focusedId`.
  let view (fields: FormField<'model, 'msg> list) (focusedId: FieldId) (model: 'model) : Element =
    fields
    |> List.map (fun f -> f.View (f.Id = focusedId) model)
    |> El.column

  /// Dispatch a terminal event to the currently focused field. Returns None if no field handles it.
  let handleEvent (fields: FormField<'model, 'msg> list) (focusedId: FieldId) (event: TerminalEvent) (model: 'model) : 'msg option =
    fields
    |> List.tryFind (fun f -> f.Id = focusedId)
    |> Option.bind (fun f -> f.HandleEvent event model)

  /// Dispatch a plain keypress to the currently focused field. Returns None if no field handles the key.
  /// Prefer `Form.handleEvent` to support modifier keys.
  [<System.Obsolete("Use Form.handleEvent to support modifier keys (Ctrl, Shift, etc.). Will be removed in v1.0.")>]
  let handleKey (fields: FormField<'model, 'msg> list) (focusedId: FieldId) (key: Key) (model: 'model) : 'msg option =
    handleEvent fields focusedId (KeyPressed(key, Modifiers.None)) model

  /// Extract all field identifiers in order. Pass to `Focus.tabOrder` for Tab navigation.
  let ids (fields: FormField<'model, 'msg> list) : FieldId list =
    fields |> List.map (fun f -> f.Id)

  /// Move focus in the given direction through the form's field list. Convenience over `Focus.tabOrder (Form.ids fields)`.
  let handleFocus (fields: FormField<'model, 'msg> list) (focusedId: FieldId) (dir: FocusDirection) : FieldId =
    Focus.tabOrder (ids fields) focusedId dir

// ---- TextForm: batteries-included form with text inputs, labels, and validation ----

/// A single field descriptor: static configuration for one `TextForm` text input field.
type TextFormField = {
  Id: FieldId
  Label: string
  Placeholder: string
  Required: bool
  /// Returns `Ok trimmedValue` on success, `Error message` on failure.
  Validator: string -> Result<string, string>
}

/// Runtime state for one field in a `TextFormModel`.
type TextFormFieldState = {
  Field: TextFormField
  Input: TextInputModel
  Error: string option
  /// True once the field has been tabbed away from at least once — enables live validation.
  Touched: bool
}

/// Overall submission lifecycle for a `TextFormModel`.
type TextFormStatus =
  | TFEditing
  | TFSubmitting
  | TFSubmitted
  /// Whole-form failure with an optional error message.
  | TFSubmitFailed of string
  /// Field-level errors returned from the server, keyed by field ID.
  /// The view renders each error beneath the matching field.
  | TFFieldErrors of Map<FieldId, string>

/// The full TextForm model: an ordered list of field states and focus/status bookkeeping.
type TextFormModel = {
  Rows: TextFormFieldState list
  FocusIndex: int
  Status: TextFormStatus
}

type TextFormMsg =
  | TFKey        of Key
  | TFEvent      of TerminalEvent
  | TFTabNext
  | TFTabPrev
  | TFSubmit
  | TFSubmitResult of Result<unit, string>
  /// Set server-side per-field errors (field ID → error message).
  /// Transitions status to TFFieldErrors and resumes editing.
  | TFSetFieldErrors of Map<FieldId, string>
  | TFReset

module TextForm =
  let private defaultValidator (required: bool) (value: string) : Result<string, string> =
    match required && value.Trim() = "" with
    | true  -> Error "Required"
    | false -> Ok (value.Trim())

  /// Create a `TextFormField` with sensible defaults (not required, passes everything).
  let field (id: FieldId) (label: string) : TextFormField =
    { Id = id
      Label = label
      Placeholder = ""
      Required = false
      Validator = defaultValidator false }

  /// Mark a field as required (empty value fails validation).
  let required   (f: TextFormField) = { f with Required = true; Validator = defaultValidator true }
  /// Set placeholder text shown when the field is empty.
  let placeholder (p: string) (f: TextFormField) = { f with Placeholder = p }
  /// Override with a custom validation function.
  let validate    (v: string -> Result<string, string>) (f: TextFormField) = { f with Validator = v }

  /// Initialize a form from a list of field descriptors. Focus starts at index 0.
  let init (fields: TextFormField list) : TextFormModel =
    { Rows = fields |> List.map (fun f ->
        { Field = f; Input = TextInput.empty; Error = None; Touched = false })
      FocusIndex = 0
      Status = TFEditing }

  let private runValidation (row: TextFormFieldState) =
    match row.Field.Validator row.Input.Text with
    | Ok _    -> { row with Error = None }
    | Error e -> { row with Error = Some e }

  let private allValid (rows: TextFormFieldState list) =
    rows |> List.forall (fun r ->
      match r.Field.Validator r.Input.Text with
      | Ok _ -> true
      | Error _ -> false)

  let private blurCurrent (m: TextFormModel) =
    m.Rows |> List.mapi (fun i r ->
      match i = m.FocusIndex with
      | false -> r
      | true  -> runValidation { r with Touched = true })

  /// Apply an update function to the currently focused field, running validation if the field is touched.
  /// In TFFieldErrors state, editing a field also clears that field's server error (user is fixing it).
  let private applyToFocused (updateInput: TextInputModel -> TextInputModel) (m: TextFormModel) =
    let idx = m.FocusIndex
    let rows =
      m.Rows |> List.mapi (fun i r ->
        match i = idx with
        | false -> r
        | true  ->
          let updated = { r with Input = updateInput r.Input }
          match r.Touched with
          | true  -> runValidation updated
          | false -> updated)
    // If in TFFieldErrors, remove the focused field's server error from status on edit.
    let newStatus =
      match m.Status with
      | TFFieldErrors errs ->
        let focusedFieldId = m.Rows.[idx].Field.Id
        let remaining = errs |> Map.remove focusedFieldId
        match remaining.IsEmpty with
        | true  -> TFEditing
        | false -> TFFieldErrors remaining
      | s -> s
    { m with Rows = rows; Status = newStatus }

  /// Update the form. Returns `(newModel, submitRequested)`.
  /// When `submitRequested = true`, fire your submit `Cmd` and then dispatch `TFSubmitResult`.
  let update (msg: TextFormMsg) (m: TextFormModel) : TextFormModel * bool =
    match msg with
    | TFReset ->
        init (m.Rows |> List.map (fun r -> r.Field)), false

    | TFSubmitResult (Ok _) ->
        { m with Status = TFSubmitted }, false

    | TFSubmitResult (Error e) ->
        { m with Status = TFSubmitFailed e }, false

    | TFSetFieldErrors errors ->
        // Clear all row-level client errors when server errors arrive — the form passed
        // client-side validation before submitting, so client errors are now stale.
        // This prevents dual error messages (client + server) on the same field after blur.
        let clearedRows = m.Rows |> List.map (fun r -> { r with Error = None })
        { m with Status = TFFieldErrors errors; Rows = clearedRows }, false

    | _ when m.Status = TFSubmitting || m.Status = TFSubmitted ->
        m, false

    | TFKey key ->
        applyToFocused (TextInput.handleKey key) m, false

    | TFEvent event ->
        applyToFocused (TextInput.handleEvent event) m, false

    | TFTabNext ->
        let blurred = blurCurrent m
        let next = (m.FocusIndex + 1) % m.Rows.Length
        { m with Rows = blurred; FocusIndex = next }, false

    | TFTabPrev ->
        let blurred = blurCurrent m
        let prev = (m.FocusIndex - 1 + m.Rows.Length) % m.Rows.Length
        { m with Rows = blurred; FocusIndex = prev }, false

    | TFSubmit ->
        let validated = m.Rows |> List.map (fun r -> runValidation { r with Touched = true })
        match allValid validated with
        | false -> { m with Rows = validated }, false
        | true  -> { m with Rows = validated; Status = TFSubmitting }, true

  /// Get the current (untrimmed) text value of a field by ID.
  let getValue (fieldId: FieldId) (m: TextFormModel) : string option =
    m.Rows |> List.tryFind (fun r -> r.Field.Id = fieldId)
    |> Option.map (fun r -> r.Input.Text)

  /// True when all fields currently pass validation.
  let isValid (m: TextFormModel) = allValid m.Rows

  /// Render the form as a column. Each field shows a focus indicator, label, text input,
  /// and an optional error line beneath it.
  let view (focused: bool) (m: TextFormModel) : Element =
    let labelWidth =
      match m.Rows |> List.map (fun r -> r.Field.Label.Length) with
      | [] -> 10
      | lens -> List.max lens

    let statusBanner =
      match m.Status with
      | TFSubmitting     -> El.text " Submitting…"  |> El.fg (Named(Yellow, Bright)) |> Some
      | TFSubmitted      -> El.text " ✓ Submitted"   |> El.fg (Named(Green, Bright))  |> Some
      | TFSubmitFailed e -> El.text (sprintf " ✗ %s" e) |> El.fg (Named(Red, Bright)) |> Some
      | TFEditing | TFFieldErrors _ -> None

    // Per-field server errors (from TFFieldErrors status) keyed by field ID.
    let serverFieldErrors =
      match m.Status with
      | TFFieldErrors errs -> errs
      | _ -> Map.empty

    let fieldRows =
      m.Rows |> List.mapi (fun i r ->
        let isFocused = focused && i = m.FocusIndex
        let indicator =
          match isFocused with
          | true  -> El.text "▶ " |> El.fg (Named(Cyan, Bright))
          | false -> El.text "  "
        let pad = String.replicate (labelWidth - r.Field.Label.Length + 1) " "
        let labelEl =
          let t = El.text (r.Field.Label + pad)
          match isFocused with true -> t |> El.bold | false -> t
        let inputEl =
          let text = match r.Input.Text = "" && not isFocused with
                     | true  -> r.Field.Placeholder
                     | false -> ""
          let _ = text  // placeholder shown via TextInput.view
          TextInput.view isFocused r.Input
        let fieldRow = El.row [ indicator; labelEl; inputEl |> El.fill ]
        let indent = String.replicate (labelWidth + 3) " "
        let validationError =
          match r.Error with
          | None -> []
          | Some msg -> [ El.text (indent + "⚠ " + msg) |> El.fg (Named(Red, Normal)) ]
        let serverError =
          match serverFieldErrors |> Map.tryFind r.Field.Id with
          | None     -> []
          | Some msg -> [ El.text (indent + "⚠ " + msg) |> El.fg (Named(Red, Bright)) ]
        fieldRow :: validationError @ serverError)
      |> List.concat

    let allRows =
      match statusBanner with
      | Some b -> fieldRows @ [b]
      | None -> fieldRows

    El.column allRows

// ─── TextEditor ────────────────────────────────────────────────────────────

/// A multi-line text editor model using a string array.
/// Suitable for note-taking, config editing, and multi-line inputs in a TUI.
type TextEditorModel = {
  Lines           : string array
  Row             : int
  Col             : int
  /// Optional selection anchor (row, col). None = no selection.
  SelectionAnchor : (int * int) option
  /// Index of the first visible line (for scrolling).
  ScrollTop       : int
  /// Optional maximum number of lines (None = unlimited).
  MaxLines        : int option
}

type TextEditorMsg =
  | TEInsertChar    of char
  | TENewline
  | TEBackspace
  | TEDelete
  | TEMoveLeft
  | TEMoveRight
  | TEMoveUp
  | TEMoveDown
  | TEMoveLineStart
  | TEMoveLineEnd
  | TEMoveDocStart
  | TEMoveDocEnd
  | TEWordJumpLeft
  | TEWordJumpRight
  | TESelectLeft    | TESelectRight | TESelectUp | TESelectDown
  | TESelectWordLeft | TESelectWordRight
  | TESelectAll
  | TEUndo
  | TERedo
  | TECut
  | TECopy
  | TEPaste of string
  | TESetContent of string

module TextEditor =
  let private clampCol (line: string) col = max 0 (min col line.Length)

  /// Create a new TextEditorModel with the given initial content.
  let init (initialContent: string) : TextEditorModel =
    let lines =
      match initialContent with
      | "" -> [| "" |]
      | s  -> s.Split('\n')
    { Lines = lines; Row = 0; Col = 0; SelectionAnchor = None; ScrollTop = 0; MaxLines = None }

  /// Get the full text content as a single string.
  let content (m: TextEditorModel) : string =
    m.Lines |> String.concat "\n"

  /// Set the maximum number of lines.
  let withMaxLines (n: int) (m: TextEditorModel) =
    { m with MaxLines = Some n }

  let private currentLine (m: TextEditorModel) = m.Lines.[m.Row]

  /// Return the current SelectionAnchor if one exists, otherwise the cursor position.
  /// Used by all select operations to preserve an existing anchor while extending.
  let private anchorOrHere (m: TextEditorModel) =
    match m.SelectionAnchor with Some a -> a | None -> (m.Row, m.Col)

  let private setPos row col (m: TextEditorModel) =
    let r = max 0 (min row (m.Lines.Length - 1))
    let c = clampCol m.Lines.[r] col
    { m with Row = r; Col = c; SelectionAnchor = None }

  let private replaceLines (lines: string array) row col (m: TextEditorModel) =
    let lines' = if lines.Length = 0 then [| "" |] else lines
    let r = max 0 (min row (lines'.Length - 1))
    let c = clampCol lines'.[r] col
    { m with Lines = lines'; Row = r; Col = c; SelectionAnchor = None }

  /// Returns the selected text as a string, or None if no selection.
  let selectedText (m: TextEditorModel) : string option =
    match m.SelectionAnchor with
    | None -> None
    | Some (ar, ac) ->
      let (r1, c1, r2, c2) =
        if (ar, ac) <= (m.Row, m.Col) then (ar, ac, m.Row, m.Col)
        else (m.Row, m.Col, ar, ac)
      if r1 = r2 then Some m.Lines.[r1].[c1..c2-1]
      else
        let first  = m.Lines.[r1].[c1..]
        let middle = m.Lines.[r1+1..r2-1]
        let last   = m.Lines.[r2].[..c2-1]
        Some (String.concat "\n" [| yield first; yield! middle; yield last |])

  /// Delete the selected region and return the new model.
  let private deleteSelection (m: TextEditorModel) : TextEditorModel =
    match m.SelectionAnchor with
    | None -> m
    | Some (ar, ac) ->
      let (r1, c1, r2, c2) =
        if (ar, ac) <= (m.Row, m.Col) then (ar, ac, m.Row, m.Col)
        else (m.Row, m.Col, ar, ac)
      let firstPart = m.Lines.[r1].[..c1-1]
      let lastPart  = m.Lines.[r2].[c2..]
      let newLines =
        [| yield! m.Lines.[..r1-1]
           yield firstPart + lastPart
           yield! m.Lines.[r2+1..] |]
      replaceLines newLines r1 c1 m

  // isWordChar is defined in TextWidgetHelpers (AutoOpen) above.

  let private prevWordBoundary (line: string) (col: int) =
    let mutable i = col - 1
    while i > 0 && not (isWordChar line (i-1)) do i <- i - 1
    while i > 0 && isWordChar line (i-1)      do i <- i - 1
    max 0 i

  let private nextWordBoundary (line: string) (col: int) =
    let mutable i = col
    while i < line.Length && not (isWordChar line i) do i <- i + 1
    while i < line.Length && isWordChar line i       do i <- i + 1
    i

  /// Update the model with a TextEditorMsg.
  let update (msg: TextEditorMsg) (m: TextEditorModel) : TextEditorModel =
    let m' =
      match m.SelectionAnchor with
      | Some _ when (match msg with TECut | TEDelete | TEBackspace | TEInsertChar _ | TENewline | TEPaste _ -> true | _ -> false) ->
        deleteSelection m
      | _ -> m
    match msg with
    | TEInsertChar c ->
      let line = m'.Lines.[m'.Row]
      let newLine = line.[..m'.Col-1] + string c + line.[m'.Col..]
      let newLines = m'.Lines |> Array.mapi (fun i l -> if i = m'.Row then newLine else l)
      replaceLines newLines m'.Row (m'.Col + 1) m'
    | TENewline ->
      match m'.MaxLines with
      | Some n when m'.Lines.Length >= n -> m'
      | _ ->
        let line = m'.Lines.[m'.Row]
        let before = line.[..m'.Col-1]
        let after  = line.[m'.Col..]
        let newLines =
          [| yield! m'.Lines.[..m'.Row-1]
             yield before
             yield after
             yield! m'.Lines.[m'.Row+1..] |]
        replaceLines newLines (m'.Row + 1) 0 m'
    | TEBackspace ->
      match m.SelectionAnchor with
      | Some _ -> m'  // selection was deleted in the pre-pass; don't also delete the char before cursor
      | None ->
        match m'.Col, m'.Row with
        | 0, 0 -> m'
        | 0, r ->
          let prev    = m'.Lines.[r-1]
          let curr    = m'.Lines.[r]
          let merged  = prev + curr
          let newLines =
            [| yield! m'.Lines.[..r-2]
               yield merged
               yield! m'.Lines.[r+1..] |]
          replaceLines newLines (r-1) prev.Length m'
        | col, r ->
          let line    = m'.Lines.[r]
          let newLine = line.[..col-2] + line.[col..]
          let newLines = m'.Lines |> Array.mapi (fun i l -> if i = r then newLine else l)
          replaceLines newLines r (col - 1) m'
    | TEDelete ->
      match m.SelectionAnchor with
      | Some _ -> m'  // selection was deleted in the pre-pass; don't also delete the char at cursor
      | None ->
        let line = m'.Lines.[m'.Row]
        if m'.Col = line.Length then
          if m'.Row = m'.Lines.Length - 1 then m'
          else
            let next = m'.Lines.[m'.Row + 1]
            let merged = line + next
            let newLines =
              [| yield! m'.Lines.[..m'.Row-1]
                 yield merged
                 yield! m'.Lines.[m'.Row+2..] |]
            replaceLines newLines m'.Row m'.Col m'
        else
          let newLine = line.[..m'.Col-1] + line.[m'.Col+1..]
          let newLines = m'.Lines |> Array.mapi (fun i l -> if i = m'.Row then newLine else l)
          replaceLines newLines m'.Row m'.Col m'
    | TEMoveLeft ->
      match m'.Col with
      | 0 when m'.Row > 0 -> setPos (m'.Row - 1) m'.Lines.[m'.Row - 1].Length m'
      | _ -> setPos m'.Row (m'.Col - 1) m'
    | TEMoveRight ->
      let line = currentLine m'
      if m'.Col = line.Length then
        if m'.Row < m'.Lines.Length - 1 then setPos (m'.Row + 1) 0 m'
        else m'
      else setPos m'.Row (m'.Col + 1) m'
    | TEMoveUp   -> setPos (m'.Row - 1) m'.Col m'
    | TEMoveDown -> setPos (m'.Row + 1) m'.Col m'
    | TEMoveLineStart -> setPos m'.Row 0 m'
    | TEMoveLineEnd   -> setPos m'.Row (currentLine m').Length m'
    | TEMoveDocStart  -> setPos 0 0 m'
    | TEMoveDocEnd    ->
      let lastRow = m'.Lines.Length - 1
      setPos lastRow m'.Lines.[lastRow].Length m'
    | TEWordJumpLeft  ->
      if m'.Col = 0 && m'.Row > 0 then setPos (m'.Row - 1) m'.Lines.[m'.Row - 1].Length m'
      else setPos m'.Row (prevWordBoundary m'.Lines.[m'.Row] m'.Col) m'
    | TEWordJumpRight ->
      let line = currentLine m'
      if m'.Col = line.Length && m'.Row < m'.Lines.Length - 1 then setPos (m'.Row + 1) 0 m'
      else setPos m'.Row (nextWordBoundary line m'.Col) m'
    | TESelectLeft ->
      // At Col=0: extend selection to end of previous line (cross-line selection).
      // At document start (Row=0, Col=0): no movement, no anchor created.
      match m'.Col, m'.Row with
      | 0, 0 -> m'  // document start — no degenerate anchor
      | 0, _ ->
        let prevRow = m'.Row - 1
        { m' with Row = prevRow; Col = m'.Lines.[prevRow].Length; SelectionAnchor = Some (anchorOrHere m') }
      | _ ->
        { m' with Col = m'.Col - 1; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectRight ->
      let line = currentLine m'
      // At end-of-line: extend selection to start of next line (cross-line selection).
      // At document end: no movement, no anchor created.
      match m'.Col = line.Length, m'.Row = m'.Lines.Length - 1 with
      | true, true  -> m'  // document end — no degenerate anchor
      | true, false ->
        { m' with Row = m'.Row + 1; Col = 0; SelectionAnchor = Some (anchorOrHere m') }
      | false, _ ->
        { m' with Col = m'.Col + 1; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectUp ->
      let newRow = max 0 (m'.Row - 1)
      let newCol = clampCol m'.Lines.[newRow] m'.Col
      match (newRow, newCol) = (m'.Row, m'.Col) with
      | true  -> m'
      | false -> { m' with Row = newRow; Col = newCol; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectDown ->
      let newRow = min (m'.Lines.Length - 1) (m'.Row + 1)
      let newCol = clampCol m'.Lines.[newRow] m'.Col
      match (newRow, newCol) = (m'.Row, m'.Col) with
      | true  -> m'
      | false -> { m' with Row = newRow; Col = newCol; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectWordLeft ->
      let newRow, newCol =
        match m'.Col with
        | 0 when m'.Row > 0 -> m'.Row - 1, m'.Lines.[m'.Row - 1].Length
        | _ -> m'.Row, prevWordBoundary m'.Lines.[m'.Row] m'.Col
      // No movement at document start → no degenerate anchor
      match (newRow, newCol) = (m'.Row, m'.Col) with
      | true  -> m'
      | false -> { m' with Row = newRow; Col = newCol; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectWordRight ->
      let line = m'.Lines.[m'.Row]
      let newRow, newCol =
        match m'.Col = line.Length with
        | true when m'.Row < m'.Lines.Length - 1 -> m'.Row + 1, 0
        | _ -> m'.Row, nextWordBoundary line m'.Col
      // No movement at document end → no degenerate anchor
      match (newRow, newCol) = (m'.Row, m'.Col) with
      | true  -> m'
      | false -> { m' with Row = newRow; Col = newCol; SelectionAnchor = Some (anchorOrHere m') }
    | TESelectAll ->
      let lastRow = m'.Lines.Length - 1
      { m' with Row = lastRow; Col = m'.Lines.[lastRow].Length; SelectionAnchor = Some (0, 0) }
    | TECopy  -> m'  // caller reads selectedText m
    | TECut   -> m'  // selection already deleted above
    | TEUndo  -> m'  // no-op in stateless update; use TextEditor.updateWithUndo for history
    | TERedo  -> m'  // no-op in stateless update; use TextEditor.updateWithUndo for history
    | TEPaste text ->
      let toInsert = text.Split('\n')
      match toInsert with
      | [| single |] ->
        let line = m'.Lines.[m'.Row]
        let newLine = line.[..m'.Col-1] + single + line.[m'.Col..]
        let newLines = m'.Lines |> Array.mapi (fun i l -> if i = m'.Row then newLine else l)
        replaceLines newLines m'.Row (m'.Col + single.Length) m'
      | parts ->
        let firstPart = m'.Lines.[m'.Row].[..m'.Col-1] + parts.[0]
        let lastPart  = parts.[parts.Length-1] + m'.Lines.[m'.Row].[m'.Col..]
        let newLines =
          [| yield! m'.Lines.[..m'.Row-1]
             yield firstPart
             yield! parts.[1..parts.Length-2]
             yield lastPart
             yield! m'.Lines.[m'.Row+1..] |]
        replaceLines newLines (m'.Row + parts.Length - 1) parts.[parts.Length-1].Length m'
    | TESetContent s ->
      let lines = match s with "" -> [| "" |] | _ -> s.Split('\n')
      { m' with Lines = lines; Row = 0; Col = 0; SelectionAnchor = None; ScrollTop = 0 }

  /// Adjust ScrollTop so that Row is visible in a viewport of `height` rows.
  let scrollIntoView (height: int) (m: TextEditorModel) : TextEditorModel =
    let top =
      if m.Row < m.ScrollTop then m.Row
      elif m.Row >= m.ScrollTop + height then m.Row - height + 1
      else m.ScrollTop
    { m with ScrollTop = top }

  /// Wrap a TextEditorModel in an UndoableModel for undo/redo support.
  /// History is capped at `maxHistoryDepth` entries (default 200) when using `updateWithUndo`.
  let withUndo (m: TextEditorModel) : UndoableModel<TextEditorModel> = Undoable.init m

  /// Exhaustive categorization of which TextEditorMsg cases mutate text content.
  /// No wildcard — the compiler enforces that new cases are explicitly categorized.
  let private textModifies (msg: TextEditorMsg) =
    match msg with
    | TEInsertChar _ | TENewline | TEBackspace | TEDelete
    | TEPaste _      | TESetContent _ | TECut -> true
    | TEMoveLeft | TEMoveRight | TEMoveUp | TEMoveDown
    | TEMoveLineStart | TEMoveLineEnd | TEMoveDocStart | TEMoveDocEnd
    | TEWordJumpLeft  | TEWordJumpRight
    | TESelectLeft | TESelectRight | TESelectUp | TESelectDown
    | TESelectWordLeft | TESelectWordRight | TESelectAll
    | TEUndo | TERedo | TECopy -> false

  /// Update an UndoableModel<TextEditorModel> with a custom history depth cap.
  /// Text-modifying messages commit to the undo stack and cap history at `maxHistoryDepth`.
  /// TEUndo/TERedo navigate the history. Cursor/selection messages update Present only.
  let updateWithUndoDepth (maxHistoryDepth: int) (msg: TextEditorMsg) (um: UndoableModel<TextEditorModel>) : UndoableModel<TextEditorModel> =
    match msg with
    | TEUndo -> Undoable.undo um
    | TERedo -> Undoable.redo um
    | _ ->
      let newPresent = update msg um.Present
      match textModifies msg with
      | true  -> um |> Undoable.commitIfChanged newPresent |> Undoable.truncate maxHistoryDepth
      | false -> { um with Present = newPresent }

  /// Update an UndoableModel<TextEditorModel> with a TextEditorMsg.
  /// Text-modifying messages commit to the undo stack (history capped at 200 entries).
  /// TEUndo and TERedo navigate the history.
  /// Selection and cursor messages update Present without touching the stack.
  /// Note: prefer `updateWithUndo` over the plain `update` when undo support is desired.
  /// For a custom depth cap use `updateWithUndoDepth`.
  let updateWithUndo (msg: TextEditorMsg) (um: UndoableModel<TextEditorModel>) : UndoableModel<TextEditorModel> =
    updateWithUndoDepth 200 msg um

  /// Render the editor into an Element.
  /// `focused` controls cursor visibility; `height` is the number of visible rows.
  let view (focused: bool) (height: int) (m: TextEditorModel) : Element =
    let m' = scrollIntoView height m
    let visibleLines = m'.Lines.[m'.ScrollTop .. min (m'.ScrollTop + height - 1) (m'.Lines.Length - 1)]

    // Normalized selection range: (startRow, startCol), (endRow, endCol) with start <= end
    let selRange =
      match focused, m'.SelectionAnchor with
      | false, _ | true, None -> None
      | true, Some (ar, ac) ->
        let cursor = (m'.Row, m'.Col)
        let anchor = (ar, ac)
        Some (if anchor <= cursor then anchor, cursor else cursor, anchor)

    let rows =
      visibleLines |> Array.mapi (fun vi line ->
        let absRow = m'.ScrollTop + vi
        match selRange with
        | Some ((startRow, startCol), (endRow, endCol)) when absRow >= startRow && absRow <= endRow ->
          // Row is (partially or fully) within selection
          let colStart = if absRow = startRow then startCol else 0
          let colEnd   = if absRow = endRow   then endCol   else line.Length
          let before = if colStart > 0 then line.[..colStart - 1] else ""
          let sel    = if colEnd > colStart then line.[colStart..colEnd - 1]
                       else match line with "" -> " " | _ -> ""
          let after  = if colEnd < line.Length then line.[colEnd..] else ""
          El.row [
            if before <> "" then El.text before
            if sel    <> "" then El.text sel |> El.reverse
            if after  <> "" then El.text after ]
        | _ ->
          // No selection — show cursor or plain text
          match focused && absRow = m'.Row with
          | false -> El.text (if line = "" then " " else line)
          | true ->
            // Surrogate-safe extraction of the character under the cursor
            let col = m'.Col
            let cursorChar, runeWidth =
              if col < line.Length then
                match System.Char.IsHighSurrogate(line.[col]) with
                | true when col + 1 < line.Length && System.Char.IsLowSurrogate(line.[col + 1]) ->
                  System.String([| line.[col]; line.[col + 1] |]), 2
                | _ -> string line.[col], 1
              else " ", 0
            let before = if col > 0 then line.[..col - 1] else ""
            let after  = if col + runeWidth < line.Length then line.[col + runeWidth..] else ""
            El.row [
              if before     <> "" then El.text before
              El.text cursorChar |> El.reverse
              if after      <> "" then El.text after ])
      |> Array.toList
    El.column rows

// ─── FuzzyFinder ────────────────────────────────────────────────────────────

/// A single fuzzy match result.
type FuzzyMatch = {
  /// The matched candidate string.
  Candidate      : string
  /// Relevance score (higher = better match).
  Score          : float
  /// Zero-based indices of matched characters in Candidate (for highlighting).
  MatchPositions : int array
  /// Index of this item in the original Items array. Use for correct lookup with duplicate display strings.
  OriginalIndex  : int
}

/// Message type for the FuzzyFinder widget.
type FuzzyFinderMsg<'item> =
  | FFQueryChanged of string
  | FFQueryKey of Key
  | FFQueryEvent of TerminalEvent
  | FFMoveUp
  | FFMoveDown
  | FFSelect
  | FFCancel
  | FFSetItems of 'item array

/// Model for the FuzzyFinder widget.
type FuzzyFinderModel<'item> = {
  Items       : 'item array
  /// Cached string representation of each item — computed once in `init`, invalidated on `FFSetItems`.
  Candidates  : string array
  /// Function to get the string to match against for each item.
  ToString    : 'item -> string
  /// The query input model (supports cursor, selection, word movement).
  QueryInput  : TextInputModel
  Results     : FuzzyMatch array
  SelectedIdx : int
}

module FuzzyFinder =

  // Word boundary characters for scoring — module-level to avoid per-call allocation
  let private boundaryChars =
    System.Collections.Generic.HashSet<char>([' '; '-'; '_'; '.'; '/'])

  // ── Scoring ──────────────────────────────────────────────────────────────

  /// Score `query` against `candidate`.
  /// Returns Some (score, positions) if it matches, None otherwise.
  let scoreMatch (query: string) (candidate: string) : (float * int array) option =
    if query.Length = 0 then Some (0.0, [||])
    else
      let cLower = candidate.ToLowerInvariant()
      let qLower = query.ToLowerInvariant()
      // Forward-pass subsequence check
      let positions = Array.create query.Length -1
      let mutable qi = 0
      let mutable ci = 0
      while qi < query.Length && ci < candidate.Length do
        match cLower.[ci] = qLower.[qi] with
        | true  -> positions.[qi] <- ci; qi <- qi + 1; ci <- ci + 1
        | false -> ci <- ci + 1
      match qi = query.Length with
      | false -> None
      | true ->
        // Compute score with bonuses
        let mutable score = 0.0
        // Base: length penalty
        score <- score - float candidate.Length * 0.01
        // Bonus: prefix match
        if positions.[0] = 0 then score <- score + 5.0
        // Bonus: consecutive run lengths
        let mutable run = 1
        let mutable maxRun = 1
        for k in 1 .. positions.Length - 1 do
          match positions.[k] = positions.[k-1] + 1 with
          | true  -> run <- run + 1; if run > maxRun then maxRun <- run
          | false -> run <- 1
        score <- score + float maxRun * 2.0
        // Bonus: word boundary matches (match after space, -, _, ., /)
        for pos in positions do
          // Skip the check if candidate.[pos-1] is a low surrogate (part of a multi-char codepoint)
          if pos > 0
             && not (System.Char.IsLowSurrogate(candidate.[pos - 1]))
             && boundaryChars.Contains(candidate.[pos - 1]) then
            score <- score + 1.5
          elif pos = 0 then score <- score + 1.0
        Some (score, positions)

  /// Sort matches: higher score first, then shorter candidate length.
  let sortMatches (matches: FuzzyMatch array) : FuzzyMatch array =
    matches |> Array.sortWith (fun a b ->
      let sc = compare b.Score a.Score
      match sc with 0 -> compare a.Candidate.Length b.Candidate.Length | _ -> sc)

  /// Run the scoring pass over all candidates synchronously.
  /// OriginalIndex in each result reflects the index of the candidate in the input array.
  let matchAll (query: string) (candidates: string array) : FuzzyMatch array =
    candidates
    |> Array.mapi (fun origIdx c ->
      match scoreMatch query c with
      | None -> None
      | Some (score, positions) ->
        Some { Candidate = c; Score = score; MatchPositions = positions; OriginalIndex = origIdx })
    |> Array.choose id
    |> sortMatches

  // ── Widget API ────────────────────────────────────────────────────────────

  /// Initialize the FuzzyFinder with an item array and a toString function.
  let init (toString: 'item -> string) (items: 'item array) : FuzzyFinderModel<'item> =
    let candidates = items |> Array.map toString
    let results = matchAll "" candidates |> Array.mapi (fun i m -> { m with Score = float -i })
    { Items = items; Candidates = candidates; ToString = toString; QueryInput = TextInput.empty; Results = results; SelectedIdx = 0 }

  /// Get the current query text.
  let query (m: FuzzyFinderModel<'item>) : string = m.QueryInput.Text

  /// Get the currently selected item, if any.
  /// Uses the OriginalIndex stored in the result for correct lookup even with duplicate display strings.
  let selectedItem (m: FuzzyFinderModel<'item>) : 'item option =
    match m.Results.Length, m.SelectedIdx with
    | 0, _ -> None
    | _, i  ->
      let origIdx = m.Results.[i].OriginalIndex
      match origIdx >= 0 && origIdx < m.Items.Length with
      | true  -> Some m.Items.[origIdx]
      | false -> None

  let private requery (queryInput: TextInputModel) (m: FuzzyFinderModel<'item>) =
    let results = matchAll queryInput.Text m.Candidates
    { m with QueryInput = queryInput; Results = results; SelectedIdx = 0 }

  /// Update the FuzzyFinder model.
  let update (msg: FuzzyFinderMsg<'item>) (m: FuzzyFinderModel<'item>) : FuzzyFinderModel<'item> =
    match msg with
    | FFQueryChanged q -> requery (TextInput.ofString q) m
    | FFQueryKey key   ->
      let qi' = TextInput.handleKey key m.QueryInput
      requery qi' m
    | FFQueryEvent evt ->
      let qi' = TextInput.handleEvent evt m.QueryInput
      requery qi' m
    | FFMoveUp         -> { m with SelectedIdx = max 0 (m.SelectedIdx - 1) }
    | FFMoveDown       -> { m with SelectedIdx = min (m.Results.Length - 1) (m.SelectedIdx + 1) }
    | FFSelect | FFCancel -> m  // caller handles side effects
    | FFSetItems items ->
      let candidates = items |> Array.map m.ToString
      let m' = { m with Items = items; Candidates = candidates }
      requery m'.QueryInput m'

  /// Create a Cmd that runs a fuzzy search asynchronously using the model's cached Candidates.
  /// This is the preferred async search entry point — it uses the model's cached string array
  /// and avoids re-running toString on every search call.
  let searchAsyncFromModel
      (query    : string)
      (m        : FuzzyFinderModel<'item>)
      (toMsg    : FuzzyMatch array -> 'msg)
      : Cmd<'msg> =
    let candidates = m.Candidates  // capture before async
    Cmd.ofAsync (fun dispatch -> async {
      do! Async.SwitchToThreadPool()
      let results = matchAll query candidates
      dispatch (toMsg results)
    })

  /// Create a Cmd that runs a fuzzy search asynchronously on a thread pool thread.
  /// Wraps result in `toMsg` so it can be dispatched to your TEA program.
  /// Prefer `searchAsyncFromModel` to avoid re-running toString on every call.
  let searchAsync
      (query    : string)
      (items    : 'item array)
      (toString : 'item -> string)
      (toMsg    : FuzzyMatch array -> 'msg)
      : Cmd<'msg> =
    Cmd.ofAsync (fun dispatch -> async {
      do! Async.SwitchToThreadPool()
      let candidates = items |> Array.map toString
      let results = matchAll query candidates
      dispatch (toMsg results)
    })

  /// Render the FuzzyFinder prompt + result list.
  /// `focused` drives cursor display on the query input.
  /// `height` is the number of result rows to show.
  let view (focused: bool) (height: int) (m: FuzzyFinderModel<'item>) : Element =
    let promptPrefix = El.text "> "
    let queryEl = TextInput.view focused m.QueryInput
    let promptInput = El.row [ promptPrefix; queryEl ]
    let resultRows =
      m.Results
      |> Array.truncate height
      |> Array.mapi (fun i r ->
        let isSelected = i = m.SelectedIdx && focused
        let prefix = if isSelected then "▶ " else "  "
        let prefixEl =
          match isSelected with
          | true  -> El.text prefix |> El.fg (Color.Named(Cyan, Bright))
          | false -> El.text prefix
        // Build highlighted candidate text by splitting on match positions
        let posSet = System.Collections.Generic.HashSet<int>(r.MatchPositions)
        let chars = r.Candidate.ToCharArray()
        // Group consecutive segments: matched vs unmatched
        let segments =
          chars
          |> Array.mapi (fun i ch -> (i, ch, posSet.Contains i))
          |> Array.fold (fun (acc: (bool * string) list) (_, ch, isMatch) ->
            match acc with
            | (m, s) :: rest when m = isMatch -> (m, s + string ch) :: rest
            | _ -> (isMatch, string ch) :: acc) []
          |> List.rev
        let segElems =
          segments |> List.map (fun (isMatch, s) ->
            match isMatch, isSelected with
            | true, _      -> El.text s |> El.fg (Color.Named(Yellow, Bright)) |> El.bold
            | false, true  -> El.text s |> El.fg (Color.Named(Cyan, Normal))
            | false, false -> El.text s)
        El.row (prefixEl :: segElems))
      |> Array.toList
    let countEl =
      El.text (sprintf " %d/%d" m.Results.Length (m.Items.Length))
      |> El.fg (Color.Named(White, Normal))
    El.column (promptInput :: resultRows @ [countEl])

// ─────────────────────────────────────────────────────────────────────────────
// SplitPane
// ─────────────────────────────────────────────────────────────────────────────

/// Orientation of a SplitPane divider.
type SplitOrientation = SplitHorizontal | SplitVertical

/// Model for a SplitPane widget — holds orientation, split ratio, and child elements.
/// This is an immutable F# record; update via `SplitPane.resize`, `grow`, `shrink`, etc.
type SplitPaneModel = {
  Orientation: SplitOrientation
  /// Percentage (1–99) allocated to the first pane. Second pane gets the remainder.
  SplitPercent: int
  First: Element
  Second: Element
}

module SplitPane =
  /// Create a SplitPane model with the given orientation and initial split percent.
  let init (orientation: SplitOrientation) (splitPercent: int) (first: Element) (second: Element) : SplitPaneModel =
    { Orientation  = orientation
      SplitPercent = splitPercent |> max 1 |> min 99
      First        = first
      Second       = second }

  /// Resize the first pane to `pct` percent (clamped to [1, 99]).
  let resize (pct: int) (m: SplitPaneModel) : SplitPaneModel =
    { m with SplitPercent = pct |> max 1 |> min 99 }

  /// Grow the first pane by `step` percentage points (clamped at 99).
  /// Passing a negative step shrinks the first pane — equivalent to `shrink (abs step)`.
  let grow (step: int) (m: SplitPaneModel) : SplitPaneModel =
    resize (m.SplitPercent + step) m

  /// Shrink the first pane by `step` percentage points (clamped at 1).
  /// Passing a negative step grows the first pane — equivalent to `grow (abs step)`.
  let shrink (step: int) (m: SplitPaneModel) : SplitPaneModel =
    resize (m.SplitPercent - step) m

  /// Replace the first (primary) child element.
  let setFirst (elem: Element) (m: SplitPaneModel) : SplitPaneModel =
    { m with First = elem }

  /// Replace the second (secondary) child element.
  let setSecond (elem: Element) (m: SplitPaneModel) : SplitPaneModel =
    { m with Second = elem }

  /// Render the SplitPane as a Row (horizontal) or Column (vertical).
  /// Each child is wrapped in a Ratio constraint so layout splits proportionally.
  let view (m: SplitPaneModel) : Element =
    let pct    = m.SplitPercent
    let remain = 100 - pct
    let firstEl  = Constrained(Ratio(pct,    100), m.First)
    let secondEl = Constrained(Ratio(remain, 100), m.Second)
    match m.Orientation with
    | SplitHorizontal -> Row    [firstEl; secondEl]
    | SplitVertical   -> Column [firstEl; secondEl]
