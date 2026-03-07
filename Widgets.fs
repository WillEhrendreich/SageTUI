namespace SageTUI

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
