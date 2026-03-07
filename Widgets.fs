namespace SageTUI

type TextInputModel = {
  Text: string
  Cursor: int
}

module TextInput =
  let empty = { Text = ""; Cursor = 0 }

  let ofString s = { Text = s; Cursor = String.length s }

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

  let view (focused: bool) (model: TextInputModel) =
    let displayText =
      match model.Text.Length = 0 with
      | true -> " "
      | false -> model.Text
    match focused with
    | true -> El.text displayText |> El.underline
    | false -> El.text displayText

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
