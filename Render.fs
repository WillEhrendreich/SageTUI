namespace SageTUI

open System.Text

module Render =
  let extractConstraint (elem: Element) =
    match elem with
    | Constrained(c, _) -> c
    | _ -> Fill

  let unwrapConstrained (elem: Element) =
    match elem with
    | Constrained(_, child) -> child
    | other -> other

  let rec render (area: Area) (inheritedStyle: Style) (buf: Buffer) (elem: Element) =
    match area.Width <= 0 || area.Height <= 0 with
    | true -> ()
    | false ->
      match elem with
      | Empty -> ()

      | Text(text, localStyle) ->
        let resolved = Style.merge inheritedStyle localStyle
        let fg = resolved.Fg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let bg = resolved.Bg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let attrs = resolved.Attrs.Value
        let mutable col = 0
        let sb = StringBuilder()
        for rune in text.EnumerateRunes() do
          let w = RuneWidth.getColumnWidth rune
          match col + w <= area.Width with
          | true ->
            sb.Append(rune.ToString()) |> ignore
            col <- col + w
          | false -> ()
        Buffer.writeString area.X area.Y fg bg attrs (sb.ToString()) buf

      | Row children ->
        let constraints = children |> List.map extractConstraint
        let unwrapped = children |> List.map unwrapConstrained
        let areas = Layout.splitH constraints area
        List.iter2 (fun childArea child ->
          render childArea inheritedStyle buf child) areas unwrapped

      | Column children ->
        let constraints = children |> List.map extractConstraint
        let unwrapped = children |> List.map unwrapConstrained
        let areas = Layout.splitV constraints area
        List.iter2 (fun childArea child ->
          render childArea inheritedStyle buf child) areas unwrapped

      | Overlay layers ->
        layers |> List.iter (render area inheritedStyle buf)

      | Styled(style, child) ->
        render area (Style.merge inheritedStyle style) buf child

      | Constrained(c, child) ->
        let constrained = applyConstraint c area
        render constrained inheritedStyle buf child

      | Bordered(borderStyle, child) ->
        renderBorder borderStyle inheritedStyle area buf
        let inner = shrinkForBorder area
        render inner inheritedStyle buf child

      | Padded(padding, child) ->
        let inner =
          { X = area.X + padding.Left
            Y = area.Y + padding.Top
            Width = max 0 (area.Width - padding.Left - padding.Right)
            Height = max 0 (area.Height - padding.Top - padding.Bottom) }
        render inner inheritedStyle buf child

      | Keyed(_, _, _, child) ->
        render area inheritedStyle buf child

      | Canvas _ -> ()

  and applyConstraint (c: Constraint) (area: Area) =
    match c with
    | Fixed n -> { area with Width = min n area.Width }
    | Min n -> { area with Width = max n area.Width }
    | Max n -> { area with Width = min n area.Width }
    | Percentage pct -> { area with Width = area.Width * pct / 100 }
    | Fill -> area
    | Ratio(num, den) ->
      match den > 0 with
      | true -> { area with Width = area.Width * num / den }
      | false -> area

  and shrinkForBorder (area: Area) =
    { X = area.X + 1; Y = area.Y + 1
      Width = max 0 (area.Width - 2); Height = max 0 (area.Height - 2) }

  and renderBorder (style: BorderStyle) (inheritedStyle: Style) (area: Area) (buf: Buffer) =
    let chars =
      match style with
      | Light -> ('\u250C', '\u2510', '\u2514', '\u2518', '\u2500', '\u2502')
      | Heavy -> ('\u250F', '\u2513', '\u2517', '\u251B', '\u2501', '\u2503')
      | Double -> ('\u2554', '\u2557', '\u255A', '\u255D', '\u2550', '\u2551')
      | Rounded -> ('\u256D', '\u256E', '\u2570', '\u256F', '\u2500', '\u2502')
      | Ascii -> ('+', '+', '+', '+', '-', '|')
    let (tl, tr, bl, br, h, v) = chars
    let resolved = Style.merge Style.empty inheritedStyle
    let fg = resolved.Fg |> Option.map PackedColor.pack |> Option.defaultValue 0
    let bg = resolved.Bg |> Option.map PackedColor.pack |> Option.defaultValue 0
    let attrs = resolved.Attrs.Value
    let inline makeCell (ch: char) =
      { Rune = int32 (System.Text.Rune(ch)).Value; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us }
    Buffer.set area.X area.Y (makeCell tl) buf
    for x in area.X + 1 .. area.X + area.Width - 2 do
      Buffer.set x area.Y (makeCell h) buf
    Buffer.set (area.X + area.Width - 1) area.Y (makeCell tr) buf
    Buffer.set area.X (area.Y + area.Height - 1) (makeCell bl) buf
    for x in area.X + 1 .. area.X + area.Width - 2 do
      Buffer.set x (area.Y + area.Height - 1) (makeCell h) buf
    Buffer.set (area.X + area.Width - 1) (area.Y + area.Height - 1) (makeCell br) buf
    for y in area.Y + 1 .. area.Y + area.Height - 2 do
      Buffer.set area.X y (makeCell v) buf
      Buffer.set (area.X + area.Width - 1) y (makeCell v) buf
