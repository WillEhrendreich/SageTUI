namespace SageTUI

module BorderRender =
  let renderBorder (style: BorderStyle) (inheritedStyle: Style) (area: Area) (buf: Buffer) =
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
