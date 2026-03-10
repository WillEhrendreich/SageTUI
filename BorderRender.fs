namespace SageTUI

module BorderRender =
  let renderBorder (style: BorderStyle) (title: string option) (inheritedStyle: Style) (area: Area) (buf: Buffer) =
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
    // Top border: plain H bars or title
    let topStart = area.X + 1
    let topEnd = area.X + area.Width - 2
    let renderPlainTop () =
      for x in topStart .. topEnd do Buffer.set x area.Y (makeCell h) buf
    match title with
    | None | Some "" -> renderPlainTop ()
    | Some titleStr ->
      // Max display cells available for title: 1(H) + 1(sp) + title + 1(sp) ≤ interior
      // where interior = W - 2, so max_title_dw = W - 5
      let maxTitleDw = area.Width - 5
      if maxTitleDw <= 0 then renderPlainTop ()
      else
        // Compute display width of full title
        let mutable fullDw = 0
        for r in titleStr.EnumerateRunes() do
          fullDw <- fullDw + RuneWidth.getColumnWidth r
        // Truncate to maxTitleDw if needed, appending "…"
        let renderTitle =
          if fullDw <= maxTitleDw then titleStr
          else
            let target = maxTitleDw - 1  // leave 1 cell for "…"
            let runes = [| for r in titleStr.EnumerateRunes() -> r |]
            let mutable acc = 0
            let mutable charIdx = 0
            let mutable i = 0
            while i < runes.Length && acc + RuneWidth.getColumnWidth runes.[i] <= target do
              acc <- acc + RuneWidth.getColumnWidth runes.[i]
              charIdx <- charIdx + runes.[i].Utf16SequenceLength
              i <- i + 1
            (if charIdx > 0 then titleStr.[0..charIdx - 1] else "") + "…"
        // Compute actual display width of (possibly truncated) title
        let mutable renderDw = 0
        for r in renderTitle.EnumerateRunes() do
          renderDw <- renderDw + RuneWidth.getColumnWidth r
        // Write: mandatory H | space | title | space | trailing H*
        Buffer.set topStart area.Y (makeCell h) buf
        Buffer.set (topStart + 1) area.Y (makeCell ' ') buf
        Buffer.writeString (topStart + 2) area.Y fg bg attrs renderTitle buf
        Buffer.set (topStart + 2 + renderDw) area.Y (makeCell ' ') buf
        for x in (topStart + 2 + renderDw + 1) .. topEnd do
          Buffer.set x area.Y (makeCell h) buf
    Buffer.set (area.X + area.Width - 1) area.Y (makeCell tr) buf
    Buffer.set area.X (area.Y + area.Height - 1) (makeCell bl) buf
    for x in area.X + 1 .. area.X + area.Width - 2 do
      Buffer.set x (area.Y + area.Height - 1) (makeCell h) buf
    Buffer.set (area.X + area.Width - 1) (area.Y + area.Height - 1) (makeCell br) buf
    for y in area.Y + 1 .. area.Y + area.Height - 2 do
      Buffer.set area.X y (makeCell v) buf
      Buffer.set (area.X + area.Width - 1) y (makeCell v) buf
