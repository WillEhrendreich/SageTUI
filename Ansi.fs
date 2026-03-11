namespace SageTUI

open System.Text

module Ansi =
  let esc = "\x1b["

  let moveCursor row col = sprintf "%s%d;%dH" esc (row + 1) (col + 1)
  let hideCursor = sprintf "%s?25l" esc
  let showCursor = sprintf "%s?25h" esc
  let clearScreen = sprintf "%s2J%sH" esc esc
  /// Erase from current cursor position to end of line.
  let clearToEol = sprintf "%sK" esc
  /// Erase from current cursor position to end of display.
  let clearToEod = sprintf "%sJ" esc
  let enterAltScreen = sprintf "%s?1049h" esc
  let leaveAltScreen = sprintf "%s?1049l" esc
  let enableMouseTracking = sprintf "%s?1000h%s?1006h" esc esc
  let disableMouseTracking = sprintf "%s?1000l%s?1006l" esc esc
  /// Enable button-event tracking (?1002h): terminal sends motion events while a mouse button is held.
  /// Also enables SGR extended encoding (?1006h) for coordinates beyond 223.
  /// Pair with disableButtonTracking on exit.
  let enableButtonTracking = sprintf "%s?1002h%s?1006h" esc esc
  /// Disable button-event tracking (?1002l) and SGR extended encoding (?1006l).
  let disableButtonTracking = sprintf "%s?1002l%s?1006l" esc esc
  /// Enable bracketed paste mode (?2004h): terminal wraps pasted text with ESC[200~ ... ESC[201~.
  /// PasteSub in Tea.fs requires this to receive Pasted events.
  let enableBracketedPaste = sprintf "%s?2004h" esc
  /// Disable bracketed paste mode (?2004l).
  let disableBracketedPaste = sprintf "%s?2004l" esc
  let resetStyle = sprintf "%s0m" esc

  let fgColor (c: Color) =
    match c with
    | Default -> sprintf "%s39m" esc
    | Named(base', intensity) ->
      let code =
        match base' with
        | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
        | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
      let offset = match intensity with Normal -> 30 | Bright -> 90
      sprintf "%s%dm" esc (offset + code)
    | Ansi256 idx -> sprintf "%s38;5;%dm" esc (int idx)
    | Rgb(r, g, b) -> sprintf "%s38;2;%d;%d;%dm" esc r g b

  let bgColor (c: Color) =
    match c with
    | Default -> sprintf "%s49m" esc
    | Named(base', intensity) ->
      let code =
        match base' with
        | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
        | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
      let offset = match intensity with Normal -> 40 | Bright -> 100
      sprintf "%s%dm" esc (offset + code)
    | Ansi256 idx -> sprintf "%s48;5;%dm" esc (int idx)
    | Rgb(r, g, b) -> sprintf "%s48;2;%d;%d;%dm" esc r g b

  let textAttrs (attrs: TextAttrs) =
    let sb = StringBuilder()
    if TextAttrs.has TextAttrs.bold attrs then sb.Append(sprintf "%s1m" esc) |> ignore
    if TextAttrs.has TextAttrs.dim attrs then sb.Append(sprintf "%s2m" esc) |> ignore
    if TextAttrs.has TextAttrs.italic attrs then sb.Append(sprintf "%s3m" esc) |> ignore
    if TextAttrs.has TextAttrs.underline attrs then sb.Append(sprintf "%s4m" esc) |> ignore
    if TextAttrs.has TextAttrs.blink attrs then sb.Append(sprintf "%s5m" esc) |> ignore
    if TextAttrs.has TextAttrs.reverse attrs then sb.Append(sprintf "%s7m" esc) |> ignore
    if TextAttrs.has TextAttrs.hidden attrs then sb.Append(sprintf "%s8m" esc) |> ignore
    if TextAttrs.has TextAttrs.strikethrough attrs then sb.Append(sprintf "%s9m" esc) |> ignore
    sb.ToString()

  let fgColorPacked (packed: int32) = fgColor (PackedColor.unpack packed)
  let bgColorPacked (packed: int32) = bgColor (PackedColor.unpack packed)
  let textAttrsPacked (packed: uint16) = textAttrs { Value = packed }

  // ---------------------------------------------------------------------------
  // Zero-alloc StringBuilder append helpers — used by Presenter.presentInto.
  // These write ANSI sequences directly into a pre-allocated StringBuilder using
  // the StringBuilder.Append(int) overload, which is zero-alloc (JIT writes the
  // decimal representation directly into the internal char[] without a string).
  // ---------------------------------------------------------------------------

  let private appendEsc (sb: StringBuilder) = sb.Append("\x1b[") |> ignore

  let appendMoveCursor (sb: StringBuilder) row col =
    appendEsc sb
    sb.Append(row + 1).Append(';').Append(col + 1).Append('H') |> ignore

  let appendResetStyle (sb: StringBuilder) = sb.Append("\x1b[0m") |> ignore

  let appendFgColor (sb: StringBuilder) (c: Color) =
    match c with
    | Default -> appendEsc sb; sb.Append("39m") |> ignore
    | Named(base', intensity) ->
      let code =
        match base' with
        | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
        | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
      let offset = match intensity with Normal -> 30 | Bright -> 90
      appendEsc sb; sb.Append(offset + code).Append('m') |> ignore
    | Ansi256 idx ->
      appendEsc sb; sb.Append("38;5;").Append(int idx).Append('m') |> ignore
    | Rgb(r, g, b) ->
      appendEsc sb
      sb.Append("38;2;").Append(int r).Append(';').Append(int g).Append(';').Append(int b).Append('m') |> ignore

  let appendBgColor (sb: StringBuilder) (c: Color) =
    match c with
    | Default -> appendEsc sb; sb.Append("49m") |> ignore
    | Named(base', intensity) ->
      let code =
        match base' with
        | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
        | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
      let offset = match intensity with Normal -> 40 | Bright -> 100
      appendEsc sb; sb.Append(offset + code).Append('m') |> ignore
    | Ansi256 idx ->
      appendEsc sb; sb.Append("48;5;").Append(int idx).Append('m') |> ignore
    | Rgb(r, g, b) ->
      appendEsc sb
      sb.Append("48;2;").Append(int r).Append(';').Append(int g).Append(';').Append(int b).Append('m') |> ignore

  let appendTextAttrs (sb: StringBuilder) (attrs: TextAttrs) =
    if TextAttrs.has TextAttrs.bold attrs then appendEsc sb; sb.Append("1m") |> ignore
    if TextAttrs.has TextAttrs.dim attrs then appendEsc sb; sb.Append("2m") |> ignore
    if TextAttrs.has TextAttrs.italic attrs then appendEsc sb; sb.Append("3m") |> ignore
    if TextAttrs.has TextAttrs.underline attrs then appendEsc sb; sb.Append("4m") |> ignore
    if TextAttrs.has TextAttrs.blink attrs then appendEsc sb; sb.Append("5m") |> ignore
    if TextAttrs.has TextAttrs.reverse attrs then appendEsc sb; sb.Append("7m") |> ignore
    if TextAttrs.has TextAttrs.hidden attrs then appendEsc sb; sb.Append("8m") |> ignore
    if TextAttrs.has TextAttrs.strikethrough attrs then appendEsc sb; sb.Append("9m") |> ignore

  /// Zero-alloc fg color: decodes packed bits directly without allocating a Color DU.
  let appendFgColorPacked (sb: StringBuilder) (packed: int32) =
    let tag = packed &&& 0x3
    appendEsc sb
    match tag with
    | 0 -> sb.Append("39m") |> ignore
    | 1 ->
      let bcIdx  = (packed >>> 2) &&& 0x7
      let iIdx   = (packed >>> 5) &&& 0x1
      let offset = if iIdx = 0 then 30 else 90
      sb.Append(offset + bcIdx).Append('m') |> ignore
    | 2 ->
      let idx = (packed >>> 8) &&& 0xFF
      sb.Append("38;5;").Append(idx).Append('m') |> ignore
    | _ ->
      let r = (packed >>>  8) &&& 0xFF
      let g = (packed >>> 16) &&& 0xFF
      let b = (packed >>> 24) &&& 0xFF
      sb.Append("38;2;").Append(r).Append(';').Append(g).Append(';').Append(b).Append('m') |> ignore

  /// Zero-alloc bg color: decodes packed bits directly without allocating a Color DU.
  let appendBgColorPacked (sb: StringBuilder) (packed: int32) =
    let tag = packed &&& 0x3
    appendEsc sb
    match tag with
    | 0 -> sb.Append("49m") |> ignore
    | 1 ->
      let bcIdx  = (packed >>> 2) &&& 0x7
      let iIdx   = (packed >>> 5) &&& 0x1
      let offset = if iIdx = 0 then 40 else 100
      sb.Append(offset + bcIdx).Append('m') |> ignore
    | 2 ->
      let idx = (packed >>> 8) &&& 0xFF
      sb.Append("48;5;").Append(idx).Append('m') |> ignore
    | _ ->
      let r = (packed >>>  8) &&& 0xFF
      let g = (packed >>> 16) &&& 0xFF
      let b = (packed >>> 24) &&& 0xFF
      sb.Append("48;2;").Append(r).Append(';').Append(g).Append(';').Append(b).Append('m') |> ignore
  let appendTextAttrsPacked (sb: StringBuilder) (packed: uint16) = appendTextAttrs sb { Value = packed }

  /// Produce an OSC 52 escape sequence that writes `text` to the system clipboard.
  /// Supported by most modern terminals (kitty, WezTerm, iTerm2, Windows Terminal, tmux ≥3.2).
  /// Silently ignored by terminals that do not implement OSC 52.
  let osc52Copy (text: string) =
    let encoded = System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(text))
    sprintf "\x1b]52;c;%s\x07" encoded

  /// Open an OSC 8 hyperlink. Emit before the link text.
  /// Format: ESC ] 8 ; params ; url ST (using BEL as ST for compatibility).
  let oscHyperlinkOpen (url: string) : string = sprintf "\x1b]8;;%s\x1b\\" url

  /// Close an OSC 8 hyperlink. Emit after the link text.
  let oscHyperlinkClose : string = "\x1b]8;;\x1b\\"

module Presenter =
  /// Zero-alloc hot path: writes ANSI output for all changed cells directly into
  /// a pre-allocated StringBuilder. Caller owns the StringBuilder lifecycle:
  ///   1. Pre-allocate once: `let sb = StringBuilder(65536)`
  ///   2. Each frame: `sb.Clear(); presentInto sb changes buf; backend.Write(sb.ToString())`
  /// This eliminates the per-frame `StringBuilder()` allocation and the hundreds of
  /// `sprintf`-produced strings from moveCursor / fgColor / bgColor / textAttrs.
  /// The final `sb.ToString()` is a single allocation; see presentIntoSpan for span path.
  let presentInto (sb: StringBuilder) (changes: ResizeArray<int>) (buf: Buffer) =
    let mutable lastRow = -1
    let mutable lastCol = -1
    let mutable lastFg = 0
    let mutable lastBg = 0
    let mutable lastAttrs = 0us

    // Reset style at the start of every frame so carry-over from the
    // previous frame's last cell doesn't corrupt the first changed cell.
    if changes.Count > 0 then
      Ansi.appendResetStyle sb
      lastFg <- 0
      lastBg <- 0
      lastAttrs <- 0us

    for idx in changes do
      let x = idx % buf.Width
      let y = idx / buf.Width
      let cell = buf.Cells.[idx]

      if cell.Rune <> 0 then

        match y <> lastRow || x <> lastCol + 1 with
        | true -> Ansi.appendMoveCursor sb y x
        | false -> ()

        match cell.Fg <> lastFg || cell.Bg <> lastBg || cell.Attrs <> lastAttrs with
        | true ->
          Ansi.appendResetStyle sb
          Ansi.appendFgColorPacked sb cell.Fg
          Ansi.appendBgColorPacked sb cell.Bg
          Ansi.appendTextAttrsPacked sb cell.Attrs
          lastFg <- cell.Fg
          lastBg <- cell.Bg
          lastAttrs <- cell.Attrs
        | false -> ()

        // ASCII fast path: avoid Rune.ToString() allocation for printable ASCII (U+0020–U+007E).
        // Rune column width is always 1 for ASCII; skip the RuneWidth lookup too.
        let colWidth =
          match cell.Rune >= 0x20 && cell.Rune <= 0x7E with
          | true ->
            sb.Append(char cell.Rune) |> ignore
            1
          | false ->
            let rune = System.Text.Rune(cell.Rune)
            sb.Append(rune.ToString()) |> ignore
            RuneWidth.getColumnWidth rune

        lastCol <- x + colWidth - 1
        lastRow <- y

  let present (changes: ResizeArray<int>) (buf: Buffer) =
    let sb = StringBuilder()
    presentInto sb changes buf
    sb.ToString()

  /// Zero-alloc inline variant — writes rows offset by `rowOffset` for non-alt-screen rendering.
  let presentAtInto (rowOffset: int) (sb: StringBuilder) (changes: ResizeArray<int>) (buf: Buffer) =
    let mutable lastTerminalRow = -1
    let mutable lastCol = -1
    let mutable lastFg = 0
    let mutable lastBg = 0
    let mutable lastAttrs = 0us

    for idx in changes do
      let x = idx % buf.Width
      let y = idx / buf.Width
      let termRow = y + rowOffset
      let cell = buf.Cells.[idx]

      if cell.Rune <> 0 then

        match termRow <> lastTerminalRow || x <> lastCol + 1 with
        | true -> Ansi.appendMoveCursor sb termRow x
        | false -> ()

        match cell.Fg <> lastFg || cell.Bg <> lastBg || cell.Attrs <> lastAttrs with
        | true ->
          Ansi.appendResetStyle sb
          Ansi.appendFgColorPacked sb cell.Fg
          Ansi.appendBgColorPacked sb cell.Bg
          Ansi.appendTextAttrsPacked sb cell.Attrs
          lastFg <- cell.Fg
          lastBg <- cell.Bg
          lastAttrs <- cell.Attrs
        | false -> ()

        let colWidth =
          match cell.Rune >= 0x20 && cell.Rune <= 0x7E with
          | true ->
            sb.Append(char cell.Rune) |> ignore
            1
          | false ->
            let rune = System.Text.Rune(cell.Rune)
            sb.Append(rune.ToString()) |> ignore
            RuneWidth.getColumnWidth rune

        lastCol <- x + colWidth - 1
        lastTerminalRow <- termRow

  /// Like presentAt, but offsets all row cursor positions by `rowOffset`.
  /// Use for inline (non-alt-screen) rendering where the frame starts below
  /// the current cursor position rather than at row 0.
  let presentAt (rowOffset: int) (changes: ResizeArray<int>) (buf: Buffer) =
    let sb = StringBuilder()
    presentAtInto rowOffset sb changes buf
    sb.ToString()

module ColorFallback =
  let redmeanDistance (r1: byte, g1: byte, b1: byte) (r2: byte, g2: byte, b2: byte) : float =
    let rMean = (float r1 + float r2) / 2.0
    let dr = float (int r1 - int r2)
    let dg = float (int g1 - int g2)
    let db = float (int b1 - int b2)
    sqrt ((2.0 + rMean / 256.0) * dr * dr + 4.0 * dg * dg + (2.0 + (255.0 - rMean) / 256.0) * db * db)

  let toAnsi256 (r: byte, g: byte, b: byte) : byte =
    let ri = int r * 5 / 255
    let gi = int g * 5 / 255
    let bi = int b * 5 / 255
    byte (16 + 36 * ri + 6 * gi + bi)

  let toBasic16 (r: byte, g: byte, b: byte) : BaseColor * Intensity =
    let ansiColors = [|
      (0uy, 0uy, 0uy), (Black, Normal)
      (128uy, 0uy, 0uy), (Red, Normal)
      (0uy, 128uy, 0uy), (Green, Normal)
      (128uy, 128uy, 0uy), (Yellow, Normal)
      (0uy, 0uy, 128uy), (Blue, Normal)
      (128uy, 0uy, 128uy), (Magenta, Normal)
      (0uy, 128uy, 128uy), (Cyan, Normal)
      (192uy, 192uy, 192uy), (White, Normal)
      (128uy, 128uy, 128uy), (Black, Bright)
      (255uy, 0uy, 0uy), (Red, Bright)
      (0uy, 255uy, 0uy), (Green, Bright)
      (255uy, 255uy, 0uy), (Yellow, Bright)
      (0uy, 0uy, 255uy), (Blue, Bright)
      (255uy, 0uy, 255uy), (Magenta, Bright)
      (0uy, 255uy, 255uy), (Cyan, Bright)
      (255uy, 255uy, 255uy), (White, Bright)
    |]
    ansiColors
    |> Array.minBy (fun ((ar, ag, ab), _) -> redmeanDistance (r, g, b) (ar, ag, ab))
    |> snd

  let resolve (profile: TerminalProfile) (color: Color) : Color =
    match color, profile.Color with
    | _, ColorCapability.TrueColor -> color
    | Rgb(r, g, b), ColorCapability.Indexed256 -> Ansi256(toAnsi256 (r, g, b))
    | Rgb(r, g, b), ColorCapability.Basic16 ->
      let (bc, i) = toBasic16 (r, g, b)
      Named(bc, i)
    | Rgb _, ColorCapability.NoColor -> Default
    | Ansi256 _, ColorCapability.Basic16 -> color
    | Ansi256 _, ColorCapability.NoColor -> Default
    | _ -> color

module SynchronizedOutput =
  let beginSync = "\x1b[?2026h"
  let endSync = "\x1b[?2026l"

  let wrap (profile: TerminalProfile) (output: string) : string =
    match profile.Output with
    | OutputCapability.SynchronizedOutput ->
      sprintf "%s%s%s" beginSync output endSync
    | _ -> output
