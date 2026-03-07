namespace SageTUI

open System.Text

module Ansi =
  let esc = "\x1b["

  let moveCursor row col = sprintf "%s%d;%dH" esc (row + 1) (col + 1)
  let hideCursor = sprintf "%s?25l" esc
  let showCursor = sprintf "%s?25h" esc
  let enterAltScreen = sprintf "%s?1049h" esc
  let leaveAltScreen = sprintf "%s?1049l" esc
  let enableMouseTracking = sprintf "%s?1000h%s?1006h" esc esc
  let disableMouseTracking = sprintf "%s?1000l%s?1006l" esc esc
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

module Presenter =
  let present (changes: ResizeArray<int>) (buf: Buffer) =
    let sb = StringBuilder()
    let mutable lastRow = -1
    let mutable lastCol = -1
    let mutable lastFg = 0
    let mutable lastBg = 0
    let mutable lastAttrs = 0us

    for idx in changes do
      let x = idx % buf.Width
      let y = idx / buf.Width
      let cell = buf.Cells.[idx]

      match y <> lastRow || x <> lastCol + 1 with
      | true -> sb.Append(Ansi.moveCursor y x) |> ignore
      | false -> ()

      match cell.Fg <> lastFg || cell.Bg <> lastBg || cell.Attrs <> lastAttrs with
      | true ->
        sb.Append(Ansi.resetStyle) |> ignore
        sb.Append(Ansi.fgColorPacked cell.Fg) |> ignore
        sb.Append(Ansi.bgColorPacked cell.Bg) |> ignore
        sb.Append(Ansi.textAttrsPacked cell.Attrs) |> ignore
        lastFg <- cell.Fg
        lastBg <- cell.Bg
        lastAttrs <- cell.Attrs
      | false -> ()

      let rune = System.Text.Rune(cell.Rune)
      sb.Append(rune.ToString()) |> ignore
      lastCol <- x + (RuneWidth.getColumnWidth rune) - 1
      lastRow <- y

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
