namespace SageTUI

type Intensity = Normal | Bright

type BaseColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type Color =
  | Default
  | Named of BaseColor * Intensity
  | Ansi256 of byte
  | Rgb of r: byte * g: byte * b: byte

type TextAttrs = { Value: uint16 }

module TextAttrs =
  let none      = { Value = 0us }
  let bold      = { Value = 1us }
  let dim       = { Value = 2us }
  let italic    = { Value = 4us }
  let underline = { Value = 8us }
  let blink     = { Value = 16us }
  let reverse   = { Value = 32us }
  let hidden    = { Value = 64us }
  let strikethrough = { Value = 128us }
  let combine a b = { Value = a.Value ||| b.Value }
  let has flag attrs = (attrs.Value &&& flag.Value) <> 0us
  let remove flag attrs = { Value = attrs.Value &&& (~~~flag.Value) }

type Style = { Fg: Color option; Bg: Color option; Attrs: TextAttrs }

module Style =
  let empty = { Fg = None; Bg = None; Attrs = TextAttrs.none }

  let merge base' overlay =
    { Fg = overlay.Fg |> Option.orElse base'.Fg
      Bg = overlay.Bg |> Option.orElse base'.Bg
      Attrs = TextAttrs.combine base'.Attrs overlay.Attrs }

  let withFg color style = { style with Fg = Some color }
  let withBg color style = { style with Bg = Some color }

  let withBold style =
    { style with Attrs = TextAttrs.combine style.Attrs TextAttrs.bold }

  let withItalic style =
    { style with Attrs = TextAttrs.combine style.Attrs TextAttrs.italic }

  let withUnderline style =
    { style with Attrs = TextAttrs.combine style.Attrs TextAttrs.underline }

module Color =
  let black = Named(BaseColor.Black, Normal)
  let red = Named(BaseColor.Red, Normal)
  let green = Named(BaseColor.Green, Normal)
  let yellow = Named(BaseColor.Yellow, Normal)
  let blue = Named(BaseColor.Blue, Normal)
  let magenta = Named(BaseColor.Magenta, Normal)
  let cyan = Named(BaseColor.Cyan, Normal)
  let white = Named(BaseColor.White, Normal)
  let brightBlack = Named(BaseColor.Black, Bright)
  let brightRed = Named(BaseColor.Red, Bright)
  let brightGreen = Named(BaseColor.Green, Bright)
  let brightYellow = Named(BaseColor.Yellow, Bright)
  let brightBlue = Named(BaseColor.Blue, Bright)
  let brightMagenta = Named(BaseColor.Magenta, Bright)
  let brightCyan = Named(BaseColor.Cyan, Bright)
  let brightWhite = Named(BaseColor.White, Bright)
  let rgb r g b = Rgb(r, g, b)

  let hex (s: string) =
    try
      let s = match s.StartsWith('#') with true -> s.Substring(1) | false -> s
      match s.Length with
      | 3 ->
        let r = System.Convert.ToByte(System.String(s.[0], 2), 16)
        let g = System.Convert.ToByte(System.String(s.[1], 2), 16)
        let b = System.Convert.ToByte(System.String(s.[2], 2), 16)
        Rgb(r, g, b)
      | 6 ->
        let r = System.Convert.ToByte(s.[0..1], 16)
        let g = System.Convert.ToByte(s.[2..3], 16)
        let b = System.Convert.ToByte(s.[4..5], 16)
        Rgb(r, g, b)
      | _ -> Default
    with _ -> Default

module PackedColor =
  let pack (c: Color) : int =
    match c with
    | Default -> 0
    | Named(bc, i) ->
      let bcIdx =
        match bc with
        | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
        | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
      let iIdx = match i with Normal -> 0 | Bright -> 1
      1 ||| (bcIdx <<< 2) ||| (iIdx <<< 5)
    | Ansi256 b -> 2 ||| (int b <<< 8)
    | Rgb(r, g, b) ->
      3 ||| (int r <<< 8) ||| (int g <<< 16) ||| (int b <<< 24)

  let unpack (packed: int) : Color =
    let tag = packed &&& 0x3
    match tag with
    | 0 -> Default
    | 1 ->
      let bcIdx = (packed >>> 2) &&& 0x7
      let iIdx = (packed >>> 5) &&& 0x1
      let bc =
        match bcIdx with
        | 0 -> Black | 1 -> Red | 2 -> Green | 3 -> Yellow
        | 4 -> Blue | 5 -> Magenta | 6 -> Cyan | _ -> White
      let i = match iIdx with 0 -> Normal | _ -> Bright
      Named(bc, i)
    | 2 -> Ansi256 (byte ((packed >>> 8) &&& 0xFF))
    | _ ->
      Rgb(
        byte ((packed >>> 8) &&& 0xFF),
        byte ((packed >>> 16) &&& 0xFF),
        byte ((packed >>> 24) &&& 0xFF))
