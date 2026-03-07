namespace SageTUI

module Ease =
  let linear: Easing = id

  let quadOut: Easing = fun t ->
    1.0 - (1.0 - t) * (1.0 - t)

  let quadIn: Easing = fun t -> t * t

  let cubicInOut: Easing = fun t ->
    match t < 0.5 with
    | true -> 4.0 * t * t * t
    | false -> 1.0 - pown (-2.0 * t + 2.0) 3 / 2.0

module Braille =
  let dotBits = [| 0x01; 0x02; 0x04; 0x40; 0x08; 0x10; 0x20; 0x80 |]
  let brailleBase = 0x2800

  let dotAt col row =
    match col >= 0 && col <= 1 && row >= 0 && row <= 3 with
    | true -> dotBits[col * 4 + row]
    | false -> 0

  let toChar bits = char (brailleBase + bits)

module Alpha =
  let lerpByte (t: float) (a: byte) (b: byte) : byte =
    let result = float a + t * (float b - float a)
    byte (System.Math.Clamp(result, 0.0, 255.0))

  let blendColor (t: float) (bg: Color) (fg: Color) : Color =
    match bg, fg with
    | Rgb(br, bg', bb), Rgb(fr, fg', fb) ->
      Rgb(lerpByte t br fr, lerpByte t bg' fg', lerpByte t bb fb)
    | _, _ ->
      match t >= 0.5 with
      | true -> fg
      | false -> bg

module Anim =
  let lerp (a: float) (b: float) (t: float) = a + (b - a) * t
  let lerpInt (a: int) (b: int) (t: float) = int (float a + float (b - a) * t)
  let progress (startMs: int64) (nowMs: int64) (durationMs: int) =
    let elapsed = float (nowMs - startMs)
    min 1.0 (max 0.0 (elapsed / float durationMs))
  let isDone (startMs: int64) (nowMs: int64) (durationMs: int) =
    nowMs - startMs >= int64 durationMs

module Oklch =
  open System

  let private srgbToLinear (c: float) =
    match c <= 0.04045 with
    | true -> c / 12.92
    | false -> ((c + 0.055) / 1.055) ** 2.4

  let private linearToSrgb (c: float) =
    match c <= 0.0031308 with
    | true -> c * 12.92
    | false -> 1.055 * (c ** (1.0 / 2.4)) - 0.055

  let private byteToLinear (b: byte) = srgbToLinear (float b / 255.0)
  let private linearToByte (f: float) =
    linearToSrgb f |> fun v -> Math.Clamp(v * 255.0, 0.0, 255.0) |> Math.Round |> byte

  let fromRgb (r: byte) (g: byte) (b: byte) =
    let lr = byteToLinear r
    let lg = byteToLinear g
    let lb = byteToLinear b
    // Forward M1: linear sRGB -> LMS (Ottosson)
    let lp = 0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb
    let mp = 0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb
    let sp = 0.0883024619 * lr + 0.2024326433 * lg + 0.6892648948 * lb
    let l = Math.Cbrt lp
    let m = Math.Cbrt mp
    let s = Math.Cbrt sp
    // Forward M2: LMS cube root -> OKLab
    let capL = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
    let a = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
    let bVal = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
    // OKLab -> OKLCH (polar)
    let capC = sqrt (a * a + bVal * bVal)
    let capH = Math.Atan2(bVal, a) * (180.0 / Math.PI)
    let capH = match capH < 0.0 with true -> capH + 360.0 | false -> capH
    (capL, capC, capH)

  let toRgb (capL: float) (capC: float) (capH: float) =
    let hRad = capH * (Math.PI / 180.0)
    let a = capC * cos hRad
    let bOklab = capC * sin hRad
    // Inverse M2: OKLab -> LMS cube root
    let l = capL + 0.3963377774 * a + 0.2158037573 * bOklab
    let m = capL - 0.1055613458 * a - 0.0638541728 * bOklab
    let s = capL - 0.0894841775 * a - 1.2914855480 * bOklab
    let lp = l * l * l
    let mp = m * m * m
    let sp = s * s * s
    // True inverse M1: LMS -> linear sRGB (analytically computed)
    let r =  4.0562053658179495 * lp - 3.2568173945365637 * mp + 0.2047061514213596 * sp
    let g = -1.2380901746958464 * lp + 2.5345477129513907 * mp - 0.3025076918388671 * sp
    let bVal = -0.1560257222986546 * lp - 0.3271459215572362 * mp + 1.5134404528807921 * sp
    (linearToByte r, linearToByte g, linearToByte bVal)

  let lerp (l1, c1, h1) (l2, c2, h2) (t: float) =
    let capL = l1 + (l2 - l1) * t
    let capC = c1 + (c2 - c1) * t
    let dh = h2 - h1
    let dh =
      match dh with
      | d when d > 180.0 -> d - 360.0
      | d when d < -180.0 -> d + 360.0
      | d -> d
    let capH = (h1 + dh * t) % 360.0
    let capH = match capH < 0.0 with true -> capH + 360.0 | false -> capH
    (capL, capC, capH)

module Gradient =
  let lerpRgb (r1, g1, b1) (r2, g2, b2) (t: float) =
    (Anim.lerpInt (int r1) (int r2) t |> byte,
     Anim.lerpInt (int g1) (int g2) t |> byte,
     Anim.lerpInt (int b1) (int b2) t |> byte)

  let horizontal (startColor: byte * byte * byte) (endColor: byte * byte * byte)
                  (width: int) (text: string) =
    text |> Seq.mapi (fun i c ->
      let t = float i / float (max 1 (width - 1))
      let (r, g, b) = lerpRgb startColor endColor t
      El.text (string c) |> El.fg (Rgb(r, g, b)))
    |> Seq.toList |> El.row

  let hueToRgb (h: float) =
    let h = h % 360.0
    let c = 1.0
    let x = 1.0 - abs ((h / 60.0) % 2.0 - 1.0)
    let (r, g, b) =
      match h with
      | h when h < 60.0 -> (c, x, 0.0)
      | h when h < 120.0 -> (x, c, 0.0)
      | h when h < 180.0 -> (0.0, c, x)
      | h when h < 240.0 -> (0.0, x, c)
      | h when h < 300.0 -> (x, 0.0, c)
      | _ -> (c, 0.0, x)
    (byte (r * 255.0), byte (g * 255.0), byte (b * 255.0))

  let rainbow (width: int) (text: string) =
    text |> Seq.mapi (fun i c ->
      let hue = float i / float (max 1 (width - 1)) * 360.0
      let (r, g, b) = hueToRgb hue
      El.text (string c) |> El.fg (Rgb(r, g, b)))
    |> Seq.toList |> El.row

  let oklch (startColor: byte * byte * byte) (endColor: byte * byte * byte)
            (width: int) (text: string) =
    let (r1, g1, b1) = startColor
    let (r2, g2, b2) = endColor
    let lch1 = Oklch.fromRgb r1 g1 b1
    let lch2 = Oklch.fromRgb r2 g2 b2
    text |> Seq.mapi (fun i c ->
      let t = float i / float (max 1 (width - 1))
      let (capL, capC, capH) = Oklch.lerp lch1 lch2 t
      let (r, g, b) = Oklch.toRgb capL capC capH
      El.text (string c) |> El.fg (Rgb(r, g, b)))
    |> Seq.toList |> El.row

module Spinner =
  let frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
  let dots (elapsed: int64) =
    let idx = int (elapsed / 80L) % frames.Length
    El.text frames.[idx]

  let lineFrames = [| "-"; "\\"; "|"; "/" |]
  let line (elapsed: int64) =
    let idx = int (elapsed / 100L) % lineFrames.Length
    El.text lineFrames.[idx]

module Blur =
  let desaturate (strength: float) (c: Color) : Color =
    match c with
    | Rgb(r, g, b) ->
      let gray = byte (float r * 0.299 + float g * 0.587 + float b * 0.114)
      Rgb(Alpha.lerpByte strength r gray,
          Alpha.lerpByte strength g gray,
          Alpha.lerpByte strength b gray)
    | other -> other

  let degradeChar (strength: float) (rune: int) : int =
    match strength with
    | s when s > 0.8 -> int ' '
    | s when s > 0.6 -> int '░'
    | s when s > 0.4 -> int '▒'
    | _ -> rune
