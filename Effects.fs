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
