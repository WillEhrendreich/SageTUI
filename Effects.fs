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
