module CanvasTests

open Expecto
open Expecto.Flip
open SageTUI

let red = Color.Rgb(255uy, 0uy, 0uy)
let green = Color.Rgb(0uy, 255uy, 0uy)
let blue = Color.Rgb(0uy, 0uy, 255uy)
let white = Color.Rgb(255uy, 255uy, 255uy)
let mkPB w h px = { Width = w; Height = h; Pixels = px }

let halfBlockDimensionTests = testList "HalfBlock.dimensions" [
  test "term width = pixel width" {
    CanvasRender.halfBlockTermWidth 10 |> Expect.equal "width" 10
  }
  test "even height halves" {
    CanvasRender.halfBlockTermHeight 10 |> Expect.equal "height" 5
  }
  test "odd height rounds up" {
    CanvasRender.halfBlockTermHeight 11 |> Expect.equal "height" 6
  }
  test "zero dimensions" {
    CanvasRender.halfBlockTermWidth 0 |> Expect.equal "width" 0
    CanvasRender.halfBlockTermHeight 0 |> Expect.equal "height" 0
  }
  test "single pixel height" {
    CanvasRender.halfBlockTermHeight 1 |> Expect.equal "height" 1
  }
]

let halfBlockCellTests = testList "HalfBlock.cell" [
  test "same colors produce space with bg" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 1 2 [|red; red|]) 0 0
    ch |> Expect.equal "char" ' '
    fg |> Expect.isNone "no fg"
    bg |> Expect.equal "bg" (Some red)
  }
  test "different colors produce half-block" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 1 2 [|red; blue|]) 0 0
    ch |> Expect.equal "char" '▀'
    fg |> Expect.equal "fg" (Some red)
    bg |> Expect.equal "bg" (Some blue)
  }
  test "top only produces half-block with fg" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 1 1 [|green|]) 0 0
    ch |> Expect.equal "char" '▀'
    fg |> Expect.equal "fg" (Some green)
    bg |> Expect.isNone "no bg"
  }
  test "out of bounds produces space" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 1 1 [|red|]) 5 5
    ch |> Expect.equal "char" ' '
    fg |> Expect.isNone "no fg"
    bg |> Expect.isNone "no bg"
  }
  test "2x2 grid top-left cell" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 2 2 [|red; green; blue; white|]) 0 0
    ch |> Expect.equal "char" '▀'
    fg |> Expect.equal "fg" (Some red)
    bg |> Expect.equal "bg" (Some blue)
  }
  test "2x2 grid top-right cell" {
    let (ch, fg, bg) = CanvasRender.halfBlockCell (mkPB 2 2 [|red; green; blue; white|]) 1 0
    ch |> Expect.equal "char" '▀'
    fg |> Expect.equal "fg" (Some green)
    bg |> Expect.equal "bg" (Some white)
  }
]

let brailleDimensionTests = testList "Braille.dimensions" [
  test "even width halves" {
    CanvasRender.brailleTermWidth 10 |> Expect.equal "width" 5
  }
  test "odd width rounds up" {
    CanvasRender.brailleTermWidth 11 |> Expect.equal "width" 6
  }
  test "height divides by 4" {
    CanvasRender.brailleTermHeight 8 |> Expect.equal "height" 2
  }
  test "height rounds up" {
    CanvasRender.brailleTermHeight 5 |> Expect.equal "height" 2
  }
  test "single pixel dimensions" {
    CanvasRender.brailleTermWidth 1 |> Expect.equal "width" 1
    CanvasRender.brailleTermHeight 1 |> Expect.equal "height" 1
  }
]

let brailleCellTests = testList "Braille.cell" [
  test "empty buffer produces empty braille" {
    let (ch, c) = CanvasRender.brailleCell (mkPB 2 4 (Array.create 8 Color.Default)) 0 0
    ch |> Expect.equal "char" '\u2800'
    c |> Expect.isNone "no color"
  }
  test "single dot top-left" {
    let px = Array.create 8 Color.Default
    px.[0] <- red
    let (ch, c) = CanvasRender.brailleCell (mkPB 2 4 px) 0 0
    ch |> Expect.equal "char" '\u2801'
    c |> Expect.equal "color" (Some red)
  }
  test "all dots set" {
    let (ch, c) = CanvasRender.brailleCell (mkPB 2 4 (Array.create 8 green)) 0 0
    ch |> Expect.equal "char" '\u28FF'
    c |> Expect.equal "color" (Some green)
  }
  test "diagonal pattern" {
    let px = Array.create 8 Color.Default
    px.[0] <- blue; px.[3] <- blue; px.[4] <- blue; px.[7] <- blue
    let (ch, c) = CanvasRender.brailleCell (mkPB 2 4 px) 0 0
    ch |> Expect.equal "char" (char (0x2800 + 0x95))
    c |> Expect.equal "color" (Some blue)
  }
]

let renderToBufferTests = testList "renderToBuffer" [
  test "HalfBlock renders solid color to buffer" {
    let config = {
      Draw = fun w h -> mkPB w h (Array.create (w * h) red)
      Mode = HalfBlock
      Fallback = None
    }
    let area = { X = 0; Y = 0; Width = 4; Height = 3 }
    let buf = Buffer.create 4 3
    CanvasRender.renderToBuffer config area buf
    let cell = buf.Cells.[0]
    cell.Bg |> Expect.notEqual "bg set" 0
  }
  test "HalfBlock alternating rows" {
    let config = {
      Draw = fun w h ->
        let px = Array.init (w * h) (fun i ->
          match (i / w) % 2 with
          | 0 -> red
          | _ -> blue)
        mkPB w h px
      Mode = HalfBlock
      Fallback = None
    }
    let area = { X = 0; Y = 0; Width = 2; Height = 1 }
    let buf = Buffer.create 2 1
    CanvasRender.renderToBuffer config area buf
    let cell = buf.Cells.[0]
    // Row 0: top=red, bottom=blue → ▀ with fg=red, bg=blue
    cell.Fg |> Expect.notEqual "fg set" 0
    cell.Bg |> Expect.notEqual "bg set" 0
    cell.Rune |> Expect.equal "half-block char" (int (System.Text.Rune('▀')).Value)
  }
  test "Braille renders to buffer" {
    let config = {
      Draw = fun w h -> mkPB w h (Array.create (w * h) green)
      Mode = Braille
      Fallback = None
    }
    let area = { X = 0; Y = 0; Width = 2; Height = 2 }
    let buf = Buffer.create 2 2
    CanvasRender.renderToBuffer config area buf
    let cell = buf.Cells.[0]
    cell.Rune |> Expect.equal "full braille" (int (System.Text.Rune('\u28FF')).Value)
  }
  test "Canvas element renders via Render.render" {
    let config = {
      Draw = fun w h -> mkPB w h (Array.create (w * h) red)
      Mode = HalfBlock
      Fallback = None
    }
    let elem = Canvas config
    let area = { X = 0; Y = 0; Width = 4; Height = 2 }
    let buf = Buffer.create 4 2
    let style = { Fg = None; Bg = None; Attrs = { Value = 0us } }
    Render.render area style buf elem
    // Should have written something (not all empty)
    let hasContent =
      buf.Cells |> Array.exists (fun c -> c.Fg <> 0 || c.Bg <> 0)
    hasContent |> Expect.isTrue "buffer has canvas content"
  }
  test "Canvas with offset renders correctly" {
    let config = {
      Draw = fun w h -> mkPB w h (Array.create (w * h) blue)
      Mode = HalfBlock
      Fallback = None
    }
    let area = { X = 2; Y = 1; Width = 3; Height = 2 }
    let buf = Buffer.create 8 5
    CanvasRender.renderToBuffer config area buf
    // Cell at (0,0) should be empty
    buf.Cells.[0].Bg |> Expect.equal "origin empty" 0
    // Cell at (2,1) should have content
    buf.Cells.[1 * 8 + 2].Bg |> Expect.notEqual "offset cell has bg" 0
  }
]

[<Tests>]
let allCanvasTests = testList "Canvas" [
  halfBlockDimensionTests
  halfBlockCellTests
  brailleDimensionTests
  brailleCellTests
  renderToBufferTests
]
