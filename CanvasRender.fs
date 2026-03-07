namespace SageTUI

module CanvasRender =
  let halfBlockChar = '▀'
  let lowerHalfBlockChar = '▄'

  let halfBlockTermWidth (pixelWidth: int) = pixelWidth
  let halfBlockTermHeight (pixelHeight: int) = (pixelHeight + 1) / 2

  let brailleTermWidth (pixelWidth: int) = (pixelWidth + 1) / 2
  let brailleTermHeight (pixelHeight: int) = (pixelHeight + 3) / 4

  let getPixel (pb: PixelBuffer) (x: int) (y: int) : Color option =
    match x >= 0 && x < pb.Width && y >= 0 && y < pb.Height with
    | true -> Some pb.Pixels.[y * pb.Width + x]
    | false -> None

  let halfBlockCell (pb: PixelBuffer) (col: int) (row: int) =
    let topColor = getPixel pb col (row * 2)
    let botColor = getPixel pb col (row * 2 + 1)
    match topColor, botColor with
    | Some top, Some bot when top = bot -> (' ', None, Some top)
    | Some top, Some bot -> (halfBlockChar, Some top, Some bot)
    | Some top, None -> (halfBlockChar, Some top, None)
    | None, Some bot -> (lowerHalfBlockChar, Some bot, None)
    | None, None -> (' ', None, None)

  let brailleDotBits = [| 0x01; 0x08; 0x02; 0x10; 0x04; 0x20; 0x40; 0x80 |]

  let brailleCell (pb: PixelBuffer) (col: int) (row: int) =
    let baseX = col * 2
    let baseY = row * 4
    let mutable pattern = 0
    let mutable firstColor = None
    for dy in 0..3 do
      for dx in 0..1 do
        match getPixel pb (baseX + dx) (baseY + dy) with
        | Some c when c <> Color.Default ->
          pattern <- pattern ||| brailleDotBits.[dy * 2 + dx]
          match firstColor with
          | None -> firstColor <- Some c
          | _ -> ()
        | _ -> ()
    (char (0x2800 + pattern), firstColor)

  let rec renderToBuffer (config: CanvasConfig) (area: Area) (buf: Buffer) =
    let pb = config.Draw area.Width (area.Height * 2)
    match config.Mode with
    | HalfBlock ->
      let termH = halfBlockTermHeight pb.Height
      for row in 0 .. min termH area.Height - 1 do
        for col in 0 .. min pb.Width area.Width - 1 do
          let (ch, fg, bg) = halfBlockCell pb col row
          let fgPacked =
            fg |> Option.map PackedColor.pack |> Option.defaultValue 0
          let bgPacked =
            bg |> Option.map PackedColor.pack |> Option.defaultValue 0
          let cell =
            PackedCell.create
              (int (System.Text.Rune(ch)).Value)
              fgPacked bgPacked 0us
          Buffer.set (area.X + col) (area.Y + row) cell buf
    | Braille ->
      let pb = config.Draw (area.Width * 2) (area.Height * 4)
      let termW = brailleTermWidth pb.Width
      let termH = brailleTermHeight pb.Height
      for row in 0 .. min termH area.Height - 1 do
        for col in 0 .. min termW area.Width - 1 do
          let (ch, color) = brailleCell pb col row
          let fgPacked =
            color |> Option.map PackedColor.pack |> Option.defaultValue 0
          let cell =
            PackedCell.create
              (int (System.Text.Rune(ch)).Value)
              fgPacked 0 0us
          Buffer.set (area.X + col) (area.Y + row) cell buf
    | PixelProtocol ->
      // PixelProtocol (e.g. Kitty, Sixel) — fall back to HalfBlock for now
      let fallbackConfig = { config with Mode = HalfBlock }
      renderToBuffer fallbackConfig area buf
