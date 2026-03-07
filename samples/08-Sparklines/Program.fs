module Sparklines

// Live Braille data visualization with OKLCH perceptual gradients.
// Demonstrates: Braille graphics, OKLCH color interpolation, TimerSub,
// ResizeSub, responsive layout, keyboard controls, complex composition.

open System
open SageTUI

// ─── Domain ───

type MetricKind = Cpu | Memory | Network | Disk

type Metric =
  { Kind: MetricKind
    Name: string
    Data: float list
    Low: byte * byte * byte
    High: byte * byte * byte }

type Model =
  { Width: int
    Height: int
    Tick: int
    Elapsed: int64
    Metrics: Metric list
    Paused: bool
    Focused: int option }

type Msg =
  | Tick
  | Resized of int * int
  | TogglePause
  | Reset
  | Focus of int
  | Unfocus
  | Quit

// ─── Data Generation ───

let inline clamp01 v = max 0.0 (min 1.0 v)

let generate (kind: MetricKind) (tick: int) : float =
  let t = float tick
  match kind with
  | Cpu ->
    let base' = 0.5 + 0.3 * sin (t * 0.08)
    let harmonic = 0.1 * sin (t * 0.23)
    let noise = 0.05 * sin (t * 1.1)
    clamp01 (base' + harmonic + noise)
  | Memory ->
    let cycle = tick % 300
    let ramp = 0.3 + 0.5 * (float cycle / 300.0)
    let gc = match cycle > 270 with true -> -0.35 * float (cycle - 270) / 30.0 | false -> 0.0
    clamp01 (ramp + gc)
  | Network ->
    let phase = tick % 40
    match phase < 12 with
    | true -> clamp01 (0.6 + 0.3 * sin (t * 0.4))
    | false -> clamp01 (0.05 + 0.03 * sin (t * 0.2))
  | Disk ->
    let step =
      match (tick / 60) % 4 with
      | 0 -> 0.8 | 1 -> 0.3 | 2 -> 0.6 | _ -> 0.15
    clamp01 (step + 0.08 * sin (t * 0.35))

// ─── OKLCH Color Helpers ───

let lerpColor (t: float) (lo: byte * byte * byte) (hi: byte * byte * byte) =
  let (r1, g1, b1) = lo
  let (r2, g2, b2) = hi
  let lch1 = Oklch.fromRgb r1 g1 b1
  let lch2 = Oklch.fromRgb r2 g2 b2
  let (l, c, h) = Oklch.lerp lch1 lch2 t
  let (r, g, b) = Oklch.toRgb l c h
  Rgb(r, g, b)

// ─── Braille Sparkline Renderer ───

let renderSparkline (chartW: int) (chartH: int) (lo: byte * byte * byte)
                    (hi: byte * byte * byte) (data: float list) : Element =
  let arr = data |> List.toArray
  let totalSub = chartH * 4

  // Build braille bit grid
  let grid = Array2D.zeroCreate chartH chartW
  let vals = Array.zeroCreate chartW

  for x in 0 .. chartW - 1 do
    let dataIdx = max 0 (arr.Length - chartW) + x
    let v =
      match dataIdx >= 0 && dataIdx < arr.Length with
      | true -> clamp01 arr[dataIdx]
      | false -> 0.0
    vals[x] <- v
    let fillTo = int (v * float (totalSub - 1))

    for p in 0 .. fillTo do
      let termRow = chartH - 1 - (p / 4)
      let dotRow = 3 - (p % 4)
      match termRow >= 0 && termRow < chartH with
      | true ->
        grid[termRow, x] <-
          grid[termRow, x]
          ||| (Braille.dotAt 0 dotRow)
          ||| (Braille.dotAt 1 dotRow)
      | false -> ()

  // Render row by row with per-character OKLCH coloring
  let rows =
    [ for r in 0 .. chartH - 1 do
        let rowT = float (chartH - 1 - r) / float (max 1 (chartH - 1))
        let chars =
          [ for x in 0 .. chartW - 1 do
              let bits = grid[r, x]
              let ch = Braille.toChar bits
              match bits > 0 with
              | true ->
                let t = (vals[x] + rowT) / 2.0
                El.text (string ch) |> El.fg (lerpColor t lo hi)
              | false ->
                El.text (string (Braille.toChar 0)) |> El.dim ]
        El.row chars ]
  El.column rows

// ─── Minibar (compact percentage indicator) ───

let minibar (width: int) (value: float) (lo: byte * byte * byte) (hi: byte * byte * byte) =
  let filled = int (float width * value)
  let empty = width - filled
  El.row [
    Gradient.oklch lo hi filled (String('━', filled))
    El.text (String('─', empty)) |> El.dim
  ]

// ─── TEA ───

let init () =
  let (w, h) = Console.WindowWidth, Console.WindowHeight
  { Width = w
    Height = h
    Tick = 0
    Elapsed = 0L
    Metrics =
      [ { Kind = Cpu; Name = "CPU"; Data = []; Low = (40uy, 200uy, 80uy); High = (255uy, 60uy, 50uy) }
        { Kind = Memory; Name = "MEM"; Data = []; Low = (60uy, 120uy, 255uy); High = (220uy, 80uy, 255uy) }
        { Kind = Network; Name = "NET"; Data = []; Low = (255uy, 200uy, 40uy); High = (255uy, 80uy, 20uy) }
        { Kind = Disk; Name = "DSK"; Data = []; Low = (40uy, 220uy, 220uy); High = (40uy, 80uy, 255uy) } ]
    Paused = false
    Focused = None },
  Cmd.none

let update msg model =
  match msg with
  | Tick ->
    match model.Paused with
    | true -> { model with Elapsed = model.Elapsed + 100L }, Cmd.none
    | false ->
      let tick = model.Tick + 1
      let maxPts = max 20 model.Width
      let metrics =
        model.Metrics
        |> List.map (fun m ->
          let v = generate m.Kind tick
          let data = m.Data @ [v]
          let data =
            match data.Length > maxPts with
            | true -> data |> List.skip (data.Length - maxPts)
            | false -> data
          { m with Data = data })
      { model with Tick = tick; Elapsed = model.Elapsed + 100L; Metrics = metrics }, Cmd.none
  | Resized (w, h) -> { model with Width = w; Height = h }, Cmd.none
  | TogglePause -> { model with Paused = not model.Paused }, Cmd.none
  | Reset ->
    { model with
        Tick = 0
        Metrics = model.Metrics |> List.map (fun m -> { m with Data = [] }) },
    Cmd.none
  | Focus idx -> { model with Focused = Some idx }, Cmd.none
  | Unfocus -> { model with Focused = None }, Cmd.none
  | Quit -> model, Cmd.quit

let view model =
  let elapsed = TimeSpan.FromMilliseconds(float model.Elapsed)
  let timeStr = sprintf "%02d:%02d:%02d" (int elapsed.TotalHours) elapsed.Minutes elapsed.Seconds

  // Header
  let header =
    El.row [
      El.text " ⣿ " |> El.fg (Rgb(180uy, 140uy, 255uy)) |> El.bold
      Gradient.oklch (140uy, 100uy, 255uy) (80uy, 220uy, 200uy) 11 "Sparklines "
      El.text "— Braille Data Viz " |> El.dim
      El.fill (El.text "")
      match model.Paused with
      | true -> El.text " ⏸ PAUSED " |> El.fg (Rgb(255uy, 200uy, 50uy)) |> El.bold
      | false -> El.empty
      El.text timeStr |> El.fg (Rgb(100uy, 110uy, 130uy))
      El.text " " ]

  // Footer
  let footer =
    El.row [
      El.text " "
      El.text "Q" |> El.bold |> El.fg (Rgb(255uy, 100uy, 100uy))
      El.text "uit  " |> El.dim
      El.text "P" |> El.bold |> El.fg (Rgb(100uy, 200uy, 255uy))
      El.text "ause  " |> El.dim
      El.text "R" |> El.bold |> El.fg (Rgb(100uy, 255uy, 150uy))
      El.text "eset  " |> El.dim
      El.text "1-4" |> El.bold |> El.fg (Rgb(255uy, 200uy, 100uy))
      El.text " Focus " |> El.dim ]

  // Metric panel renderer
  let renderPanel (idx: int) (m: Metric) =
    let isFocused = model.Focused = Some idx
    let current = match m.Data with [] -> 0.0 | data -> List.last data
    let pct = sprintf "%3.0f%%" (current * 100.0)

    // Calculate chart dimensions
    let panelW =
      match model.Focused with
      | Some fi when fi = idx -> model.Width - 2
      | Some _ -> 0
      | None -> (model.Width - 4) / 2

    let chartH =
      match model.Focused with
      | Some fi when fi = idx -> model.Height - 8
      | _ -> (model.Height - 8) / 2

    match panelW > 6 && chartH > 2 with
    | false -> El.empty
    | true ->
      let sparkW = max 1 (panelW - 4)
      let sparkH = max 1 (chartH - 4)

      // Title row with name + value
      let title =
        El.row [
          El.text (sprintf " %s " m.Name) |> El.bold |> El.fg (lerpColor 0.8 m.Low m.High)
          El.text pct |> El.bold |> El.fg (lerpColor current m.Low m.High)
          El.text "" |> El.fill
          match isFocused with
          | true -> El.text "ESC " |> El.dim
          | false -> El.empty ]

      // Mini bar under title
      let bar = minibar (min sparkW 30) current m.Low m.High

      // Sparkline chart
      let chart = renderSparkline sparkW sparkH m.Low m.High m.Data

      El.column [ title; bar; El.text ""; chart ]
      |> El.padHV 1 0
      |> El.bordered (match isFocused with true -> Double | false -> Rounded)

  // Layout
  let content =
    match model.Focused with
    | Some idx ->
      match idx >= 0 && idx < model.Metrics.Length with
      | true -> renderPanel idx model.Metrics[idx]
      | false -> El.empty
    | None ->
      let panels = model.Metrics |> List.mapi renderPanel
      match panels.Length >= 4 with
      | true ->
        El.column [
          El.row [ panels[0] |> El.fill; panels[1] |> El.fill ] |> El.fill
          El.row [ panels[2] |> El.fill; panels[3] |> El.fill ] |> El.fill ]
      | false -> El.column panels

  El.column [ header; content |> El.fill; footer ]

let subscribe _model =
  [ TimerSub("tick", TimeSpan.FromMilliseconds(100.0), fun () -> Tick)
    ResizeSub (fun (w, h) -> Resized (w, h))
    Keys.bind [
      Key.Char 'q', Quit
      Key.Char 'Q', Quit
      Key.Escape, Quit
      Key.Char 'p', TogglePause
      Key.Char 'P', TogglePause
      Key.Char 'r', Reset
      Key.Char 'R', Reset
      Key.Char '1', Focus 0
      Key.Char '2', Focus 1
      Key.Char '3', Focus 2
      Key.Char '4', Focus 3
    ] ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _ = App.run program; 0