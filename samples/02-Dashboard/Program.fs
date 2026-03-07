module Dashboard

// Real-time dashboard with clock, progress bars, event log, and multi-panel layout.
// Demonstrates: TimerSub, ResizeSub, borders, fill/ratio, colors, Gradient, Spinner.

open System
open SageTUI

type LogLevel = Info | Warn | Error | Debug

type LogEntry = { Time: DateTime; Text: string; Level: LogLevel }

type Model =
  { Now: DateTime
    Cpu: float list
    Memory: int
    Events: LogEntry list
    Uptime: int
    Seed: int
    BarWidth: int
    Elapsed: int64 }

type Msg =
  | Tick
  | Resized of int * int
  | Quit

let init () =
  { Now = DateTime.Now
    Cpu = [ 0.3; 0.5; 0.2; 0.8 ]
    Memory = 42
    Events =
      [ { Time = DateTime.Now; Text = "System started"; Level = Info }
        { Time = DateTime.Now; Text = "Config loaded"; Level = Info } ]
    Uptime = 0
    Seed = 42
    BarWidth = 20
    Elapsed = 0L },
  Cmd.none

let levelColor level =
  match level with
  | Info -> Color.Named(Green, Normal)
  | Warn -> Color.Named(Yellow, Bright)
  | Error -> Color.Named(Red, Bright)
  | Debug -> Color.Named(Blue, Bright)

let levelTag level =
  match level with
  | Info -> "INFO" | Warn -> "WARN" | Error -> "ERROR" | Debug -> "DEBUG"

let update msg model =
  match msg with
  | Tick ->
    let rng = Random(model.Seed)
    let newCpu =
      model.Cpu
      |> List.map (fun c ->
        let delta = (rng.NextDouble() - 0.5) * 0.2
        min 1.0 (max 0.0 (c + delta)))
    let newMem = max 10 (min 90 (model.Memory + rng.Next(-3, 4)))
    let newEvent =
      match rng.Next(10) with
      | 0 -> Some { Time = DateTime.Now; Text = "Request handled /api/data"; Level = Info }
      | 1 -> Some { Time = DateTime.Now; Text = "Cache miss: user-42"; Level = Warn }
      | 2 -> Some { Time = DateTime.Now; Text = "Health check OK"; Level = Info }
      | 3 -> Some { Time = DateTime.Now; Text = "Connection pool: 8/20 active"; Level = Debug }
      | 4 -> Some { Time = DateTime.Now; Text = "Timeout on upstream service"; Level = Error }
      | _ -> None
    let events =
      match newEvent with
      | Some e -> (e :: model.Events) |> List.truncate 8
      | None -> model.Events
    { model with
        Now = DateTime.Now
        Cpu = newCpu
        Memory = newMem
        Events = events
        Uptime = model.Uptime + 1
        Seed = model.Seed + 1
        Elapsed = model.Elapsed + 1000L },
    Cmd.none
  | Resized (w, _) ->
    { model with BarWidth = max 10 (w / 4 - 10) }, Cmd.none
  | Quit -> model, Cmd.quit

let progressBar (width: int) (pct: float) =
  let filled = int (float width * pct)
  let empty = width - filled
  let startColor =
    match pct with
    | p when p > 0.8 -> (255uy, 60uy, 60uy)
    | p when p > 0.5 -> (255uy, 200uy, 50uy)
    | _ -> (80uy, 220uy, 100uy)
  let endColor =
    match pct with
    | p when p > 0.8 -> (180uy, 30uy, 30uy)
    | p when p > 0.5 -> (200uy, 150uy, 30uy)
    | _ -> (40uy, 180uy, 60uy)
  El.row [
    Gradient.horizontal startColor endColor filled (String('█', filled))
    El.text (String('░', empty)) |> El.dim
    El.text $" {pct * 100.0:F0}%%"
  ]

let view model =
  let header =
    El.row [
      El.text " ◆ SageTUI Dashboard"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.text "  "
      Spinner.dots model.Elapsed
        |> El.fg (Color.Named(Green, Bright))
      El.fill (El.text "")
      El.text (model.Now.ToString("HH:mm:ss"))
        |> El.fg (Color.Named(Yellow, Bright))
      El.text " "
    ]
    |> El.bg (Color.Named(Black, Normal))

  let cpuPanel =
    El.column [
      El.text " CPU Usage" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      yield!
        model.Cpu
        |> List.mapi (fun i c ->
          El.row [
            El.text $" Core {i} " |> El.dim
            progressBar model.BarWidth c
          ])
    ]
    |> El.bordered Light

  let memPanel =
    let pct = float model.Memory / 100.0
    El.column [
      El.text " Memory" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      El.row [
        El.text "   "
        progressBar model.BarWidth pct
      ]
      El.text ""
      El.text $"   {model.Memory} MB / 100 MB" |> El.dim
    ]
    |> El.bordered Light

  let uptimePanel =
    let h = model.Uptime / 3600
    let m = (model.Uptime % 3600) / 60
    let s = model.Uptime % 60
    El.column [
      El.text " Uptime" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      El.row [
        El.text "   "
        El.text $"{h:D2}:{m:D2}:{s:D2}"
          |> El.fg (Color.Named(Green, Bright))
        El.text "  "
        Spinner.line model.Elapsed |> El.dim
      ]
    ]
    |> El.bordered Light

  let logPanel =
    El.column [
      El.text " Event Log" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      yield!
        model.Events
        |> List.map (fun e ->
          let timeStr = e.Time.ToString("HH:mm:ss")
          El.row [
            El.text (sprintf " %s " timeStr) |> El.dim
            El.text (sprintf "[%s]" (levelTag e.Level))
              |> El.fg (levelColor e.Level)
              |> El.bold
            El.text (sprintf " %s" e.Text)
          ])
    ]
    |> El.bordered Light

  let footer =
    El.text " [q] Quit" |> El.dim

  El.column [
    header
    El.row [
      El.ratio 1 2 cpuPanel
      El.ratio 1 2 (
        El.column [
          memPanel
          uptimePanel
        ])
    ]
    |> El.fill
    logPanel |> El.fill
    footer
  ]

let subscribe _model =
  [ TimerSub("tick", TimeSpan.FromSeconds(1.0), fun () -> Tick)
    ResizeSub (fun (w, h) -> Resized(w, h))
    KeySub (fun (key, _mods) ->
      match key with
      | Char 'q' | Char 'Q' | Escape -> Some Quit
      | _ -> None) ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _ = App.run program; 0