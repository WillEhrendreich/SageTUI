module Dashboard

// Real-time dashboard with clock, progress bars, event log, and multi-panel layout.
// Demonstrates: TimerSub, borders, fill/ratio layouts, colors, dynamic content.

open System
open SageTUI

type LogEntry = { Time: DateTime; Text: string; Level: string }

type Model =
  { Now: DateTime
    Cpu: float list
    Memory: int
    Events: LogEntry list
    Uptime: int }

type Msg =
  | Tick
  | Quit

let rng = Random(42)

let init () =
  { Now = DateTime.Now
    Cpu = [ 0.3; 0.5; 0.2; 0.8 ]
    Memory = 42
    Events =
      [ { Time = DateTime.Now; Text = "System started"; Level = "INFO" }
        { Time = DateTime.Now; Text = "Config loaded"; Level = "INFO" } ]
    Uptime = 0 },
  Cmd.none

let update msg model =
  match msg with
  | Tick ->
    let newCpu =
      model.Cpu
      |> List.map (fun c ->
        let delta = (rng.NextDouble() - 0.5) * 0.2
        min 1.0 (max 0.0 (c + delta)))
    let newMem = max 10 (min 90 (model.Memory + rng.Next(-3, 4)))
    let newEvent =
      match rng.Next(10) with
      | 0 -> Some { Time = DateTime.Now; Text = "Request handled /api/data"; Level = "INFO" }
      | 1 -> Some { Time = DateTime.Now; Text = "Cache miss: user-42"; Level = "WARN" }
      | 2 -> Some { Time = DateTime.Now; Text = "Health check OK"; Level = "INFO" }
      | 3 -> Some { Time = DateTime.Now; Text = "Connection pool: 8/20 active"; Level = "DEBUG" }
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
        Uptime = model.Uptime + 1 },
    Cmd.none
  | Quit -> model, Cmd.quit

let progressBar (width: int) (pct: float) (color: Color) =
  let filled = int (float width * pct)
  let empty = width - filled
  El.row [
    El.text (String('█', filled)) |> El.fg color
    El.text (String('░', empty)) |> El.dim
    El.text (sprintf " %3.0f%%" (pct * 100.0))
  ]

let levelColor level =
  match level with
  | "INFO" -> Color.Named(Green, Normal)
  | "WARN" -> Color.Named(Yellow, Bright)
  | "ERROR" -> Color.Named(Red, Bright)
  | "DEBUG" -> Color.Named(Blue, Bright)
  | _ -> Color.Default

let view model =
  let header =
    El.row [
      El.text " ◆ SageTUI Dashboard"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
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
          let color =
            match c with
            | c when c > 0.8 -> Color.Named(Red, Bright)
            | c when c > 0.5 -> Color.Named(Yellow, Normal)
            | _ -> Color.Named(Green, Normal)
          El.row [
            El.text (sprintf " Core %d " i) |> El.dim
            progressBar 20 c color
          ])
    ]
    |> El.bordered Light

  let memPanel =
    let pct = float model.Memory / 100.0
    let color =
      match model.Memory with
      | m when m > 80 -> Color.Named(Red, Bright)
      | m when m > 60 -> Color.Named(Yellow, Normal)
      | _ -> Color.Named(Green, Normal)
    El.column [
      El.text " Memory" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      El.row [
        El.text "   "
        progressBar 20 pct color
      ]
      El.text ""
      El.text (sprintf "   %d MB / 100 MB" model.Memory) |> El.dim
    ]
    |> El.bordered Light

  let uptimePanel =
    let h = model.Uptime / 3600
    let m = (model.Uptime % 3600) / 60
    let s = model.Uptime % 60
    El.column [
      El.text " Uptime" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      El.text (sprintf "   %02d:%02d:%02d" h m s)
        |> El.fg (Color.Named(Green, Bright))
    ]
    |> El.bordered Light

  let logPanel =
    El.column [
      El.text " Event Log" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
      El.text ""
      yield!
        model.Events
        |> List.map (fun e ->
          El.row [
            El.text (sprintf " %s " (e.Time.ToString("HH:mm:ss"))) |> El.dim
            El.text (sprintf "[%s]" e.Level)
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
let main _argv =
  let profile =
    Detect.fromEnvironment
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
      (fun () -> System.Console.WindowWidth, System.Console.WindowHeight)
    |> Detect.adjustForMultiplexer
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
    |> UserOverride.apply
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
  let backend = Backend.create profile
  App.run backend program
  0
