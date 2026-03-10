module Dashboard

// Real-time dashboard with gradient header, services panel, animated metrics, and event log.
// Demonstrates: TimerSub, ResizeSub, borders, fill/ratio, colors, Gradient, Spinner, ServiceStatus DU.

open System
open SageTUI

// ── Domain Types ──────────────────────────────────────────────────────

type ServiceStatus = Running | Degraded | Starting | Down

type Service = { Name: string; Status: ServiceStatus }

type LogLevel = Info | Warn | Error | Debug

type LogEntry = { Time: DateTime; Text: string; Level: LogLevel }

// ── Model ─────────────────────────────────────────────────────────────

type Model =
  { Now: DateTime
    Cpu: float list
    MemPct: float
    MemGb: float
    TotalMem: float
    Events: LogEntry list
    Uptime: int
    Services: Service list
    ReqPerSec: float
    Seed: int
    BarWidth: int
    Elapsed: int64 }

type Msg =
  | Tick
  | Resized of int * int
  | Quit

// ── Init ──────────────────────────────────────────────────────────────

let init () =
  { Now = DateTime.Now
    Cpu = [ 0.30; 0.55; 0.22; 0.78 ]
    MemPct = 0.55
    MemGb = 5.5
    TotalMem = 10.0
    Events =
      [ { Time = DateTime.Now.AddSeconds(-14.0); Text = "System started"; Level = Info }
        { Time = DateTime.Now.AddSeconds(-9.0); Text = "Config loaded"; Level = Info } ]
    Uptime = 0
    Services =
      [ { Name = "api-gateway"; Status = Running }
        { Name = "auth-service"; Status = Running }
        { Name = "db-primary"; Status = Running }
        { Name = "cache-proxy"; Status = Running } ]
    ReqPerSec = 720.0
    Seed = 42
    BarWidth = 18
    Elapsed = 0L },
  Cmd.none

// ── Update ────────────────────────────────────────────────────────────

let private logMessages = [|
  Info,  "Request handled /api/users/42"
  Warn,  "Cache miss: session-8821"
  Info,  "Health check OK"
  Debug, "Connection pool: 9/20 active"
  Error, "Upstream timeout: payment-svc"
  Info,  "Config reload triggered"
  Warn,  "High memory pressure detected"
  Debug, "GC triggered, heap 312 MB"
|]

let update msg model =
  match msg with
  | Tick ->
    let rng = Random(model.Seed)
    let newCpu =
      model.Cpu |> List.map (fun c ->
        let delta = (rng.NextDouble() - 0.5) * 0.15
        min 0.98 (max 0.02 (c + delta)))
    let newMemPct = min 0.95 (max 0.15 (model.MemPct + (rng.NextDouble() - 0.5) * 0.04))
    let newReqPerSec = model.ReqPerSec * 0.80 + float (rng.Next(500, 1250)) * 0.20
    let newServices =
      model.Services |> List.map (fun svc ->
        let roll = rng.Next(100)
        let newStatus =
          match svc.Status with
          | Running when roll < 3 -> Degraded
          | Degraded when roll < 40 -> Running
          | Degraded when roll >= 95 -> Starting
          | Starting when roll < 60 -> Running
          | Down when roll < 20 -> Starting
          | other -> other
        { svc with Status = newStatus })
    let newEvent =
      match rng.Next(8) with
      | n when n < Array.length logMessages ->
        let (level, text) = logMessages.[n]
        Some { Time = DateTime.Now; Text = text; Level = level }
      | _ -> None
    let events =
      match newEvent with
      | Some e -> (e :: model.Events) |> List.truncate 6
      | None -> model.Events
    { model with
        Now = DateTime.Now
        Cpu = newCpu
        MemPct = newMemPct
        MemGb = newMemPct * model.TotalMem
        Events = events
        Services = newServices
        ReqPerSec = newReqPerSec
        Uptime = model.Uptime + 1
        Seed = model.Seed + 1
        Elapsed = model.Elapsed + 250L },
    Cmd.none
  | Resized (w, _) ->
    { model with BarWidth = max 8 (w / 4 - 8) }, Cmd.none
  | Quit -> model, Cmd.quit

// ── View Helpers ──────────────────────────────────────────────────────

let private progressBar (width: int) (pct: float) =
  let filled = int (float width * pct)
  let empty = width - filled
  let gradStart, gradEnd =
    match pct with
    | p when p > 0.80 -> (255uy, 60uy, 60uy),  (180uy, 30uy, 30uy)
    | p when p > 0.55 -> (255uy, 200uy, 50uy), (200uy, 150uy, 30uy)
    | _               -> (80uy, 220uy, 100uy),  (40uy, 180uy, 60uy)
  El.row [
    Gradient.horizontal gradStart gradEnd filled (String('█', max 1 filled))
    El.text (String('░', max 0 empty)) |> El.dim
    El.text (sprintf " %3.0f%%%%" (pct * 100.0))
  ]

let private serviceRow (svc: Service) =
  let dot, color =
    match svc.Status with
    | Running  -> "●", Color.Named(Green, Bright)
    | Degraded -> "●", Color.Named(Yellow, Bright)
    | Starting -> "●", Color.Named(Cyan, Normal)
    | Down     -> "○", Color.Named(White, Normal)
  let statusLabel =
    match svc.Status with
    | Running  -> "running  " | Degraded -> "degraded "
    | Starting -> "starting " | Down     -> "down     "
  El.row [
    El.text (sprintf " %s " dot) |> El.fg color
    El.text (sprintf "%-14s" svc.Name)
    El.text statusLabel |> El.dim
  ]

let private levelStyle =
  function
  | Info  -> Color.Named(Green, Bright),  "INFO "
  | Warn  -> Color.Named(Yellow, Bright), "WARN "
  | Error -> Color.Named(Red, Bright),    "ERROR"
  | Debug -> Color.Named(Blue, Normal),   "DEBUG"

// ── Panel Builders ────────────────────────────────────────────────────

let private headerRow (model: Model) =
  let h = model.Uptime / 3600
  let m = (model.Uptime % 3600) / 60
  let s = model.Uptime % 60
  let uptimeStr = sprintf "up %02d:%02d:%02d" h m s
  let reqColor =
    match model.ReqPerSec with
    | r when r >= 1100.0 -> Color.Named(Red, Bright)
    | r when r >= 800.0  -> Color.Named(Yellow, Bright)
    | _                  -> Color.Named(Green, Bright)
  El.row [
    Gradient.horizontal (0uy, 180uy, 220uy) (80uy, 255uy, 160uy) 20 " ◆ SageTUI Dashboard"
    El.text "  "
    Spinner.dots model.Elapsed |> El.fg (Color.Named(Green, Bright))
    El.fill (El.text "")
    El.text (sprintf "%.0f req/s" model.ReqPerSec) |> El.fg reqColor |> El.bold
    El.text "  "
    El.text (sprintf "MEM %2.0f%%%%" (model.MemPct * 100.0)) |> El.dim
    El.text "  "
    El.text uptimeStr |> El.dim
    El.text "  "
    El.text (model.Now.ToString("HH:mm:ss")) |> El.fg (Color.Named(Yellow, Bright))
    El.text " "
  ]

let private cpuPanel (model: Model) =
  El.column [
    El.text " CPU" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
    El.text ""
    yield!
      model.Cpu |> List.mapi (fun i c ->
        El.row [
          El.text (sprintf " Core %d " i) |> El.dim
          progressBar model.BarWidth c
        ])
  ]
  |> El.bordered Rounded

let private servicesPanel (model: Model) =
  El.column [
    El.text " Services" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
    El.text ""
    yield! model.Services |> List.map serviceRow
    El.text ""
    El.text " Memory" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
    El.text ""
    El.row [
      El.text "  "
      progressBar model.BarWidth model.MemPct
    ]
    El.text (sprintf "  %.1f GB / %.1f GB" model.MemGb model.TotalMem)
      |> El.dim
  ]
  |> El.bordered Rounded

let private logPanel (now: DateTime) (events: LogEntry list) =
  El.column [
    El.text " Event Log" |> El.bold |> El.fg (Color.Named(Cyan, Normal))
    El.text ""
    yield!
      events |> List.map (fun e ->
        let age = int (now - e.Time).TotalSeconds
        let (color, tag) = levelStyle e.Level
        El.row [
          El.text (sprintf " %3ds " age) |> El.dim
          El.text tag |> El.fg color |> El.bold
          El.text (sprintf "  %s" e.Text)
        ])
  ]
  |> El.bordered Light

// ── View ──────────────────────────────────────────────────────────────

let view model =
  El.column [
    headerRow model
    El.row [
      El.ratio 1 2 (cpuPanel model)
      El.ratio 1 2 (servicesPanel model)
    ]
    |> El.fill
    logPanel model.Now model.Events |> El.fill
    El.text " [q] Quit" |> El.dim
  ]

// ── Subscriptions ─────────────────────────────────────────────────────

let keyBindings =
  Keys.bind [
    Key.Char (System.Text.Rune 'q'), Quit
    Key.Char (System.Text.Rune 'Q'), Quit
    Key.Escape, Quit
  ]

let subscribe _model =
  [ TimerSub("tick", TimeSpan.FromMilliseconds(250.0), fun () -> Tick)
    ResizeSub (fun (w, h) -> Some (Resized(w, h)))
    keyBindings ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe
    OnError = CrashOnError }

[<EntryPoint>]
let main _ = App.run program; 0