module SystemMonitor

open System
open SageTUI

type Msg =
  | Tick
  | Quit
  | ScrollUp
  | ScrollDown
  | NextTab
  | PrevTab
  | NextDemoStep

type ProcessInfo = {
  Name: string
  Cpu: float
  Mem: float
  Status: string
}

type Model = {
  CpuHistory: float list
  MemHistory: float list
  NetInHistory: float list
  NetOutHistory: float list
  Processes: ProcessInfo list
  ProcessScroll: ScrollState
  Uptime: int
  ActiveTab: int
  Rng: Random
  DemoStep: int
}

// ─── Demo Mode ───────────────────────────────────────────────────────────────

let isDemoMode = Environment.GetEnvironmentVariable("SAGETUI_DEMO_MODE") = "1"

// Sequence: scroll process table, cycle tabs, back to overview.
// Total: ~11.1s. Set SAGETUI_EXIT_AFTER_MS=12000.
let demoSteps : (int * Msg option) list = [
  1000, None              // pause on Overview — metrics ticking live
  350,  Some ScrollDown   // scroll processes down
  350,  Some ScrollDown
  350,  Some ScrollDown
  500,  None              // pause — see lower-ranked processes
  350,  Some ScrollUp     // scroll back to top
  350,  Some ScrollUp
  350,  Some ScrollUp
  1200, Some NextTab      // → Network tab
  2000, None              // linger on Network — sparklines updating live
  900,  Some NextTab      // → About tab
  1000, None              // About
  600,  Some NextTab      // → Overview (tab 2 → 0)
  1000, None              // back at Overview — loop
]

let maxHistory = 40
let theme = Theme.nord

let private chip fg bg text =
  El.text (sprintf " %s " text)
  |> El.bold
  |> El.fg fg
  |> El.bg bg

let private formatUptime ticks =
  let totalSeconds = ticks / 2
  let minutes = totalSeconds / 60
  let seconds = totalSeconds % 60
  sprintf "%02d:%02d" minutes seconds

let private trimHistory value xs =
  let next = xs @ [value]
  match List.length next > maxHistory with
  | true -> next |> List.skip 1
  | false -> next

let private metricCard accent label value detail =
  El.column [
    El.text label
      |> El.fg theme.TextDim
      |> El.bold
    El.text value
      |> El.fg accent
      |> El.bold
    El.text detail
      |> El.fg theme.TextDim
      |> El.dim
  ]
  |> El.padHV 1 0
  |> El.bordered Rounded

let fakeProcesses (rng: Random) =
  [| "gateway"; "event-store"; "scheduler"; "search-index"; "api-edge"; "asset-proxy"; "auth-svc"
     "session-cache"; "email-worker"; "audit-writer"; "metrics"; "billing-sync"; "blob-sync"
     "web-liveview"; "image-proxy"; "job-runner"; "replay"; "postgres"; "redis"; "queue" |]
  |> Array.map (fun name ->
    { Name = name
      Cpu = Math.Round(2.0 + rng.NextDouble() * 36.0, 1)
      Mem = Math.Round(120.0 + rng.NextDouble() * 760.0, 0)
      Status =
        match rng.Next(10) with
        | 0 -> "idle"
        | n when n < 3 -> "sync"
        | _ -> "run" })
  |> Array.sortByDescending (fun p -> p.Cpu)
  |> Array.toList

let init () =
  let rng = Random(42)
  { CpuHistory = [ for _ in 1..maxHistory -> rng.NextDouble() * 30.0 ]
    MemHistory = [ for _ in 1..maxHistory -> 40.0 + rng.NextDouble() * 20.0 ]
    NetInHistory = [ for _ in 1..maxHistory -> rng.NextDouble() * 100.0 ]
    NetOutHistory = [ for _ in 1..maxHistory -> rng.NextDouble() * 50.0 ]
    Processes = fakeProcesses rng
    ProcessScroll = ScrollState.create 20 8
    Uptime = 0
    ActiveTab = 0
    Rng = rng
    DemoStep = 0 },
  match isDemoMode with true -> Cmd.delay 800 NextDemoStep | false -> Cmd.none

let rec update msg model =
  match msg with
  | Tick ->
    let cpu = model.Rng.NextDouble() * 40.0
    let mem = 40.0 + model.Rng.NextDouble() * 25.0
    let netIn = model.Rng.NextDouble() * 100.0
    let netOut = model.Rng.NextDouble() * 50.0
    { model with
        CpuHistory = trimHistory cpu model.CpuHistory
        MemHistory = trimHistory mem model.MemHistory
        NetInHistory = trimHistory netIn model.NetInHistory
        NetOutHistory = trimHistory netOut model.NetOutHistory
        Processes = fakeProcesses model.Rng
        Uptime = model.Uptime + 1 }, Cmd.none
  | ScrollUp ->
    { model with ProcessScroll = ScrollState.scrollUp model.ProcessScroll }, Cmd.none
  | ScrollDown ->
    { model with ProcessScroll = ScrollState.scrollDown model.ProcessScroll }, Cmd.none
  | NextTab ->
    { model with ActiveTab = (model.ActiveTab + 1) % 3 }, Cmd.none
  | PrevTab ->
    { model with ActiveTab = (model.ActiveTab + 2) % 3 }, Cmd.none
  | Quit -> model, Cmd.quit
  | NextDemoStep when isDemoMode ->
    let step = model.DemoStep % demoSteps.Length
    let (delayMs, actionOpt) = demoSteps[step]
    let model' = { model with DemoStep = step + 1 }
    let model'', actionCmd =
      match actionOpt with
      | Some action -> update action model'
      | None -> model', Cmd.none
    model'', Cmd.batch [actionCmd; Cmd.delay delayMs NextDemoStep]
  | NextDemoStep -> model, Cmd.none

let sparkline color label (data: float list) maxVal =
  let bars =
    data
    |> List.map (fun v -> min 1.0 (v / maxVal))
    |> List.map (fun pct ->
      let chars = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]
      let idx = int (pct * 7.0) |> max 0 |> min 7
      chars[idx])
    |> String.concat ""
  let last = data |> List.tryLast |> Option.defaultValue 0.0
  El.column [
    El.row [
      El.text label |> El.bold |> El.fg color
      El.fill El.empty
      El.text (sprintf "current %.1f" last) |> El.fg theme.TextDim |> El.dim
    ]
    El.text bars |> El.fg color
    El.text (sprintf "%.1f%% of threshold" last) |> El.fg theme.TextDim |> El.dim
  ]
  |> El.padHV 1 0
  |> El.bordered Rounded

let statusDot status =
  match status with
  | "run" -> El.text "●" |> El.fg theme.Success
  | "sync" -> El.text "●" |> El.fg theme.Warning
  | _ -> El.text "●" |> El.fg theme.TextDim

let processTable (procs: ProcessInfo list) scroll =
  let items =
    procs
    |> List.mapi (fun i p ->
      let nameCell =
        let baseCell = El.text p.Name |> El.width 18
        match i = 0 with
        | true -> baseCell |> El.bold
        | false -> baseCell

      let cpuCell =
        El.text (sprintf "%5.1f%%" p.Cpu)
        |> El.width 8
        |> El.fg (match p.Cpu > 28.0 with | true -> theme.Error | false -> theme.Success)

      let stateCell =
        El.text p.Status
        |> El.width 8
        |> El.fg (match p.Status with | "run" -> theme.Success | "sync" -> theme.Warning | _ -> theme.TextDim)

      let row =
        El.row [
          statusDot p.Status |> El.width 2
          nameCell
          cpuCell
          El.text (sprintf "%5.0f MB" p.Mem) |> El.width 10
          stateCell
          El.text
            (match p.Status with
             | "run" -> "serving requests"
             | "sync" -> "checkpointing"
             | _ -> "waiting for work")
            |> El.fg theme.TextDim
            |> El.dim
        ]
      row)
  El.column [
    El.row [
      El.text "Top processes" |> El.bold |> El.fg theme.Primary
      El.fill El.empty
      El.text "↑/↓ scroll" |> El.fg theme.TextDim |> El.dim
    ]
    El.row [
      El.text " " |> El.width 2
      El.text "PROCESS" |> El.bold |> El.width 18
      El.text "CPU" |> El.bold |> El.width 8
      El.text "MEM" |> El.bold |> El.width 10
      El.text "STATE" |> El.bold |> El.width 8
      El.text "DETAIL" |> El.bold
    ] |> El.fg theme.Secondary
    El.text (String.replicate 72 "─") |> El.fg theme.TextDim |> El.dim
    Scroll.view scroll (fun _idx el -> el) items
  ]
  |> El.padHV 1 0
  |> El.bordered Rounded

let overviewTab model =
  let lastCpu = model.CpuHistory |> List.tryLast |> Option.defaultValue 0.0
  let lastMem = model.MemHistory |> List.tryLast |> Option.defaultValue 0.0
  let lastIn = model.NetInHistory |> List.tryLast |> Option.defaultValue 0.0
  let lastOut = model.NetOutHistory |> List.tryLast |> Option.defaultValue 0.0
  let hottest =
    model.Processes
    |> List.tryHead
    |> Option.map (fun p -> sprintf "%s at %.1f%%" p.Name p.Cpu)
    |> Option.defaultValue "No process activity"

  let headline =
    El.row [
      metricCard theme.Primary "CPU LOAD" (sprintf "%.1f%%" lastCpu) "Nominal envelope < 45%" |> El.width 22
      El.text " " |> El.width 1
      metricCard theme.Accent "MEMORY" (sprintf "%.1f%%" lastMem) "Working set stays steady" |> El.width 22
      El.text " " |> El.width 1
      metricCard theme.Success "THROUGHPUT" (sprintf "%.1f Mbps" (lastIn + lastOut)) "Ingress + egress combined" |> El.width 24
      El.text " " |> El.width 1
      metricCard theme.Warning "UPTIME" (formatUptime model.Uptime) hottest |> El.width 22
    ]

  El.column [
    headline
    El.text ""
    El.row [
      sparkline theme.Primary "CPU history" model.CpuHistory 100.0 |> El.width 49
      El.text " " |> El.width 1
      sparkline theme.Accent "Memory history" model.MemHistory 100.0 |> El.width 49
    ]
    El.text ""
    processTable model.Processes model.ProcessScroll |> El.fill
  ]

let networkTab model =
  let lastIn = model.NetInHistory |> List.tryLast |> Option.defaultValue 0.0
  let lastOut = model.NetOutHistory |> List.tryLast |> Option.defaultValue 0.0

  El.column [
    El.row [
      metricCard theme.Success "DOWNLOAD" (sprintf "%.1f Mbps" lastIn) "Public edge + replication traffic" |> El.width 32
      El.text " " |> El.width 1
      metricCard theme.Warning "UPLOAD" (sprintf "%.1f Mbps" lastOut) "Background jobs + client responses" |> El.width 32
      El.text " " |> El.width 1
      metricCard theme.Primary "INTERFACES" "3 / 4 up" "docker bridge parked" |> El.width 32
    ]
    El.text ""
    El.row [
      sparkline theme.Success "Download trend" model.NetInHistory 100.0 |> El.width 49
      El.text " " |> El.width 1
      sparkline theme.Warning "Upload trend" model.NetOutHistory 100.0 |> El.width 49
    ]
    El.text ""
    El.column [
      El.row [
        El.text "Interface health" |> El.bold |> El.fg theme.Primary
        El.fill El.empty
        El.text "updated every 500 ms" |> El.fg theme.TextDim |> El.dim
      ]
      El.row [
        El.text "eth0" |> El.width 10 |> El.bold
        chip (Color.Named(Black, Normal)) theme.Success "UP"
        El.text "  ingress 62.1 Mbps • primary application edge"
      ]
      El.row [
        El.text "wlan0" |> El.width 10 |> El.bold
        chip (Color.Named(Black, Normal)) theme.Success "UP"
        El.text "  management 14.3 Mbps • maintenance and remote access"
      ]
      El.row [
        El.text "lo" |> El.width 10 |> El.bold
        chip (Color.Named(Black, Normal)) theme.Warning "LOCAL"
        El.text "  service mesh and local health checks"
      ]
      El.row [
        El.text "docker0" |> El.width 10 |> El.bold
        El.text " DOWN " |> El.bold |> El.fg theme.TextDim |> El.bordered Rounded
        El.text "  idle bridge • no active containers attached"
          |> El.fg theme.TextDim
          |> El.dim
      ]
    ]
    |> El.padHV 1 0
    |> El.bordered Rounded
  ]

let aboutTab model =
  El.column [
    Theme.heading theme "SageTUI System Monitor"
    Theme.subheading theme "A dense operator-console sample built to show credible live data in a terminal-first UI."
    El.text ""
    El.column [
      El.row [ chip (Color.Named(Black, Normal)) theme.Primary "TEA"; El.text " Elm-style update loop with timer-driven live refresh" ]
      El.row [ chip (Color.Named(Black, Normal)) theme.Accent "WIDGETS"; El.text " Tabs, progress bars, scroll regions, and themed panels" ]
      El.row [ chip (Color.Named(Black, Normal)) theme.Success "OPERATIONS"; El.text " Realistic naming, process tables, and throughput trends" ]
      El.row [ chip (Color.Named(Black, Normal)) theme.Warning "INPUT"; El.text " Arrow keys, Tab, and q support immediate exploration" ]
    ]
    |> El.gap 1
    El.text ""
    El.row [
      metricCard theme.Primary "SESSION" (formatUptime model.Uptime) "Synthetic uptime since launch" |> El.width 30
      El.text " " |> El.width 1
      metricCard theme.Success "REFRESH" "500 ms" "Balanced for readable motion" |> El.width 30
    ]
  ]
  |> El.padHV 2 1
  |> El.bordered Rounded
  |> El.center

let view model =
  let tabBar =
    El.row [
      Tabs.view {
        Items = ["Overview"; "Network"; "About"]
        ActiveIndex = model.ActiveTab
        ToString = id
        ActiveColor = Some theme.Primary
        InactiveColor = Some theme.TextDim
      }
      El.fill El.empty
      El.text "←/→ or Tab to switch views"
        |> El.fg theme.TextDim
        |> El.dim
    ]
    |> El.padHV 1 0

  let content =
    match model.ActiveTab with
    | 0 -> overviewTab model
    | 1 -> networkTab model
    | _ -> aboutTab model

  El.column [
    El.row [
      chip (Color.Named(Black, Normal)) theme.Accent "FLAGSHIP"
      Theme.heading theme "System Monitor"
      El.text "  "
      Theme.subheading theme "dense operator console"
      El.fill El.empty
      chip (Color.Named(Black, Normal)) theme.Success "LIVE"
      El.text " "
      El.text (sprintf "uptime %s" (formatUptime model.Uptime))
        |> El.fg theme.TextDim
        |> El.dim
    ]
    |> El.padHV 1 0
    tabBar
    content |> El.fill |> El.padAll 1
    El.row [
      El.text "↑/↓" |> El.bold |> El.fg theme.Primary
      El.text " scroll processes  " |> El.fg theme.TextDim |> El.dim
      El.text "q" |> El.bold |> El.fg theme.Primary
      El.text " quit" |> El.fg theme.TextDim |> El.dim
    ]
    |> El.padHV 1 0
  ]
  |> Theme.apply theme

let keyBindings =
  Keys.bind [
    Key.Char 'q', Quit
    Key.Escape, Quit
    Key.Up, ScrollUp
    Key.Down, ScrollDown
    Key.Right, NextTab
    Key.Left, PrevTab
    Key.Tab, NextTab
  ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [ keyBindings; TimerSub("tick", TimeSpan.FromMilliseconds(500.0), fun () -> Tick) ] }

[<EntryPoint>]
let main _ = App.run program; 0

