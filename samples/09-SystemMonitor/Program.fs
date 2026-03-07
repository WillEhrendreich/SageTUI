open SageTUI
open System

type Msg =
  | Tick
  | Quit
  | ScrollUp
  | ScrollDown
  | NextTab
  | PrevTab

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
}

let maxHistory = 40

let fakeProcesses (rng: Random) =
  [| "dotnet"; "code"; "chrome"; "firefox"; "nvim"; "bash"; "node"
     "postgres"; "redis"; "nginx"; "systemd"; "sshd"; "docker"
     "tmux"; "htop"; "git"; "cargo"; "rustc"; "python3"; "fsi" |]
  |> Array.map (fun name ->
    { Name = name
      Cpu = Math.Round(rng.NextDouble() * 25.0, 1)
      Mem = Math.Round(rng.NextDouble() * 500.0, 0)
      Status =
        match rng.Next(10) with
        | 0 -> "idle"
        | n when n < 3 -> "wait"
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
    Rng = rng }, Cmd.none

let update msg model =
  match msg with
  | Tick ->
    let cpu = model.Rng.NextDouble() * 40.0
    let mem = 40.0 + model.Rng.NextDouble() * 25.0
    let netIn = model.Rng.NextDouble() * 100.0
    let netOut = model.Rng.NextDouble() * 50.0
    let trim xs = (xs @ [cpu]) |> List.skip (match List.length xs >= maxHistory with | true -> 1 | false -> 0)
    { model with
        CpuHistory = trim model.CpuHistory
        MemHistory = (model.MemHistory @ [mem]) |> List.skip (match model.MemHistory.Length >= maxHistory with | true -> 1 | false -> 0)
        NetInHistory = (model.NetInHistory @ [netIn]) |> List.skip (match model.NetInHistory.Length >= maxHistory with | true -> 1 | false -> 0)
        NetOutHistory = (model.NetOutHistory @ [netOut]) |> List.skip (match model.NetOutHistory.Length >= maxHistory with | true -> 1 | false -> 0)
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
    El.text label |> El.bold |> El.fg color
    El.text bars |> El.fg color
    El.text (sprintf "%.1f%%" last) |> El.dim
  ]

let gauge color label pct =
  El.row [
    El.text (sprintf "%s " label) |> El.bold |> El.width 6
    ProgressBar.view
      { ProgressBar.defaults with
          Percent = pct
          Width = 30
          FilledColor = Some color
          EmptyColor = Some (Color.Named(White, Normal)) }
    El.text (sprintf " %d%%" (int (pct * 100.0))) |> El.dim
  ]

let statusDot status =
  match status with
  | "run" -> El.text "●" |> El.fg (Color.Named(Green, Bright))
  | "wait" -> El.text "●" |> El.fg (Color.Named(Yellow, Bright))
  | _ -> El.text "●" |> El.fg (Color.Named(Red, Normal))

let processTable (procs: ProcessInfo list) scroll =
  let items =
    procs
    |> List.mapi (fun i p ->
      El.row [
        statusDot p.Status |> El.width 2
        El.text p.Name |> El.width 12
        El.text (sprintf "%5.1f%%" p.Cpu) |> El.width 8
          |> El.fg (match p.Cpu > 15.0 with | true -> Color.Named(Red, Bright) | false -> Color.Named(Green, Bright))
        El.text (sprintf "%5.0f MB" p.Mem) |> El.width 10
        El.text p.Status |> El.dim
      ])
  El.column [
    El.row [
      El.text "  " |> El.width 2
      El.text "PROCESS" |> El.bold |> El.width 12
      El.text "   CPU" |> El.bold |> El.width 8
      El.text "    MEM" |> El.bold |> El.width 10
      El.text "STATUS" |> El.bold
    ] |> El.fg (Color.Named(Cyan, Bright))
    El.text (String.replicate 44 "─") |> El.dim
    Scroll.view scroll (fun _idx el -> el) items
  ] |> El.bordered Light

let overviewTab model =
  let theme = Theme.dark
  let lastCpu = model.CpuHistory |> List.tryLast |> Option.defaultValue 0.0
  let lastMem = model.MemHistory |> List.tryLast |> Option.defaultValue 0.0
  El.column [
    El.row [
      sparkline (Color.Named(Cyan, Bright)) "CPU" model.CpuHistory 100.0 |> El.fill
      sparkline (Color.Named(Magenta, Bright)) "MEM" model.MemHistory 100.0 |> El.fill
    ] |> El.bordered Light
    El.row [
      gauge (Color.Named(Cyan, Bright)) "CPU" (lastCpu / 100.0)
      El.text "  " |> El.width 2
      gauge (Color.Named(Magenta, Bright)) "MEM" (lastMem / 100.0)
    ] |> El.padAll 1
    processTable model.Processes model.ProcessScroll |> El.fill
  ]

let networkTab model =
  El.column [
    El.row [
      sparkline (Color.Named(Green, Bright)) "↓ Download" model.NetInHistory 100.0 |> El.fill
      sparkline (Color.Named(Yellow, Bright)) "↑ Upload" model.NetOutHistory 100.0 |> El.fill
    ] |> El.bordered Light
    El.row [
      El.column [
        El.text "Network Stats" |> El.bold |> El.fg (Color.Named(Green, Bright))
        let lastIn = model.NetInHistory |> List.tryLast |> Option.defaultValue 0.0
        let lastOut = model.NetOutHistory |> List.tryLast |> Option.defaultValue 0.0
        El.text (sprintf "  Download: %.1f Mbps" lastIn)
        El.text (sprintf "  Upload:   %.1f Mbps" lastOut)
        El.text (sprintf "  Total:    %.1f Mbps" (lastIn + lastOut))
      ] |> El.fill |> El.padAll 1
      El.column [
        El.text "Interfaces" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
        El.text "  eth0   ● up" |> El.fg (Color.Named(Green, Bright))
        El.text "  wlan0  ● up" |> El.fg (Color.Named(Green, Bright))
        El.text "  lo     ● up" |> El.fg (Color.Named(Yellow, Bright))
        El.text "  docker ○ down" |> El.dim
      ] |> El.fill |> El.padAll 1
    ] |> El.bordered Light |> El.fill
  ]

let aboutTab model =
  El.column [
    El.text "SageTUI System Monitor" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
    El.text ""
    El.text "A showcase of SageTUI features:" |> El.dim
    El.text "  • Elm Architecture (TEA)" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Sparkline charts" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Progress bars" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Scrollable lists" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Tabs navigation" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Theme system" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Keyboard bindings" |> El.fg (Color.Named(Green, Bright))
    El.text "  • Live updates via subscriptions" |> El.fg (Color.Named(Green, Bright))
    El.text ""
    El.text (sprintf "Uptime: %d ticks" model.Uptime) |> El.dim
    El.text "Built with ❤ in F#" |> El.fg (Color.Named(Magenta, Bright))
  ] |> El.padAll 2 |> El.bordered Rounded |> El.center

let view model =
  let tabs = [| "Overview"; "Network"; "About" |]
  let tabBar =
    El.row (
      tabs
      |> Array.mapi (fun i name ->
        match i = model.ActiveTab with
        | true ->
          El.text (sprintf " %s " name)
          |> El.bold |> El.fg (Color.Named(Cyan, Bright))
          |> El.bordered Light
        | false ->
          El.text (sprintf " %s " name)
          |> El.dim)
      |> Array.toList)

  let content =
    match model.ActiveTab with
    | 0 -> overviewTab model
    | 1 -> networkTab model
    | _ -> aboutTab model

  El.column [
    El.row [
      El.text " ⚡ System Monitor " |> El.bold |> El.fg (Color.Named(Cyan, Bright))
      tabBar |> El.fill
      El.text (sprintf " tick:%d " model.Uptime) |> El.dim
    ]
    content |> El.fill
    El.row [
      El.text " [←/→] tabs  [↑/↓] scroll  [q] quit " |> El.dim
    ]
  ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [
      Keys.bind [
        Key.Char 'q', Quit
        Key.Escape, Quit
        Key.Up, ScrollUp
        Key.Down, ScrollDown
        Key.Right, NextTab
        Key.Left, PrevTab
        Key.Tab, NextTab
      ]
      TimerSub("tick", TimeSpan.FromMilliseconds(500.0), fun () -> Tick)
    ] }

[<EntryPoint>]
let main _ = App.run program; 0
