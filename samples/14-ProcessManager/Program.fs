/// Sample 14 – Process Manager
/// ─────────────────────────────────────────────────────────────────────────────
/// Demonstrates:
///   • TableState  — multi-select, cursor navigation (j/k/↑↓/Space/Ctrl+A)
///   • Cmd.debounce — search field with 150 ms debounce (no stutter)
///   • Sub.throttle — live CPU % update throttled to 4 fps
///   • KeyMap<Msg>  — composable modal keybindings (normal / search modes)
///   • El.lazyEq    — per-row memoisation; only dirty rows re-render
///   • Toast        — kill confirmation feedback
module ProcessManager

open SageTUI

// ── Domain ───────────────────────────────────────────────────────────────────

type ProcessStatus = Running | Sleeping | Stopped | Zombie

type ProcessEntry = {
  Pid   : int
  Name  : string
  Cpu   : float   // percent
  Mem   : int     // MB
  User  : string
  Status: ProcessStatus
}

let private rng = System.Random(42)

let private fakeProcesses () =
  [| ("systemd", "root"); ("sshd", "root"); ("nginx", "www-data"); ("postgres", "postgres")
     ("redis-server", "redis"); ("node", "app"); ("python3", "app"); ("dotnet", "app")
     ("bash", "user"); ("vim", "user"); ("git", "user"); ("curl", "root")
     ("journald", "root"); ("dbus", "root"); ("NetworkManager", "root"); ("cron", "root") |]
  |> Array.mapi (fun i (name, user) ->
    { Pid    = 1000 + i * 37
      Name   = name
      Cpu    = float (rng.Next(0, 40)) + rng.NextDouble()
      Mem    = rng.Next(8, 512)
      User   = user
      Status = if i % 8 = 7 then Sleeping else Running })

// ── Msg / Model ──────────────────────────────────────────────────────────────

type Mode = Normal | Search

type Msg =
  | Tick
  | SetSearch   of string
  | CommitSearch of string           // debounced commit
  | TableKey    of Key * Modifiers
  | KillSelected
  | ToggleMode
  | Quit

type Model = {
  Processes   : ProcessEntry array
  Filtered    : ProcessEntry array
  TableState  : TableState
  SearchInput : string
  CommittedSearch: string
  Mode        : Mode
  LastKillMsg : string option
  ToastTimer  : int
}

let private applyFilter (q: string) (procs: ProcessEntry array) =
  if q = "" then procs
  else
    let q = q.ToLowerInvariant ()
    procs |> Array.filter (fun p ->
      p.Name.ToLowerInvariant().Contains(q) || p.User.Contains(q))

let init () =
  let procs = fakeProcesses ()
  { Processes    = procs
    Filtered     = procs
    TableState   = TableState.empty procs.Length
    SearchInput  = ""
    CommittedSearch = ""
    Mode         = Normal
    LastKillMsg  = None
    ToastTimer   = 0 }, Cmd.none

// ── Update ────────────────────────────────────────────────────────────────────

let update msg model =
  match msg with
  | Quit -> model, Cmd.quit

  | Tick ->
    // Simulate CPU drift
    let procs =
      model.Processes |> Array.map (fun p ->
        { p with Cpu = max 0.0 (min 99.9 (p.Cpu + (rng.NextDouble () - 0.5) * 4.0)) })
    let filtered = applyFilter model.CommittedSearch procs
    let ts = model.TableState |> TableState.withRowCount filtered.Length
    let timer = if model.ToastTimer > 0 then model.ToastTimer - 1 else 0
    let kill  = if timer = 0 then None else model.LastKillMsg
    { model with Processes = procs; Filtered = filtered; TableState = ts
                 ToastTimer = timer; LastKillMsg = kill }, Cmd.none

  | SetSearch s ->
    { model with SearchInput = s },
    Cmd.debounce "search" 150 (CommitSearch s)

  | CommitSearch s ->
    let filtered = applyFilter s model.Processes
    let ts = model.TableState |> TableState.withRowCount filtered.Length
    { model with CommittedSearch = s; Filtered = filtered; TableState = ts }, Cmd.none

  | TableKey (key, mods) ->
    let ts = TableState.handleKey key mods model.TableState
    { model with TableState = ts }, Cmd.none

  | KillSelected ->
    let selected = model.TableState.Selected
    let names =
      selected
      |> Set.toList
      |> List.choose (fun i ->
        match model.Filtered |> Array.tryItem i with
        | Some p -> Some p.Name
        | None   -> None)
    let procs =
      model.Processes
      |> Array.filter (fun p -> not (names |> List.contains p.Name))
    let filtered = applyFilter model.CommittedSearch procs
    let ts =
      { TableState.empty filtered.Length with Cursor = min model.TableState.Cursor (filtered.Length - 1) }
    let msg =
      match names with
      | []  -> "Nothing selected"
      | [n] -> $"Killed {n}"
      | ns  -> $"Killed {ns.Length} processes"
    { model with
        Processes  = procs
        Filtered   = filtered
        TableState = ts
        LastKillMsg = Some msg
        ToastTimer  = 8 }, Cmd.none

  | ToggleMode ->
    { model with Mode = if model.Mode = Normal then Search else Normal }, Cmd.none

// ── Key bindings ──────────────────────────────────────────────────────────────

let normalKeys =
  Keys.bind [
    Key.Char(System.Text.Rune 'q'), Quit
    Key.Escape,                     Quit
    Key.Char(System.Text.Rune '/'), ToggleMode
    Key.Char(System.Text.Rune 'x'), KillSelected
  ]

// ── View ──────────────────────────────────────────────────────────────────────

let private statusColor =
  function
  | Running  -> Color.Named(BaseColor.Green,   Intensity.Normal)
  | Sleeping -> Color.Named(BaseColor.Cyan,    Intensity.Normal)
  | Stopped  -> Color.Named(BaseColor.Yellow,  Intensity.Normal)
  | Zombie   -> Color.Named(BaseColor.Red,     Intensity.Normal)

let private statusLabel =
  function
  | Running  -> "run"
  | Sleeping -> "slp"
  | Stopped  -> "stp"
  | Zombie   -> "zmb"

let private columns : TableColumn<ProcessEntry> list = [
  TableColumn.create "PID"    6  (fun p -> El.text (string p.Pid))
  TableColumn.create "NAME"   18 (fun p -> El.text p.Name)
  TableColumn.create "USER"   12 (fun p -> El.text p.User)
  TableColumn.create "CPU%"   7  (fun p -> El.text (sprintf "%5.1f" p.Cpu))
  TableColumn.create "MEM"    6  (fun p -> El.text (sprintf "%4dM" p.Mem))
  { Header = "ST"; Width = 4; SortKey = None; Fill = false
    Render = fun p -> El.text (statusLabel p.Status) |> El.fg (statusColor p.Status) }
]

// Memoized row renderer — only re-renders when (index, entry, cursor, selection) changes
let private renderRowMemo =
  El.lazyEq<int * ProcessEntry * bool * bool> (fun (i, p, isCursor, isSelected) ->
    let rowEl =
      columns
      |> List.map (fun col ->
        let el = col.Render p
        match col.Fill with
        | true  -> El.fill el
        | false -> El.width col.Width el)
      |> El.row
    match isCursor, isSelected with
    | true,  _     -> rowEl |> El.bg (Color.Named(BaseColor.Blue,  Intensity.Normal))
    | false, true  -> rowEl |> El.bg (Color.Named(BaseColor.Cyan,  Intensity.Normal))
    | false, false -> rowEl)

let private renderRow (i: int) (p: ProcessEntry) (state: TableState) =
  let isCursor   = state.Cursor = i
  let isSelected = Set.contains i state.Selected
  renderRowMemo (i, p, isCursor, isSelected)

let view (model: Model) =
  let header =
    El.row (columns |> List.map (fun col ->
      let h = El.text col.Header |> El.bold
      match col.Fill with true -> El.fill h | false -> El.width col.Width h))
  let separator =
    El.row (columns |> List.map (fun col ->
      match col.Fill with
      | true  -> El.fill (El.text "")
      | false -> El.text (System.String('─', col.Width)) |> El.width col.Width))
  let dataRows =
    model.Filtered
    |> Array.toList
    |> List.mapi (fun i p -> renderRow i p model.TableState)
  let table =
    El.column (header :: separator :: dataRows)
    |> El.bordered BorderStyle.Rounded

  let selectedCount = model.TableState.Selected.Count
  let statusBar =
    let modeLabel =
      match model.Mode with
      | Normal -> El.text " [q]uit  [/]search  [x]kill  [j/k]nav  [Space]select  [Ctrl+A]all"
                  |> El.fg (Color.Named(BaseColor.White, Intensity.Normal))
      | Search -> El.text " [Esc]exit search  type to filter"
                  |> El.fg (Color.Named(BaseColor.Cyan, Intensity.Normal))
    let selLabel =
      if selectedCount > 0 then
        El.text $"  {selectedCount} selected" |> El.fg (Color.Named(BaseColor.Yellow, Intensity.Normal))
      else El.empty
    El.row [ modeLabel; selLabel ]

  let searchBar =
    match model.Mode with
    | Normal ->
      match model.CommittedSearch with
      | "" -> El.empty
      | q  -> El.text $"  filter: {q}" |> El.fg (Color.Named(BaseColor.Cyan, Intensity.Normal))
    | Search ->
      El.row [
        El.text "  search: " |> El.fg (Color.Named(BaseColor.Yellow, Intensity.Normal))
        El.text model.SearchInput
      ]

  let toast =
    match model.LastKillMsg with
    | None   -> El.empty
    | Some m ->
      El.text $"  ✔ {m}"
      |> El.fg (Color.Named(BaseColor.Green, Intensity.Normal))
      |> El.bold

  El.column [
    El.row [
      El.text " ⚙ Process Manager"
      |> El.bold
      |> El.fg (Color.Named(BaseColor.Green, Intensity.Normal))
      El.text $"  ({model.Filtered.Length}/{model.Processes.Length} processes)"
      |> El.fg (Color.Named(BaseColor.White, Intensity.Normal))
    ]
    El.text ""
    table
    statusBar
    searchBar
    toast
  ]

// ── Subscribe ─────────────────────────────────────────────────────────────────

let subscribe (model: Model) =
  [
    // timer fires every 250 ms → 4 fps CPU updates
    TimerSub("tick", System.TimeSpan.FromMilliseconds(250.0), fun () -> Tick)

    // normal-mode key bindings
    normalKeys

    // table navigation keys (arrow + j/k/space) — active in both modes
    KeySub(fun (key, mods) ->
      match key, mods with
      | Key.Up,    _ | Key.Down,  _ -> Some (TableKey(key, mods))
      | Key.Char r, _ when r = System.Text.Rune 'j' || r = System.Text.Rune 'k' ->
        Some (TableKey(key, mods))
      | Key.Char r, _ when r = System.Text.Rune ' ' -> Some (TableKey(key, mods))
      | Key.Char r, Modifiers.Ctrl when r = System.Text.Rune 'a' -> Some (TableKey(key, mods))
      | _ -> None)

    // search mode text input — captures printable keys when Mode = Search
    KeySub(fun (key, _mods) ->
      match key with
      | Key.Escape when model.Mode = Search -> Some ToggleMode
      | Key.Char r when model.Mode = Search ->
        Some (SetSearch (model.SearchInput + r.ToString()))
      | Key.Backspace when model.Mode = Search ->
        let s = model.SearchInput
        Some (SetSearch (if s.Length > 0 then s[..s.Length - 2] else ""))
      | _ -> None)
  ]

// ── Entry point ───────────────────────────────────────────────────────────────

[<EntryPoint>]
let main _ =
  let program: Program<Model, Msg> =
    { Init      = init
      Update    = update
      View      = view
      Subscribe = subscribe
      OnError = None }
  App.run program
  0
