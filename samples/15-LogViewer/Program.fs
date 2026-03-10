module LogViewer

// ─────────────────────────────────────────────────────────────────────────────
// Sample 15: LogViewer — Canonical example application
//
// Demonstrates the full SageTUI widget composition pattern:
//   • VirtualList  — efficiently renders thousands of log entries
//   • TextInput    — live filter that rebuilds the list on every keystroke
//   • FocusRing    — Tab/Shift-Tab cycles between filter input and log list
//   • Toast        — transient "Copied!" notification with auto-dismiss
//   • Cmd.delay    — schedules toast dismissal without blocking
//   • Cmd.copyToClipboard — OSC 52 clipboard write
//   • Theme        — consistent look via Theme.catppuccin
//
// Key bindings:
//   /           Focus the filter input
//   Tab         Move to log list
//   Shift+Tab   Move to filter input
//   j/k         Navigate down/up in the list (when list is focused)
//   g/G         Jump to first/last entry
//   Enter       Copy selected entry to clipboard
//   Escape      Quit
// ─────────────────────────────────────────────────────────────────────────────

open SageTUI

// ── Domain ────────────────────────────────────────────────────────────────────

type LogLevel = Trace | Debug | Info | Warn | Error | Fatal

type LogEntry =
  { Index: int
    Timestamp: string
    Level: LogLevel
    Source: string
    Message: string }

// ── Model ─────────────────────────────────────────────────────────────────────

type FocusField = FilterField | ListField

type Model =
  { AllEntries: LogEntry array
    FilterInput: TextInputModel
    LogList: VirtualListModel<LogEntry>
    Toasts: ToastQueue
    Focus: FocusRing<FocusField> }

type Msg =
  | KeyInput of Key * Modifiers
  | ClipboardWritten
  | DismissToast of ToastId
  | Quit

// ── Helpers ───────────────────────────────────────────────────────────────────

let levelLabel = function
  | Trace -> "TRC" | Debug -> "DBG" | Info -> "INF"
  | Warn  -> "WRN" | Error -> "ERR" | Fatal -> "FTL"

let levelColor (theme: Theme) = function
  | Trace -> theme.TextDim
  | Debug -> theme.Secondary
  | Info  -> theme.Success
  | Warn  -> theme.Warning
  | Error -> theme.Error
  | Fatal -> theme.Error

let matchesFilter (filter: string) (entry: LogEntry) =
  match filter with
  | "" -> true
  | f  ->
    let lower = f.ToLowerInvariant()
    entry.Message.ToLowerInvariant().Contains(lower)
    || entry.Source.ToLowerInvariant().Contains(lower)
    || (levelLabel entry.Level).ToLowerInvariant().Contains(lower)

let viewportHeight = 15

let rebuildList (filter: string) (all: LogEntry array) =
  all
  |> Array.filter (matchesFilter filter)
  |> VirtualList.ofArray viewportHeight

// ── Sample data ───────────────────────────────────────────────────────────────

let sampleEntries : LogEntry array =
  [| { Index = 0;  Timestamp = "12:00:00.001"; Level = Info;  Source = "App";    Message = "Application started" }
     { Index = 1;  Timestamp = "12:00:00.012"; Level = Debug; Source = "Config"; Message = "Loaded config from /etc/app.yaml" }
     { Index = 2;  Timestamp = "12:00:00.045"; Level = Info;  Source = "DB";     Message = "Connected to PostgreSQL at localhost:5432" }
     { Index = 3;  Timestamp = "12:00:01.120"; Level = Debug; Source = "HTTP";   Message = "GET /health -> 200 OK (2ms)" }
     { Index = 4;  Timestamp = "12:00:02.340"; Level = Info;  Source = "Worker"; Message = "Scheduled 3 background jobs" }
     { Index = 5;  Timestamp = "12:00:03.001"; Level = Warn;  Source = "Cache";  Message = "Cache miss rate above 40% - consider pre-warming" }
     { Index = 6;  Timestamp = "12:00:03.550"; Level = Debug; Source = "HTTP";   Message = "POST /api/events -> 201 Created (8ms)" }
     { Index = 7;  Timestamp = "12:00:04.900"; Level = Error; Source = "DB";     Message = "Query timeout after 5000ms: SELECT * FROM events" }
     { Index = 8;  Timestamp = "12:00:04.910"; Level = Warn;  Source = "DB";     Message = "Retrying failed query (attempt 1/3)" }
     { Index = 9;  Timestamp = "12:00:05.120"; Level = Info;  Source = "DB";     Message = "Query retry succeeded" }
     { Index = 10; Timestamp = "12:00:06.200"; Level = Debug; Source = "HTTP";   Message = "GET /api/metrics -> 200 OK (1ms)" }
     { Index = 11; Timestamp = "12:00:07.001"; Level = Fatal; Source = "Worker"; Message = "Unhandled exception in job processor - shutting down worker" }
     { Index = 12; Timestamp = "12:00:07.010"; Level = Error; Source = "Worker"; Message = "Job 'email-send-queue' failed: SMTP connection refused" }
     { Index = 13; Timestamp = "12:00:08.500"; Level = Info;  Source = "Worker"; Message = "Worker restarted after crash" }
     { Index = 14; Timestamp = "12:00:09.000"; Level = Debug; Source = "Auth";   Message = "Token validated for user 42" }
     { Index = 15; Timestamp = "12:00:10.300"; Level = Info;  Source = "HTTP";   Message = "Server listening on :8080" }
     { Index = 16; Timestamp = "12:00:11.100"; Level = Warn;  Source = "Config"; Message = "Deprecated config key 'db.pool_size'" }
     { Index = 17; Timestamp = "12:00:12.050"; Level = Debug; Source = "Auth";   Message = "OAuth2 token refreshed for user 7" }
     { Index = 18; Timestamp = "12:00:13.200"; Level = Info;  Source = "App";    Message = "Checkpoint: 1000 events processed" }
     { Index = 19; Timestamp = "12:00:14.400"; Level = Trace; Source = "HTTP";   Message = "Connection from 10.0.0.1:54321 accepted" }
  |]

// ── Init ──────────────────────────────────────────────────────────────────────

let init () =
  { AllEntries = sampleEntries
    FilterInput = TextInput.empty
    LogList     = VirtualList.ofArray viewportHeight sampleEntries
    Toasts      = ToastQueue.empty
    Focus       = FocusRing.create [FilterField; ListField] },
  Cmd.none

// ── Update ────────────────────────────────────────────────────────────────────

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
  match msg with
  | Quit         -> model, Cmd.quit
  | ClipboardWritten -> model, Cmd.none
  | DismissToast id -> { model with Toasts = ToastQueue.dismiss id model.Toasts }, Cmd.none

  | KeyInput(key, mods) ->
    let focused = FocusRing.current model.Focus
    match key, mods, focused with
    | Escape, _, _ -> model, Cmd.quit

    | Tab, m, _ when m.HasFlag(Modifiers.Shift) ->
      { model with Focus = FocusRing.prev model.Focus }, Cmd.none
    | Tab, _, _ ->
      { model with Focus = FocusRing.next model.Focus }, Cmd.none

    | KeyChar '/', _, _ ->
      { model with Focus = FocusRing.create [FilterField; ListField] }, Cmd.none

    // Filter input receives all typed keys
    | _, _, Some FilterField ->
      let input' = TextInput.handleKey key model.FilterInput
      let list'  = rebuildList input'.Text model.AllEntries
      { model with FilterInput = input'; LogList = list' }, Cmd.none

    // List navigation
    | Key.Down, _, Some ListField ->
      { model with LogList = VirtualList.selectNext model.LogList }, Cmd.none
    | Key.Up, _, Some ListField ->
      { model with LogList = VirtualList.selectPrev model.LogList }, Cmd.none
    | KeyChar 'j', _, Some ListField ->
      { model with LogList = VirtualList.selectNext model.LogList }, Cmd.none
    | KeyChar 'k', _, Some ListField ->
      { model with LogList = VirtualList.selectPrev model.LogList }, Cmd.none
    | KeyChar 'g', _, Some ListField ->
      { model with LogList = VirtualList.selectFirst model.LogList }, Cmd.none
    | KeyChar 'G', _, Some ListField ->
      { model with LogList = VirtualList.selectLast model.LogList }, Cmd.none

    | Enter, _, Some ListField ->
      match model.LogList.SelectedIndex with
      | None -> model, Cmd.none
      | Some i ->
        let entry = model.LogList.Items[i]
        let text  = sprintf "[%s] %s %s: %s" entry.Timestamp (levelLabel entry.Level) entry.Source entry.Message
        let toasts', id = ToastQueue.push (sprintf "Copied entry #%d" entry.Index) 30 Style.empty model.Toasts
        { model with Toasts = toasts' },
        Cmd.batch [
          Cmd.copyToClipboard text ClipboardWritten
          Cmd.delay 3000 (DismissToast id)
        ]

    | _ -> model, Cmd.none

// ── View ──────────────────────────────────────────────────────────────────────

let theme = Theme.catppuccin

let private levelBadge (level: LogLevel) =
  El.text (sprintf " %s " (levelLabel level))
  |> El.bold
  |> El.fg (Color.Named(Black, Normal))
  |> El.bg (levelColor theme level)

let private renderEntry (isFocused: bool) (isSelected: bool) (entry: LogEntry) =
  let rowStyle =
    match isSelected, isFocused with
    | true, true  -> El.bg theme.Accent >> El.bold
    | true, false -> El.bg theme.Secondary
    | false, _    -> id

  El.row [
    El.text (sprintf " %s " entry.Timestamp) |> El.fg theme.TextDim |> El.width 16
    levelBadge entry.Level
    El.text " " |> El.width 1
    El.text entry.Source |> El.fg theme.Secondary |> El.width 10
    El.text " " |> El.width 1
    El.fill (El.text entry.Message |> El.fg theme.TextFg)
  ]
  |> rowStyle

let private renderList (model: Model) =
  let isFocused = FocusRing.current model.Focus = Some ListField
  let scroll    = model.LogList.ScrollOffset
  let total     = model.LogList.Items.Length
  let vh        = model.LogList.ViewportHeight
  let count     = min vh (total - scroll)

  match count with
  | 0 ->
    El.text " (no entries match the filter)" |> El.fg theme.TextDim |> El.dim
  | n ->
    Array.init n (fun i ->
      let abs = scroll + i
      renderEntry isFocused (model.LogList.SelectedIndex = Some abs) model.LogList.Items[abs])
    |> Array.toList
    |> El.column

let private renderFilter (model: Model) =
  let isFocused = FocusRing.current model.Focus = Some FilterField
  let inputEl   = TextInput.viewWithPlaceholder "filter by message, source, or level..." isFocused model.FilterInput

  let total   = model.AllEntries.Length
  let visible = model.LogList.Items.Length
  let counter =
    El.text (sprintf " %d/%d " visible total)
    |> El.fg (match visible < total with true -> theme.Warning | false -> theme.TextDim)

  El.row [
    El.text (if isFocused then "/ " else "  ") |> El.fg theme.Primary |> El.bold
    inputEl
    counter
  ]
  |> El.bordered (if isFocused then Rounded else Light)

let private renderDetail (model: Model) =
  match model.LogList.SelectedIndex with
  | None ->
    El.text " <- select an entry with j/k then press Enter to copy"
    |> El.fg theme.TextDim |> El.dim
  | Some i ->
    let e = model.LogList.Items[i]
    El.column [
      El.row [
        levelBadge e.Level
        El.text (sprintf "  %s  -  %s" e.Source e.Timestamp) |> El.fg theme.TextDim
      ]
      El.text e.Message |> El.fg theme.TextFg |> El.bold
      El.text "Press Enter to copy this entry to clipboard."
      |> El.fg theme.TextDim |> El.dim
    ]
    |> El.padAll 1

let view (model: Model) =
  let title =
    El.row [
      El.text " LogViewer " |> El.bold |> El.fg theme.Primary
      El.text " - sample 15 " |> El.fg theme.TextDim
      El.fill El.empty
      El.text "Tab " |> El.bold |> El.fg theme.Accent
      El.text "/ " |> El.bold |> El.fg theme.Accent
      El.text "j/k " |> El.bold |> El.fg theme.Accent
      El.text "Enter " |> El.bold |> El.fg theme.Accent
      El.text "Esc" |> El.bold |> El.fg theme.Accent
    ]
    |> El.padHV 1 0

  let listPanel =
    renderList model
    |> El.height viewportHeight
    |> El.bordered (if FocusRing.current model.Focus = Some ListField then Rounded else Light)

  let detailPanel =
    renderDetail model
    |> El.height 5
    |> El.bordered Light

  let toasts = ToastQueue.view model.Toasts

  El.column [
    title
    renderFilter model
    listPanel
    detailPanel
    toasts
  ]
  |> Theme.apply theme

// ── Subscribe / Program ───────────────────────────────────────────────────────

let subscribe _model =
  [ KeySub (fun (key, mods) -> Some (KeyInput(key, mods))) ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe
    OnError = None }

[<EntryPoint>]
let main _ = App.run program; 0
