/// 13-GitLog: A two-pane git log browser with live GC overlay.
///
/// Features:
///   - Scrollable VirtualList of commits (hash, author, date, subject columns)
///   - Right pane: async `git show --stat <hash>` via Cmd.ofCancellableAsync
///   - GC overlay: Δgen0 counter in the top-right corner (toggle with 'd')
///   - FuzzyFinder search on '/' key to filter commits by subject/author
///   - Arrow keys navigate, Enter selects, 'q'/Escape quits
///   - Cancels in-flight diff load when moving to a new commit
///
/// Run from the SageTUI repo root:
///   dotnet run --project samples/13-GitLog
module GitLog

open System
open System.Diagnostics
open SageTUI

// ─── Domain types ─────────────────────────────────────────────────────────────

type Commit = {
  Hash:    string
  Short:   string
  Author:  string
  Date:    string
  Subject: string
}

type DiffState =
  | NotSelected
  | Loading   of hash: string
  | Loaded    of hash: string * lines: string array
  | Failed    of hash: string * err: string

type Msg =
  | CommitsLoaded  of Commit list
  | CommitsFailed  of string
  | DiffLoaded     of hash: string * lines: string array
  | DiffFailed     of hash: string * string
  // Navigation — committed to VirtualList.handleEvent
  | ListUp | ListDown | ListPageUp | ListPageDown | ListHome | ListEnd
  | Select
  // Diff pane scroll
  | DiffScrollUp | DiffScrollDown
  // FuzzyFinder filter
  | FilterQueryKey of Key
  | FilterMoveUp | FilterMoveDown | FilterApply
  | OpenFilter | CloseFilter
  // GC / system
  | Resize of int * int
  | Tick
  | ToggleGcOverlay
  | Quit

type Model = {
  All:          Commit list
  Commits:      VirtualListModel<Commit>
  Filter:       FuzzyFinderModel<Commit>
  FilterOpen:   bool
  DiffState:    DiffState
  DiffScroll:   int
  ShowGc:       bool
  Gen0Baseline: int
  Gen0Delta:    int
  Width:        int
  Height:       int
}

// ─── Git helpers ──────────────────────────────────────────────────────────────

let runProcess (exe: string) (args: string) : Result<string, string> =
  try
    let psi =
      ProcessStartInfo(
        exe, args,
        RedirectStandardOutput = true,
        RedirectStandardError  = true,
        UseShellExecute        = false,
        CreateNoWindow         = true)
    use p = new Process()
    p.StartInfo <- psi
    p.Start() |> ignore
    let stdout = p.StandardOutput.ReadToEnd()
    let stderr = p.StandardError.ReadToEnd()
    p.WaitForExit()
    match p.ExitCode with
    | 0 -> Ok stdout
    | c -> Error (sprintf "exit %d: %s" c stderr)
  with ex ->
    Error ex.Message

let parseCommits (raw: string) : Commit list =
  raw.Split('\n', StringSplitOptions.RemoveEmptyEntries)
  |> Array.toList
  |> List.choose (fun line ->
      let parts = line.Split('|')
      match parts with
      | [| hash; author; date; subject |] when hash.Length >= 7 ->
        Some { Hash = hash; Short = hash.[..6]; Author = author; Date = date; Subject = subject }
      | _ -> None)

let loadCommitsAsync (dispatch: Msg -> unit) =
  async {
    do! Async.SwitchToThreadPool()
    let result =
      match runProcess "git" "log --format=%H|%an|%ad|%s --date=short -n 2000" with
      | Ok raw    -> CommitsLoaded (parseCommits raw)
      | Error msg -> CommitsFailed msg
    dispatch result
  }

let loadDiffAsync (hash: string) (dispatch: Msg -> unit) =
  async {
    do! Async.SwitchToThreadPool()
    let result =
      match runProcess "git" (sprintf "show --stat %s" hash) with
      | Ok raw    -> DiffLoaded (hash, raw.Split('\n'))
      | Error msg -> DiffFailed (hash, msg)
    dispatch result
  }

// ─── Init ─────────────────────────────────────────────────────────────────────

let listHeight (totalHeight: int) = max 1 (totalHeight - 3)

let init () =
  let h = 30
  let model = {
    All          = []
    Commits      = VirtualList.ofArray (listHeight h) [||]
    Filter       = FuzzyFinder.init (fun c -> sprintf "%s %s %s" c.Short c.Author c.Subject) [||]
    FilterOpen   = false
    DiffState    = NotSelected
    DiffScroll   = 0
    ShowGc       = true
    Gen0Baseline = GC.CollectionCount(0)
    Gen0Delta    = 0
    Width        = 80
    Height       = h
  }
  model, Cmd.ofAsync loadCommitsAsync

// ─── Update ───────────────────────────────────────────────────────────────────

let selectCurrent (model: Model) : Model * Cmd<Msg> =
  match VirtualList.selectedItem model.Commits with
  | None -> model, Cmd.none
  | Some c ->
    match model.DiffState with
    | Loading h when h = c.Hash -> model, Cmd.none
    | Loaded (h, _) when h = c.Hash -> model, Cmd.none
    | _ ->
      { model with DiffState = Loading c.Hash; DiffScroll = 0 },
      Cmd.ofCancellableAsync "diff" (fun _ct dispatch ->
        loadDiffAsync c.Hash dispatch)

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
  match msg with
  | CommitsLoaded commits ->
    let arr = commits |> List.toArray
    let commits' = VirtualList.ofArray (listHeight model.Height) arr
    let filter'  = FuzzyFinder.init (fun c -> sprintf "%s %s %s" c.Short c.Author c.Subject) arr
    { model with All = commits; Commits = commits'; Filter = filter' }, Cmd.none

  | CommitsFailed err ->
    { model with DiffState = Failed ("", err) }, Cmd.none

  | DiffLoaded (hash, lines) ->
    match model.DiffState with
    | Loading h when h = hash -> { model with DiffState = Loaded (hash, lines) }, Cmd.none
    | _ -> model, Cmd.none

  | DiffFailed (hash, err) ->
    match model.DiffState with
    | Loading h when h = hash -> { model with DiffState = Failed (hash, err) }, Cmd.none
    | _ -> model, Cmd.none

  | ListUp ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.Up, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | ListDown ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.Down, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | ListPageUp ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.PageUp, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | ListPageDown ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.PageDown, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | ListHome ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.Home, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | ListEnd ->
    let l' = VirtualList.handleEvent (KeyPressed(Key.End, Modifiers.None)) model.Commits
    selectCurrent { model with Commits = l' }

  | Select ->
    selectCurrent model

  | DiffScrollUp   -> { model with DiffScroll = max 0 (model.DiffScroll - 1) }, Cmd.none
  | DiffScrollDown -> { model with DiffScroll = model.DiffScroll + 1 }, Cmd.none

  | FilterQueryKey k ->
    let f' = FuzzyFinder.update (FFQueryKey k) model.Filter
    { model with Filter = f' }, Cmd.none

  | FilterMoveUp ->
    let f' = FuzzyFinder.update FFMoveUp model.Filter
    { model with Filter = f' }, Cmd.none

  | FilterMoveDown ->
    let f' = FuzzyFinder.update FFMoveDown model.Filter
    { model with Filter = f' }, Cmd.none

  | FilterApply ->
    match FuzzyFinder.selectedItem model.Filter with
    | None -> { model with FilterOpen = false }, Cmd.none
    | Some c ->
      let idx = model.All |> List.tryFindIndex (fun x -> x.Hash = c.Hash)
      let commits' =
        match idx with
        | None -> model.Commits
        | Some i -> VirtualList.handleEvent (KeyPressed(Key.Home, Modifiers.None)) model.Commits
                    |> fun l -> { l with SelectedIndex = Some i; ScrollOffset = max 0 (i - l.ViewportHeight / 2) }
      { model with FilterOpen = false; Commits = commits' }, Cmd.none

  | OpenFilter  -> { model with FilterOpen = true }, Cmd.none
  | CloseFilter -> { model with FilterOpen = false }, Cmd.none

  | Resize (w, h) ->
    let commits' = VirtualList.resize (listHeight h) model.Commits
    { model with Width = w; Height = h; Commits = commits' }, Cmd.none

  | Tick ->
    { model with Gen0Delta = GC.CollectionCount(0) - model.Gen0Baseline }, Cmd.none

  | ToggleGcOverlay -> { model with ShowGc = not model.ShowGc }, Cmd.none

  | Quit -> model, Cmd.quit

// ─── View ─────────────────────────────────────────────────────────────────────

let listConfig =
  VirtualList.create (fun selected (c: Commit) ->
    let subjectFg = if selected then El.fg (Color.Named(White, Bright)) else id
    let hashFg    = El.fg (Color.Named(Yellow, Normal))
    let dateFg    = El.fg (Color.Named(Cyan, Normal))
    let bg        = if selected then El.bg (Color.Named(Blue, Normal)) else id
    El.row [
      El.width 7  (hashFg (El.text c.Short))
      El.width 1  (El.text " ")
      El.width 11 (dateFg (El.text c.Date))
      El.width 1  (El.text " ")
      El.fill     (subjectFg (El.text c.Subject))
    ] |> bg)

let viewDiffPane (state: DiffState) (scroll: int) : Element =
  match state with
  | NotSelected ->
    El.fg (Color.Named(White, Normal))
      (El.text "  Select a commit to view its diff")

  | Loading hash ->
    El.fg (Color.Named(Yellow, Normal))
      (El.text (sprintf "  Loading %s…" hash.[..6]))

  | Failed (_, err) ->
    El.fg (Color.Named(Red, Normal))
      (El.text (sprintf "  Error: %s" err))

  | Loaded (_, lines) ->
    let startIdx = min scroll (max 0 (lines.Length - 1))
    let visible = lines |> Array.skip startIdx
    El.column (visible |> Array.toList |> List.map El.text)

let viewGcOverlay (delta: int) : Element =
  let color = match delta with 0 -> Color.Named(Green, Normal) | _ -> Color.Named(Yellow, Bright)
  El.fg color (El.bold (El.text (sprintf " Δgen0: %d " delta)))

let view (model: Model) : Element =
  let theme = Theme.nord

  let gcBadge =
    match model.ShowGc with
    | true  -> viewGcOverlay model.Gen0Delta
    | false -> El.empty

  let header =
    El.row [
      El.bold (El.fg theme.Accent (El.text " 13-GitLog "))
      El.fg theme.TextDim (El.text "│ ↑↓ navigate  Enter select  / filter  d GC overlay  q quit")
      El.fill (El.alignRight gcBadge)
    ]

  let listPane =
    El.bordered theme.Border (VirtualList.view listConfig model.Commits)
    |> El.width (model.Width * 6 / 10)

  let diffPane =
    El.bordered theme.Border (viewDiffPane model.DiffState model.DiffScroll)
    |> El.fill

  let body = El.row [ listPane; diffPane ]

  let filterBar =
    match model.FilterOpen with
    | false -> El.empty
    | true  ->
      let filterH = min 12 (model.Height / 3)
      El.bordered theme.Border (FuzzyFinder.view true filterH model.Filter)

  El.column [ header; body; filterBar ]
  |> Theme.apply theme

// ─── Subscriptions ────────────────────────────────────────────────────────────

let normalKeys : Sub<Msg> =
  Keys.bind [
    Key.Up,                                ListUp
    Key.Down,                              ListDown
    Key.PageUp,                            ListPageUp
    Key.PageDown,                          ListPageDown
    Key.Home,                              ListHome
    Key.End,                               ListEnd
    Key.Enter,                             Select
    Key.Char (Text.Rune '/'),              OpenFilter
    Key.Char (Text.Rune 'd'),              ToggleGcOverlay
    Key.Char (Text.Rune 'j'),              ListDown
    Key.Char (Text.Rune 'k'),              ListUp
    Key.Char (Text.Rune 'J'),              DiffScrollDown
    Key.Char (Text.Rune 'K'),              DiffScrollUp
    Key.Char (Text.Rune 'q'),              Quit
    Key.Escape,                            Quit
  ]

let filterQueryKeys : Sub<Msg> =
  KeySub (fun (key, mods) ->
    match key, mods with
    | Key.Char _, Modifiers.None        -> Some (FilterQueryKey key)
    | Key.Backspace, _ | Key.Delete, _  -> Some (FilterQueryKey key)
    | Key.Home, _ | Key.End, _          -> Some (FilterQueryKey key)
    | Key.Up, _                         -> Some FilterMoveUp
    | Key.Down, _                       -> Some FilterMoveDown
    | Key.Enter, _                      -> Some FilterApply
    | Key.Escape, _                     -> Some CloseFilter
    | _ -> None)

let subscribe (model: Model) : Sub<Msg> list =
  [
    match model.FilterOpen with
    | false -> normalKeys
    | true  -> filterQueryKeys
    TimerSub("tick", TimeSpan.FromMilliseconds(500.0), fun () -> Tick)
    ResizeSub(fun (w, h) -> Some (Resize(w, h)))
  ]

// ─── Program ──────────────────────────────────────────────────────────────────

let program : Program<Model, Msg> = {
  Init      = init
  Update    = update
  View      = view
  Subscribe = subscribe
}

[<EntryPoint>]
let main _ =
  App.run program
  0

