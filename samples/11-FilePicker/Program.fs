/// Sample 11 — FilePicker
///
/// A two-pane file browser demonstrating how to compose multiple SageTUI
/// widgets into a cohesive UI:
///
///   Left pane  — FuzzyFinder over filenames in the current directory.
///                Type to filter; ↑/↓ to navigate; Enter to select.
///   Right pane — VirtualList showing the contents of the selected file
///                (or a directory listing), with keyboard scrolling.
///   Status bar — current path + selected filename + line count.
///
/// Key patterns shown:
///   - FuzzyFinder.init / FuzzyFinder.update / FuzzyFinder.view
///   - VirtualList.ofArray / VirtualList.view / VirtualList.handleEvent
///   - VirtualList.resize for handling terminal resize
///   - ResizeSub for terminal resize events
///   - Cmd.ofAsync for non-blocking file I/O
///   - El.row with proportional sizing (El.width / El.fill)
///   - Theme.nord for consistent styling throughout
module FilePicker

open System
open System.IO
open SageTUI

// ── Domain types ──────────────────────────────────────────────────────────────

type FileEntry = {
  Name     : string
  FullPath : string
  IsDir    : bool
  Size     : int64
}

type PreviewState =
  | NotLoaded
  | Loading
  | Lines of string array
  | Error of string

// ── Model & Msg ───────────────────────────────────────────────────────────────

type Model = {
  Directory   : string
  Files       : FuzzyFinderModel<FileEntry>
  Preview     : PreviewState
  PreviewList : VirtualListModel<string>
  Width       : int
  Height      : int
}

type Msg =
  | FilesLoaded    of FileEntry array
  | QueryKey       of Key
  | ListMoveUp
  | ListMoveDown
  | SelectFile
  | PreviewLoaded  of string array
  | PreviewFailed  of string
  | PreviewScrollUp
  | PreviewScrollDown
  | PreviewPageUp
  | PreviewPageDown
  | Resize         of int * int
  | Quit

// ── File system helpers ───────────────────────────────────────────────────────

let loadDirectory (path: string) : FileEntry array =
  let safeEntries () =
    try
      let dirs =
        Directory.GetDirectories(path)
        |> Array.map (fun d ->
          { Name = Path.GetFileName(d) + "/"; FullPath = d; IsDir = true; Size = 0L })
      let files =
        Directory.GetFiles(path)
        |> Array.map (fun f ->
          let info = FileInfo(f)
          { Name = info.Name; FullPath = f; IsDir = false; Size = info.Length })
      Array.append dirs files
    with _ -> [||]
  safeEntries ()

let readPreviewAsync (entry: FileEntry) (dispatch: Msg -> unit) =
  async {
    do! Async.SwitchToThreadPool()
    if entry.IsDir then
      let lines =
        try
          Directory.GetFileSystemEntries(entry.FullPath)
          |> Array.map (fun p ->
            let name = Path.GetFileName(p)
            if Directory.Exists(p) then "  📁 " + name + "/" else "  📄 " + name)
        with ex -> [| "  Error: " + ex.Message |]
      dispatch (PreviewLoaded lines)
    else
      let lines =
        try
          let info = FileInfo(entry.FullPath)
          match info.Length with
          | n when n > 512L * 1024L ->
            // Don't read files larger than 512 KB — show metadata instead
            [| sprintf "  Name:  %s" entry.Name
               sprintf "  Size:  %s" (
                 match n with
                 | s when s > 1024L * 1024L -> sprintf "%.1f MB" (float s / (1024.0 * 1024.0))
                 | s -> sprintf "%.1f KB" (float s / 1024.0))
               ""
               "  (File too large to preview)" |]
          | _ ->
            File.ReadAllLines(entry.FullPath)
        with ex -> [| "Error reading file: " + ex.Message |]
      dispatch (PreviewLoaded lines)
  } |> Async.StartImmediate

// ── Init ──────────────────────────────────────────────────────────────────────

let init () : Model * Cmd<Msg> =
  let dir = Directory.GetCurrentDirectory()
  let files = loadDirectory dir
  let viewportH = 24
  { Directory   = dir
    Files       = FuzzyFinder.init (fun e -> e.Name) files
    Preview     = NotLoaded
    PreviewList = VirtualList.ofArray viewportH [||]
    Width       = 80
    Height      = 30 },
  Cmd.none

// ── Update ────────────────────────────────────────────────────────────────────

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
  match msg with
  | FilesLoaded entries ->
    let ff' = FuzzyFinder.init (fun e -> e.Name) entries
    { model with Files = ff' }, Cmd.none

  | QueryKey key ->
    let ff' = FuzzyFinder.update (FFQueryKey key) model.Files
    let cmd =
      match FuzzyFinder.selectedItem ff' with
      | Some entry -> Cmd.ofAsync (fun dispatch -> async { readPreviewAsync entry dispatch })
      | None -> Cmd.none
    { model with Files = ff'; Preview = Loading }, cmd

  | ListMoveUp ->
    let ff' = FuzzyFinder.update FFMoveUp model.Files
    let cmd =
      match FuzzyFinder.selectedItem ff' with
      | Some entry -> Cmd.ofAsync (fun dispatch -> async { readPreviewAsync entry dispatch })
      | None -> Cmd.none
    { model with Files = ff'; Preview = Loading }, cmd

  | ListMoveDown ->
    let ff' = FuzzyFinder.update FFMoveDown model.Files
    let cmd =
      match FuzzyFinder.selectedItem ff' with
      | Some entry -> Cmd.ofAsync (fun dispatch -> async { readPreviewAsync entry dispatch })
      | None -> Cmd.none
    { model with Files = ff'; Preview = Loading }, cmd

  | SelectFile ->
    match FuzzyFinder.selectedItem model.Files with
    | None -> model, Cmd.none
    | Some entry ->
      match entry.IsDir with
      | true ->
        let entries = loadDirectory entry.FullPath
        let ff' = FuzzyFinder.init (fun e -> e.Name) entries
        { model with Directory = entry.FullPath; Files = ff'; Preview = NotLoaded }, Cmd.none
      | false ->
        model, Cmd.ofAsync (fun dispatch -> async { readPreviewAsync entry dispatch })

  | PreviewLoaded lines ->
    let listH = model.Height - 5  // leave room for header + status bar
    let list' = VirtualList.ofArray (max 1 listH) lines
    { model with Preview = Lines lines; PreviewList = list' }, Cmd.none

  | PreviewFailed msg ->
    { model with Preview = Error msg; PreviewList = VirtualList.ofArray 1 [||] }, Cmd.none

  | PreviewScrollUp ->
    let list' = VirtualList.handleEvent (KeyPressed(Key.Up, Modifiers.None)) model.PreviewList
    { model with PreviewList = list' }, Cmd.none

  | PreviewScrollDown ->
    let list' = VirtualList.handleEvent (KeyPressed(Key.Down, Modifiers.None)) model.PreviewList
    { model with PreviewList = list' }, Cmd.none

  | PreviewPageUp ->
    let list' = VirtualList.handleEvent (KeyPressed(Key.PageUp, Modifiers.None)) model.PreviewList
    { model with PreviewList = list' }, Cmd.none

  | PreviewPageDown ->
    let list' = VirtualList.handleEvent (KeyPressed(Key.PageDown, Modifiers.None)) model.PreviewList
    { model with PreviewList = list' }, Cmd.none

  | Resize (w, h) ->
    let listH = max 1 (h - 5)
    let list' = VirtualList.resize listH model.PreviewList
    { model with Width = w; Height = h; PreviewList = list' }, Cmd.none

  | Quit -> model, Cmd.quit

// ── View ──────────────────────────────────────────────────────────────────────

let view (model: Model) : Element =
  let theme = Theme.nord
  let leftW = model.Width / 3

  // ── Left pane: FuzzyFinder ──
  let ffH = model.Height - 4  // header + border padding + status bar
  let leftPane =
    El.column [
      El.text "Files" |> El.bold |> El.fg theme.Primary
      FuzzyFinder.view true ffH model.Files
    ]
    |> El.bordered theme.Border
    |> El.width leftW

  // ── Right pane: Preview ──
  let previewTitle =
    match FuzzyFinder.selectedItem model.Files with
    | Some e ->
      let icon = match e.IsDir with true -> "📁 " | false -> "📄 "
      El.text (icon + e.Name) |> El.bold |> El.fg theme.Primary
    | None -> El.text "Preview" |> El.bold |> El.fg theme.TextDim

  let previewContent =
    match model.Preview with
    | NotLoaded ->
      El.text "  Select a file on the left to preview it."
      |> El.fg theme.TextDim
    | Loading ->
      El.text "  Loading…" |> El.fg theme.Warning
    | Error msg ->
      El.text ("  Error: " + msg) |> El.fg theme.Error
    | Lines _ ->
      let listH = max 1 (model.Height - 6)
      let config = VirtualList.create (fun _selected line -> El.text ("  " + line))
                   |> VirtualList.withTheme theme
      VirtualList.view config { model.PreviewList with ViewportHeight = listH }

  let rightPane =
    El.column [
      previewTitle
      previewContent
    ]
    |> El.bordered theme.Border
    |> El.fill

  // ── Status bar ──
  let dirLabel = El.text (" 📂 " + model.Directory) |> El.fg theme.Secondary
  let countLabel =
    match model.Preview with
    | Lines lines -> El.text (sprintf "  %d lines " lines.Length) |> El.fg theme.TextDim
    | _ -> El.empty
  let helpLabel =
    El.text " ↑↓ navigate  Enter select  Tab preview  q quit"
    |> El.fg theme.TextDim

  let statusBar =
    El.row [ dirLabel; countLabel; helpLabel ]
    |> El.fg theme.TextFg

  El.column [
    El.text "SageTUI — FilePicker (Sample 11)" |> El.bold |> El.fg theme.Accent
    El.row [ leftPane; rightPane ]
    statusBar
  ]
  |> Theme.apply theme

// ── Subscriptions & key bindings ──────────────────────────────────────────────

// Query input key bindings: printable chars + editing keys go to FuzzyFinder
let queryKeys : Sub<Msg> =
  KeySub (fun (key, mods) ->
    match key, mods with
    | Key.Char _, Modifiers.None -> Some (QueryKey key)
    | Key.Backspace, _ | Key.Delete, _ -> Some (QueryKey key)
    | Key.Home, _ | Key.End, _ -> Some (QueryKey key)
    | Key.Char r, Modifiers.Ctrl
      when List.contains r [ Text.Rune 'a'; Text.Rune 'e'; Text.Rune 'k'
                             Text.Rune 'u'; Text.Rune 'w'; Text.Rune 'y' ] ->
      Some (QueryKey key)
    | _ -> None)

// Arrow navigation goes to FuzzyFinder list
let navKeys = Keys.bind [
  Key.Up,    ListMoveUp
  Key.Down,  ListMoveDown
  Key.Enter, SelectFile ]

// Preview pane scrolling
let previewKeys = Keys.bind [
  Key.Tab,     PreviewScrollDown
  Key.PageUp,  PreviewPageUp
  Key.PageDown, PreviewPageDown ]

let quitKeys = Keys.bind [
  Key.Char (Text.Rune 'q'), Quit
  Key.Escape, Quit ]

let subscribe (model: Model) : Sub<Msg> list =
  [ queryKeys; navKeys; previewKeys; quitKeys
    ResizeSub (fun (w, h) -> Some (Resize(w, h))) ]

// ── Program ───────────────────────────────────────────────────────────────────

let program : Program<Model, Msg> =
  { Init = init; Update = update; View = view; Subscribe = subscribe }

[<EntryPoint>]
let main _ =
  App.run program
  0
