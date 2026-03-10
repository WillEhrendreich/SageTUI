module FocusRingSample

// FocusRing showcase: cycle between panels with Tab / Shift+Tab.
// Demonstrates: FocusRing.next/prev/isFocusedAt, El.borderedWithTitle,
// TextInput, ProgressBar, Select — all in one live interactive demo.

open System.Text
open SageTUI

// ─── Domain ─────────────────────────────────────────────────────────────────

type Panel = SearchBox | VolumeSlider | ColorPicker | Summary

let allPanels = [ SearchBox; VolumeSlider; ColorPicker; Summary ]
let panelIndex p = List.findIndex ((=) p) allPanels

type Model =
  { Focus:    FocusRing<Panel>
    Search:   TextInputModel
    Volume:   int
    ColorSel: SelectModel<string> }

type Msg =
  | NextFocus
  | PrevFocus
  | SearchKey  of Key
  | VolumeUp
  | VolumeDown
  | DropToggle
  | DropDown
  | DropUp
  | DropConfirm
  | Quit

// ─── Init ────────────────────────────────────────────────────────────────────

let colorNames = [ "Catppuccin Mocha"; "Dracula"; "Solarized Dark"; "Nord"; "Gruvbox" ]

let init () =
  { Focus    = FocusRing.create allPanels
    Search   = TextInput.empty
    Volume   = 50
    ColorSel = Select.create colorNames }, Cmd.none

// ─── Update ──────────────────────────────────────────────────────────────────

let update msg (model: Model) =
  match msg with
  | NextFocus    -> { model with Focus = FocusRing.next model.Focus }, Cmd.none
  | PrevFocus    -> { model with Focus = FocusRing.prev model.Focus }, Cmd.none
  | SearchKey k  -> { model with Search = TextInput.handleKey k model.Search }, Cmd.none
  | VolumeUp     -> { model with Volume = min 100 (model.Volume + 5) }, Cmd.none
  | VolumeDown   -> { model with Volume = max 0   (model.Volume - 5) }, Cmd.none
  | DropToggle   -> { model with ColorSel = Select.toggle   model.ColorSel }, Cmd.none
  | DropDown     -> { model with ColorSel = Select.moveDown model.ColorSel }, Cmd.none
  | DropUp       -> { model with ColorSel = Select.moveUp   model.ColorSel }, Cmd.none
  | DropConfirm  -> { model with ColorSel = Select.confirm  model.ColorSel }, Cmd.none
  | Quit         -> model, Cmd.quit

// ─── View helpers ────────────────────────────────────────────────────────────

let focusBox panel (model: Model) title child =
  let focused = FocusRing.isFocusedAt (panelIndex panel) model.Focus
  let style   = match focused with true -> BorderStyle.Double | false -> BorderStyle.Rounded
  let label   = match focused with true -> sprintf " ▶ %s ◀ " title | false -> sprintf "  %s  " title
  El.borderedWithTitle label style child
  |> (match focused with true -> El.fg Color.cyan | false -> El.fg Color.brightBlack)

let searchView (model: Model) =
  let focused = FocusRing.isFocusedAt (panelIndex SearchBox) model.Focus
  let input   = TextInput.viewWithPlaceholder "Search SageTUI…" focused model.Search
  let results =
    match model.Search.Text.Length > 0 with
    | false -> El.empty
    | true  ->
      El.column [
        El.text "─────────────────────" |> El.fg Color.brightBlack
        El.textf "  Found: %s"  model.Search.Text |> El.fg Color.yellow
        El.text  "  Widget reference"              |> El.fg Color.brightBlack
        El.text  "  API guide"                     |> El.fg Color.brightBlack
      ]
  El.column [ input; results ]
  |> focusBox SearchBox model "Search"

let volumeView (model: Model) =
  let pct = float model.Volume / 100.0
  let cfg = { ProgressBar.defaults with
                Percent     = pct
                Width       = 22
                ShowLabel   = true
                FilledColor = Some Color.green
                EmptyColor  = Some (Color.Named(BaseColor.Black, Bright)) }
  El.column [
    ProgressBar.view cfg
    El.textf "  %d%%  (↑↓ to adjust)" model.Volume |> El.fg Color.brightGreen
  ]
  |> focusBox VolumeSlider model "Volume"

let colorView (model: Model) =
  let focused = FocusRing.isFocusedAt (panelIndex ColorPicker) model.Focus
  Select.view id focused model.ColorSel
  |> focusBox ColorPicker model "Color Theme"

let summaryView (model: Model) =
  let focused   = FocusRing.isFocusedAt (panelIndex Summary) model.Focus
  let colorStr  = Select.selectedValue model.ColorSel |> Option.defaultValue "(none)"
  let searchStr = match model.Search.Text with "" -> "(empty)" | t -> sprintf "'%s'" t
  El.column [
    El.text "Live state snapshot:" |> El.bold
    El.text " "
    El.textf "  Theme  : %s" colorStr  |> El.fg Color.cyan
    El.textf "  Volume : %d%%" model.Volume |> El.fg Color.green
    El.textf "  Search : %s" searchStr      |> El.fg Color.yellow
    El.text " "
    match focused with
    | true  -> El.text " ← watching live!" |> El.bold |> El.fg Color.magenta
    | false -> El.empty
  ]
  |> focusBox Summary model "Summary"

let focusStrip (model: Model) =
  El.row (
    allPanels
    |> List.mapi (fun i p ->
      let active = FocusRing.isFocusedAt i model.Focus
      let label  = sprintf " %d:%A " (i + 1) p
      El.text label
      |> (match active with
          | true  -> El.bold >> El.fg Color.black >> El.bg Color.cyan
          | false -> El.fg Color.brightBlack)))

let view (model: Model) =
  let panelName =
    FocusRing.current model.Focus
    |> Option.map (sprintf "%A")
    |> Option.defaultValue "—"
  El.column [
    El.row [
      El.text "  SageTUI · FocusRing Demo" |> El.bold |> El.fg Color.cyan
      El.fill El.empty
      El.textf " panel: %-12s  Tab/⇧Tab cycle  q quit " panelName
      |> El.fg Color.brightBlack
    ]
    El.text " "
    El.row [
      El.column [ searchView model; El.text " "; volumeView model ] |> El.width 36
      El.text "  "
      El.column [ colorView  model; El.text " "; summaryView model ] |> El.fill
    ]
    El.text " "
    focusStrip model
  ]

// ─── Subscriptions ────────────────────────────────────────────────────────────

// Global navigation bindings (active regardless of focus).
let navBindings =
  Keys.bind [
    Key.Tab,             NextFocus
    Key.Escape,          Quit
    Key.Char (Rune 'q'), Quit
  ]

let navShiftBindings =
  Keys.bindWithMods [ (Key.Tab, Modifiers.Shift), PrevFocus ]

// Search panel: forward all printable keys plus cursor-movement keys.
let searchBindings =
  KeySub(fun (key, _mods) ->
    match key with
    | Key.Char _ | Key.Backspace | Key.Delete
    | Key.Left  | Key.Right
    | Key.Home  | Key.End       -> Some (SearchKey key)
    | _                          -> None)

// Volume panel bindings.
let volumeBindings =
  Keys.bind [ Key.Up, VolumeUp; Key.Down, VolumeDown ]

// Color picker bindings.
let colorBindings =
  Keys.bind [
    Key.Enter,           DropConfirm
    Key.Char (Rune ' '), DropToggle
    Key.Up,              DropUp
    Key.Down,            DropDown
  ]

let subscribe (model: Model) =
  let panel = FocusRing.current model.Focus
  let panelSubs =
    match panel with
    | Some SearchBox    -> [ searchBindings ]
    | Some VolumeSlider -> [ volumeBindings ]
    | Some ColorPicker  -> [ colorBindings  ]
    | _                 -> []
  navBindings :: navShiftBindings :: panelSubs

// ─── Entry point ─────────────────────────────────────────────────────────────

let program : Program<Model, Msg> =
  { Init      = init
    Update    = update
    View      = view
    Subscribe = subscribe
    OnError = CrashOnError }

[<EntryPoint>]
let main _ =
  App.run program
  0
