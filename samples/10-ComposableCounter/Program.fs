/// Sample 10 — Composable Counter
///
/// Demonstrates how two independent counter components are composed into a
/// parent program using `Program.map`. Each counter is a self-contained
/// Elm Architecture component. `Program.map` lifts each into the parent's
/// model and message type — zero shared state, zero coupling.
///
/// Key patterns shown:
///   - `Program.map toMsg toModel withModel child`
///   - `Cmd.map` propagation through child commands
///   - `El.grid` for automatic 2-column layout
///   - `Theme.forProgram` for one-liner theming
///   - Per-counter key bindings via `Keys.bind`
module ComposableCounter

open System
open SageTUI

// ── Counter component ──────────────────────────────────────────────────────

type CounterModel = { Count: int; Label: string }
type CounterMsg = Increment | Decrement | Reset

let counterInit (label: string) () : CounterModel * Cmd<CounterMsg> =
  { Count = 0; Label = label }, Cmd.none

let counterUpdate (msg: CounterMsg) (model: CounterModel) : CounterModel * Cmd<CounterMsg> =
  match msg with
  | Increment -> { model with Count = model.Count + 1 }, Cmd.none
  | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
  | Reset     -> { model with Count = 0 },               Cmd.none

let counterView (model: CounterModel) : Element =
  El.column [
    El.text model.Label |> El.bold
    El.text (sprintf "  %d" model.Count)
      |> El.fg (match model.Count with
                | n when n > 0 -> Color.Named(Green, Bright)
                | n when n < 0 -> Color.Named(Red, Bright)
                | _             -> Color.Named(White, Normal))
    El.text "  [+] inc  [-] dec  [0] reset" |> El.fg (Color.Named(Black, Bright))
  ] |> El.bordered Rounded

let makeCounter (label: string) : Program<CounterModel, CounterMsg> =
  { Init      = counterInit label
    Update    = counterUpdate
    View      = counterView
    Subscribe = fun _ -> []
    OnError   = None }

type Model = { Left: CounterModel; Right: CounterModel }

type Msg =
  | LeftMsg  of CounterMsg
  | RightMsg of CounterMsg
  | Quit

// ── Lift child programs via Program.map ────────────────────────────────────

let leftCounter =
  Program.map LeftMsg (fun m -> m.Left) (fun c m -> { m with Left = c }) (makeCounter "Counter A")

let rightCounter =
  Program.map RightMsg (fun m -> m.Right) (fun c m -> { m with Right = c }) (makeCounter "Counter B")

// ── Parent program ─────────────────────────────────────────────────────────

let init () : Model * Cmd<Msg> =
  let m = { Left = { Count = 0; Label = "Counter A" }; Right = { Count = 0; Label = "Counter B" } }
  let m, cmdL = leftCounter.Init m
  let m, cmdR = rightCounter.Init m
  m, Cmd.batch [ cmdL; cmdR ]

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
  match msg with
  | LeftMsg  cm -> leftCounter.Update  cm model
  | RightMsg cm -> rightCounter.Update cm model
  | Quit        -> model, Cmd.quit

let view (model: Model) : Element =
  El.column [
    El.text "SageTUI — Composable Counter Demo" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
    El.text "" // spacer
    El.grid 2 EqualWidth [
      leftCounter.View  model
      rightCounter.View model
    ]
    El.text "" // spacer
    El.text "Press [q] or [Esc] to quit" |> El.fg (Color.Named(Black, Bright))
  ]

// Per-component key bindings — each counter has its own keys
let leftBindings = Keys.bind [
  Key.Char (System.Text.Rune '+'), LeftMsg Increment
  Key.Char (System.Text.Rune '-'), LeftMsg Decrement
  Key.Char (System.Text.Rune '0'), LeftMsg Reset ]
let rightBindings = Keys.bind [
  Key.Char (System.Text.Rune '='), RightMsg Increment
  Key.Char (System.Text.Rune '_'), RightMsg Decrement
  Key.Char (System.Text.Rune ')'), RightMsg Reset ]
let quitBindings = Keys.bind [
  Key.Char (System.Text.Rune 'q'), Quit
  Key.Escape, Quit ]

let subscribe (_: Model) : Sub<Msg> list =
  [ leftBindings; rightBindings; quitBindings ]

let rawProgram : Program<Model, Msg> =
  { Init = init; Update = update; View = view; Subscribe = subscribe; OnError = None }

let program = Theme.forProgram Theme.nord rawProgram

[<EntryPoint>]
let main _ =
  App.run program
  0

