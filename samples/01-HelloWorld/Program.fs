module HelloWorld

// The simplest possible SageTUI app.
// Demonstrates: TEA pattern, keyboard bindings, borders, padding, colors.

open System
open SageTUI

type Msg = Increment | Decrement | Quit | NextDemoStep

type Model = { Count: int; DemoStep: int }

// ─── Demo Mode ───────────────────────────────────────────────────────────────

let isDemoMode = Environment.GetEnvironmentVariable("SAGETUI_DEMO_MODE") = "1"

// (pauseAfterMs, actionOption) — None = pause only, no state change
let demoSteps : (int * Msg option) list = [
  300,  Some Increment   // 1
  300,  Some Increment   // 2
  300,  Some Increment   // 3
  350,  Some Increment   // 4
  400,  Some Increment   // 5
  800,  None             // pause at 5 — let viewer read it
  250,  Some Decrement   // 4
  250,  Some Decrement   // 3
  250,  Some Decrement   // 2
  250,  Some Decrement   // 1
  300,  Some Decrement   // 0
  900,  None             // pause at 0 — reset loop
]

// ─── TEA ─────────────────────────────────────────────────────────────────────

let init () =
  let m = { Count = 0; DemoStep = 0 }
  let cmd = match isDemoMode with true -> Cmd.delay 600 NextDemoStep | false -> Cmd.none
  m, cmd

let rec update msg model =
  match msg with
  | Increment -> { model with Count = model.Count + 1 }, Cmd.none
  | Decrement -> { model with Count = max 0 (model.Count - 1) }, Cmd.none
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

let view model =
  El.column [
    El.text "Hello, SageTUI!"
      |> El.bold
      |> El.fg (Color.Rgb(255uy, 200uy, 50uy))
    El.text ""
    El.text (sprintf "  Count: %d" model.Count) |> El.bold
    El.text ""
    El.text "  [j] increment  [k] decrement  [q] quit" |> El.dim
  ]
  |> El.padAll 1
  |> El.bordered Rounded

let keyBindings =
  Keys.bind [
    Key.Char (System.Text.Rune 'j'), Increment
    Key.Char (System.Text.Rune 'k'), Decrement
    Key.Char (System.Text.Rune 'q'), Quit
    Key.Escape, Quit
  ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [ keyBindings ]
    OnError = CrashOnError }

[<EntryPoint>]
let main _ = App.run program; 0
