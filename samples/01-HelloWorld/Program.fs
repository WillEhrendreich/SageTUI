module HelloWorld

// The simplest possible SageTUI app.
// Demonstrates: TEA + TextInput widget, borders, padding, colors.

open SageTUI

type Model = { Input: TextInputModel }

type Msg =
  | KeyPressed of Key * Modifiers
  | Quit

let init () = { Input = TextInput.empty }, Cmd.none

let update msg model =
  match msg with
  | Quit -> model, Cmd.quit
  | KeyPressed (key, _) ->
    { model with Input = TextInput.handleKey key model.Input }, Cmd.none

let view model =
  let greeting =
    match model.Input.Text with
    | "" -> "World"
    | name -> name
  El.column [
    El.text $"Hello, {greeting}!"
      |> El.bold
      |> El.fg (Color.Rgb(255uy, 200uy, 50uy))
      |> El.padHV 2 0
      |> El.bordered Rounded
    El.text ""
    El.text "Type your name:" |> El.dim
    El.row [
      El.text "▸ " |> El.fg (Color.Named(Green, Bright))
      TextInput.view true model.Input
    ]
    El.text ""
    El.text "[Esc] Quit" |> El.dim
  ]
  |> El.padAll 1

let subscribe _model =
  [ KeySub (fun (key, _mods) ->
      match key with
      | Escape -> Some Quit
      | _ -> Some (KeyPressed(key, _mods))) ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _argv =
  let profile =
    Detect.fromEnvironment
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
      (fun () -> System.Console.WindowWidth, System.Console.WindowHeight)
    |> Detect.adjustForMultiplexer
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
    |> UserOverride.apply
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
  let backend = Backend.create profile
  App.run backend program
  0
