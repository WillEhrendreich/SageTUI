module HelloWorld

// The simplest possible SageTUI app.
// Demonstrates: TEA architecture, styled text, keyboard input, borders, padding.

open SageTUI

type Model = { Name: string }

type Msg =
  | TypeChar of char
  | DeleteChar
  | Quit

let init () =
  { Name = "" }, Cmd.none

let update msg model =
  match msg with
  | TypeChar c ->
    { model with Name = model.Name + string c }, Cmd.none
  | DeleteChar ->
    match model.Name.Length > 0 with
    | true -> { model with Name = model.Name.[..model.Name.Length - 2] }, Cmd.none
    | false -> model, Cmd.none
  | Quit -> model, Cmd.quit

let view model =
  let greeting =
    match model.Name with
    | "" -> "World"
    | n -> n
  El.column [
    El.row [
      El.text "╔═══════════════════════════════╗"
    ]
    |> El.fg (Color.Named(Cyan, Bright))
    El.row [
      El.text "║  " |> El.fg (Color.Named(Cyan, Bright))
      El.text (sprintf "Hello, %s!" greeting)
        |> El.bold
        |> El.fg (Color.Rgb(255uy, 200uy, 50uy))
      El.fill (El.text "")
      El.text "  ║" |> El.fg (Color.Named(Cyan, Bright))
    ]
    El.row [
      El.text "╚═══════════════════════════════╝"
    ]
    |> El.fg (Color.Named(Cyan, Bright))
    El.text ""
    El.text "Type your name:" |> El.dim
    El.row [
      El.text "▸ "
        |> El.fg (Color.Named(Green, Bright))
      El.text (model.Name + "▌")
        |> El.underline
    ]
    El.text ""
    El.text "[Backspace] Delete  [Esc/q] Quit" |> El.dim
  ]
  |> El.padAll 1

let subscribe _model =
  [ KeySub (fun (key, _mods) ->
      match key with
      | Char c when c <> 'q' -> Some (TypeChar c)
      | Char 'q' -> Some Quit
      | Backspace -> Some DeleteChar
      | Escape -> Some Quit
      | _ -> None) ]

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
