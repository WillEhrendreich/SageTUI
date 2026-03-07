module Counter

open SageTUI

type Model = { Count: int }

type Msg =
  | Increment
  | Decrement
  | Quit

let init () =
  { Count = 0 }, Cmd.none

let update msg model =
  match msg with
  | Increment -> { model with Count = model.Count + 1 }, Cmd.none
  | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
  | Quit -> model, Cmd.quit

let view model =
  El.column [
    El.text (sprintf "Count: %d" model.Count)
    El.text ""
    El.text "[+] Increment  [-] Decrement  [q] Quit"
  ]

let subscribe _model =
  [ KeySub (fun (key, _mods) ->
      match key with
      | Char '+' | Char '=' -> Some Increment
      | Char '-' -> Some Decrement
      | Char 'q' | Char 'Q' | Escape -> Some Quit
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
