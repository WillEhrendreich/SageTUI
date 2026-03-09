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
      | KeyChar '+' | KeyChar '=' -> Some Increment
      | KeyChar '-' -> Some Decrement
      | KeyChar 'q' | KeyChar 'Q' | Escape -> Some Quit
      | _ -> None) ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _ = App.run program; 0
