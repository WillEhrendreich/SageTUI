module SageTUI.ContainerProbe

open SageTUI

type Model =
  | WaitingForEnter
  | EnterReceived

type Msg =
  | Advance

let init () = WaitingForEnter, Cmd.none

let update message _model =
  match message with
  | Advance ->
    // Deliberately observable outside the alternate screen: the container
    // integration test requires this after injecting CR into a real PTY.
    printfn "PTY_INPUT_CONFIRMED"
    EnterReceived, Cmd.quit

let view model =
  match model with
  | WaitingForEnter -> El.text "WAITING_FOR_ENTER"
  | EnterReceived -> El.text "ENTER_RECEIVED"

let subscribe _model =
  [ KeySub (fun (key, _modifiers) ->
      match key with
      | Key.Enter -> Some Advance
      | _ -> None) ]

[<EntryPoint>]
let main _argv =
  App.run {
    Init = init
    Update = update
    View = view
    Subscribe = subscribe
    OnError = CrashOnError
  }
  0
