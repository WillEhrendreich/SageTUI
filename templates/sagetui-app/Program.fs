open SageTUI

// Messages your app can receive
type Msg =
  | Increment
  | Decrement
  | Quit

// Initialize state and commands
let init () = 0, Cmd.none

// Handle messages, return new state + commands
let update msg count =
  match msg with
  | Increment -> count + 1, Cmd.none
  | Decrement -> count - 1, Cmd.none
  | Quit -> count, Cmd.quit

// Render state as terminal UI
let view count =
  El.column [
    El.text "SageTUI Counter" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
    El.text ""
    El.text (sprintf "  Count: %d" count) |> El.bold
    El.text ""
    El.text "  [j] increment  [k] decrement  [q] quit" |> El.dim
  ] |> El.padAll 1 |> El.bordered Rounded

// Wire it all together
let keyBindings =
  Keys.bind [
    keyChar 'j', Increment
    keyChar 'k', Decrement
    keyChar 'q', Quit
    Key.Escape, Quit
  ]

let program : Program<int, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [ keyBindings ]
    OnError = CrashOnError }

[<EntryPoint>]
let main _ = App.run program; 0
