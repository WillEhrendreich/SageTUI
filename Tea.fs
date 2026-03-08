namespace SageTUI

open System
open System.Threading

/// A command to be executed by the runtime. Commands produce messages asynchronously.
type Cmd<'msg> =
  | NoCmd
  | Batch of Cmd<'msg> list
  | OfAsync of (('msg -> unit) -> Async<unit>)
  | OfCancellableAsync of id: string * (CancellationToken -> ('msg -> unit) -> Async<unit>)
  | CancelSub of string
  | Delay of milliseconds: int * 'msg
  /// Quit the application with the given exit code (0 = success).
  | Quit of exitCode: int

/// Command constructors and combinators.
module Cmd =
  /// No-op command.
  let none = NoCmd
  /// Combine multiple commands into one.
  let batch cmds = Batch cmds
  /// Run an async function that dispatches messages.
  let ofAsync f = OfAsync f
  /// Run a cancellable async function with a subscription ID.
  let ofCancellableAsync id f = OfCancellableAsync(id, f)
  /// Cancel a subscription by ID.
  let cancel id = CancelSub id
  /// Dispatch a message after a delay in milliseconds.
  let delay ms msg = Delay(ms, msg)
  /// Quit with exit code 0 (success).
  let quit = Quit 0
  /// Quit with an explicit exit code. Use non-zero for error conditions.
  let quitWith (code: int) = Quit code

  /// Write `text` to the system clipboard via OSC 52.
  /// Supported by most modern terminals (kitty, WezTerm, iTerm2, Windows Terminal, tmux ≥3.2).
  /// The `written` message is dispatched after the write completes.
  let copyToClipboard (text: string) (written: 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        printf "%s" (Ansi.osc52Copy text)
        dispatch written
      })

  /// Transform the message type of a command.
  let rec map (f: 'a -> 'b) (cmd: Cmd<'a>) : Cmd<'b> =
    match cmd with
    | NoCmd -> NoCmd
    | Batch cmds -> Batch(List.map (map f) cmds)
    | OfAsync run -> OfAsync(fun dispatch -> run (f >> dispatch))
    | OfCancellableAsync(id, run) ->
      OfCancellableAsync(id, fun ct dispatch -> run ct (f >> dispatch))
    | CancelSub id -> CancelSub id
    | Delay(ms, msg) -> Delay(ms, f msg)
    | Quit code -> Quit code

  /// Dispatch a message immediately (synchronous, no delay).
  let ofMsg (msg: 'msg) : Cmd<'msg> = Delay(0, msg)

  /// Run a Task and dispatch the result as a message.
  /// If the task throws, the exception is logged to stderr and ignored — the app
  /// continues running. For controlled error handling use <see cref="ofTaskResult"/>.
  let ofTask (task: unit -> Threading.Tasks.Task<'a>) (toMsg: 'a -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = task() |> Async.AwaitTask
          dispatch (toMsg result)
        with ex ->
          eprintfn "SageTUI Cmd.ofTask: unhandled exception — %s" ex.Message
      })

  /// Run a Task with success/error message mapping.
  let ofTaskResult
    (task: unit -> Threading.Tasks.Task<'a>)
    (onOk: 'a -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = task() |> Async.AwaitTask
          dispatch (onOk result)
        with ex ->
          dispatch (onError ex)
      })

  /// Extract all synchronously-dispatchable messages from a Cmd tree.
  /// Returns messages from every Delay case (regardless of delay duration).
  /// Async operations and subscriptions are not executed.
  /// Useful for asserting what messages a command will eventually produce in unit tests.
  let rec toMessages (cmd: Cmd<'msg>) : 'msg list =
    match cmd with
    | NoCmd | OfAsync _ | OfCancellableAsync _ | CancelSub _ | Quit _ -> []
    | Batch cmds -> cmds |> List.collect toMessages
    | Delay(_, msg) -> [ msg ]

/// A subscription that connects external events to messages.
type Sub<'msg> =
  | KeySub of (Key * Modifiers -> 'msg option)
  | MouseSub of (MouseEvent -> 'msg option)
  | ClickSub of (MouseEvent * string option -> 'msg option)
  | FocusSub of (FocusDirection -> 'msg option)
  | TimerSub of id: string * interval: TimeSpan * tick: (unit -> 'msg)
  | ResizeSub of (int * int -> 'msg)
  | CustomSub of id: string * start: (('msg -> unit) -> CancellationToken -> Async<unit>)

and FocusDirection =
  | FocusNext
  | FocusPrev

/// Key binding helpers for zero-ceremony keyboard subscriptions.
module Keys =
  /// Create a KeySub from a list of key-to-message bindings.
  /// Unmatched keys are ignored.
  let bind (bindings: (Key * 'msg) list) : Sub<'msg> =
    let lookup = System.Collections.Generic.Dictionary(bindings.Length)
    for (k, msg) in bindings do
      lookup[k] <- msg
    KeySub(fun (k, _mods) ->
      match lookup.TryGetValue(k) with
      | true, msg -> Some msg
      | false, _ -> None)

  /// Create a KeySub from key+modifier bindings.
  let bindWithMods (bindings: ((Key * Modifiers) * 'msg) list) : Sub<'msg> =
    KeySub(fun (k, mods) ->
      bindings
      |> List.tryPick (fun ((bk, bm), msg) ->
        match bk = k && bm = mods with
        | true -> Some msg
        | false -> None))

/// Subscription combinators.
module Sub =
  /// Transform the message type of a subscription.
  let map (f: 'a -> 'b) (sub: Sub<'a>) : Sub<'b> =
    match sub with
    | KeySub handler -> KeySub(fun input -> handler input |> Option.map f)
    | MouseSub handler -> MouseSub(fun input -> handler input |> Option.map f)
    | ClickSub handler -> ClickSub(fun input -> handler input |> Option.map f)
    | FocusSub handler -> FocusSub(fun input -> handler input |> Option.map f)
    | TimerSub(id, interval, tick) -> TimerSub(id, interval, tick >> f)
    | ResizeSub handler -> ResizeSub(fun input -> handler input |> f)
    | CustomSub(id, start) ->
      CustomSub(id, fun dispatch ct -> start (f >> dispatch) ct)

/// The Elm Architecture program definition. Init/Update/View/Subscribe.
type Program<'model, 'msg> = {
  Init: unit -> 'model * Cmd<'msg>
  Update: 'msg -> 'model -> 'model * Cmd<'msg>
  View: 'model -> Element
  Subscribe: 'model -> Sub<'msg> list
}

/// Program combinators for component composition.
module Program =
  /// Transform a component's program to work within a parent program.
  /// `toMsg` wraps child messages into parent messages.
  /// `toModel` extracts the child model from the parent model.
  /// `withModel` sets the child model into the parent model.
  let map
    (toMsg: 'childMsg -> 'parentMsg)
    (toModel: 'parentModel -> 'childModel)
    (withModel: 'childModel -> 'parentModel -> 'parentModel)
    (child: Program<'childModel, 'childMsg>)
    : {| Init: 'parentModel -> 'parentModel * Cmd<'parentMsg>
         Update: 'childMsg -> 'parentModel -> 'parentModel * Cmd<'parentMsg>
         View: 'parentModel -> Element
         Subscribe: 'parentModel -> Sub<'parentMsg> list |} =
    {| Init = fun parentModel ->
         let childModel, childCmd = child.Init()
         withModel childModel parentModel, Cmd.map toMsg childCmd
       Update = fun childMsg parentModel ->
         let childModel = toModel parentModel
         let newChildModel, childCmd = child.Update childMsg childModel
         withModel newChildModel parentModel, Cmd.map toMsg childCmd
       View = fun parentModel ->
         child.View (toModel parentModel)
       Subscribe = fun parentModel ->
         child.Subscribe (toModel parentModel)
         |> List.map (Sub.map toMsg) |}

  /// Run a list of messages through a program's Update function and return all
  /// intermediate (model, cmd) states. The initial model comes from Init.
  /// Useful for unit-testing update logic without spinning up a terminal.
  ///
  /// Example:
  /// ```fsharp
  /// let states = Program.simulate [Increment; Increment; Decrement] myProgram
  /// let finalModel = states |> List.last |> fst
  /// ```
  let simulate (msgs: 'msg list) (program: Program<'model, 'msg>) : ('model * Cmd<'msg>) list =
    let initModel, _ = program.Init()
    msgs
    |> List.scan (fun (m, _) msg -> program.Update msg m) (initModel, Cmd.none)
    |> List.tail

type TerminalBackend = {
  Size: unit -> int * int
  Write: string -> unit
  Flush: unit -> unit
  PollEvent: int -> TerminalEvent option
  EnterRawMode: unit -> unit
  LeaveRawMode: unit -> unit
  Profile: TerminalProfile
}

module SafeProfile =
  let minimum (w: int) (h: int) : TerminalProfile =
    let platform =
      match System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) with
      | true -> Windows
      | false ->
        match System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX) with
        | true -> MacOS
        | false -> Linux
    { Color = ColorCapability.Basic16
      Unicode = UnicodeCapability.AsciiOnly
      Graphics = GraphicsCapability.TextOnly
      Input = InputCapability.BasicKeys
      Output = OutputCapability.RawMode
      Size = (w, h)
      TermName = "unknown"
      Platform = platform }

module TestBackend =
  let create (width: int) (height: int) (events: TerminalEvent list) : TerminalBackend * (unit -> string) =
    let output = System.Text.StringBuilder()
    let eventQueue = System.Collections.Generic.Queue(events)
    let backend =
      { Size = fun () -> width, height
        Write = fun s -> output.Append(s) |> ignore
        Flush = fun () -> ()
        PollEvent = fun _ ->
          match eventQueue.Count > 0 with
          | true -> Some(eventQueue.Dequeue())
          | false -> None
        EnterRawMode = fun () -> ()
        LeaveRawMode = fun () -> ()
        Profile = SafeProfile.minimum width height }
    backend, fun () -> output.ToString()
