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
  | Quit

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
  /// Quit the application.
  let quit = Quit

  let rec map (f: 'a -> 'b) (cmd: Cmd<'a>) : Cmd<'b> =
    match cmd with
    | NoCmd -> NoCmd
    | Batch cmds -> Batch(List.map (map f) cmds)
    | OfAsync run -> OfAsync(fun dispatch -> run (f >> dispatch))
    | OfCancellableAsync(id, run) ->
      OfCancellableAsync(id, fun ct dispatch -> run ct (f >> dispatch))
    | CancelSub id -> CancelSub id
    | Delay(ms, msg) -> Delay(ms, f msg)
    | Quit -> Quit

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

/// The Elm Architecture program definition. Init/Update/View/Subscribe.
type Program<'model, 'msg> = {
  Init: unit -> 'model * Cmd<'msg>
  Update: 'msg -> 'model -> 'model * Cmd<'msg>
  View: 'model -> Element
  Subscribe: 'model -> Sub<'msg> list
}

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
