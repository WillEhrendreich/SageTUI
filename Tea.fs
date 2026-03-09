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
  /// Write a raw ANSI/escape sequence through the terminal backend.
  /// Written before the next frame flush. Use only for valid ANSI sequences
  /// (e.g., OSC 52 clipboard write). Arbitrary text will corrupt the display.
  | TerminalOutput of string

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
    Batch [ TerminalOutput(Ansi.osc52Copy text); Delay(0, written) ]

  /// Write a raw ANSI/escape sequence through the terminal backend.
  /// Written before the next frame flush. Only use for valid ANSI sequences —
  /// arbitrary text will corrupt the display.
  let terminalWrite (sequence: string) : Cmd<'msg> = TerminalOutput sequence

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
    | TerminalOutput s -> TerminalOutput s

  /// Dispatch a message immediately (synchronous, no delay).
  let ofMsg (msg: 'msg) : Cmd<'msg> = Delay(0, msg)

  /// Run a Task and dispatch the result as a message.
  /// If the task throws, the exception is logged to stderr and ignored — the app
  /// continues running. For controlled error handling use <see cref="ofTaskResult"/>.
  [<System.Obsolete("Prefer Cmd.ofTaskResult for controlled error handling. Cmd.ofTask silently swallows exceptions to stderr.")>]
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
    | NoCmd | OfAsync _ | OfCancellableAsync _ | CancelSub _ | Quit _ | TerminalOutput _ -> []
    | Batch cmds -> cmds |> List.collect toMessages
    | Delay(_, msg) -> [ msg ]

  // --- Cmd inspection utilities (useful in tests) ---

  /// Returns true if the Cmd tree contains a Quit command.
  let rec isQuit (cmd: Cmd<'msg>) : bool =
    match cmd with
    | Quit _ -> true
    | Batch cmds -> List.exists isQuit cmds
    | _ -> false

  /// Returns the exit code of the first Quit command found, or None.
  let rec exitCode (cmd: Cmd<'msg>) : int option =
    match cmd with
    | Quit code -> Some code
    | Batch cmds -> cmds |> List.tryPick exitCode
    | _ -> None

  /// Returns true if the Cmd tree contains any async operations (OfAsync or OfCancellableAsync).
  /// Useful for asserting that an Update call issued an async side-effect without executing it.
  let rec hasAsync (cmd: Cmd<'msg>) : bool =
    match cmd with
    | OfAsync _ | OfCancellableAsync _ -> true
    | Batch cmds -> List.exists hasAsync cmds
    | _ -> false

  /// Extract all (milliseconds, message) pairs from Delay commands in a Cmd tree.
  let rec extractDelays (cmd: Cmd<'msg>) : (int * 'msg) list =
    match cmd with
    | Delay(ms, msg) -> [ (ms, msg) ]
    | Batch cmds -> cmds |> List.collect extractDelays
    | _ -> []

/// Per-frame render timing measurements. All values are in milliseconds.
/// Available via FrameTimingsSub — subscribe to receive one record per frame.
type FrameTimings = {
  /// Time spent in ArenaRender.renderRoot (layout + draw into back buffer). Milliseconds.
  RenderMs: float
  /// Time spent in Buffer.diff (SIMD chunk comparison). Milliseconds.
  DiffMs: float
  /// Time spent in Presenter.present + backend.Write + Flush. Milliseconds.
  PresentMs: float
  /// Total frame time (render + diff + present). Milliseconds.
  TotalMs: float
  /// Number of cells that changed this frame (0 = no update written to terminal).
  ChangedCells: int
}

module FrameTimings =
  let empty = { RenderMs = 0.0; DiffMs = 0.0; PresentMs = 0.0; TotalMs = 0.0; ChangedCells = 0 }

/// A subscription that connects external events to messages.
type Sub<'msg> =
  /// Fires for every key press. The callback receives the key and any active modifiers.
  | KeySub of (Key * Modifiers -> 'msg option)
  /// Fires for mouse press and release events only (Phase = Pressed or Released).
  /// Does NOT fire for drag/motion events — use DragSub for those.
  /// When button-event tracking (?1002h) is enabled in a future release, Motion events
  /// will arrive via DragSub, not MouseSub.
  | MouseSub of (MouseEvent -> 'msg option)
  /// Fires for mouse press events only (Phase = Pressed) at a Keyed element.
  /// The second argument is the key of the innermost Keyed element at the cursor, or None.
  /// Does NOT fire for mouse releases or drag/motion events.
  | ClickSub of (MouseEvent * string option -> 'msg option)
  /// Fires for mouse drag/motion events (Phase = Motion) when button-event tracking
  /// (?1002h) is enabled. Does NOT fire for press or release events.
  /// Re-enable ?1002h in Detect.fs once your app registers a DragSub.
  | DragSub of (MouseEvent -> 'msg option)
  /// Fires when the terminal window gains or loses OS focus (?1004h must be enabled).
  /// The bool argument is true for focus-gained, false for focus-lost.
  | TerminalFocusSub of (bool -> 'msg option)
  /// Fires when the terminal sends bracketed-paste content (?2004h must be enabled).
  /// The string argument contains the full pasted text.
  | PasteSub of (string -> 'msg option)
  | FocusSub of (FocusDirection -> 'msg option)
  | TimerSub of id: string * interval: TimeSpan * tick: (unit -> 'msg)
  | ResizeSub of (int * int -> 'msg)
  | CustomSub of id: string * start: (('msg -> unit) -> CancellationToken -> Async<unit>)
  /// Delivers a FrameTimings record to the given message after each rendered frame.
  | FrameTimingsSub of (FrameTimings -> 'msg)

and FocusDirection =
  | FocusNext
  | FocusPrev

/// Key binding helpers for zero-ceremony keyboard subscriptions.
module Keys =
  /// Create a KeySub from a list of key-to-message bindings.
  /// Unmatched keys are ignored.
  ///
  /// IMPORTANT — call at module/let level, NOT inside the Subscribe lambda:
  ///   let keyBindings = Keys.bind [ Key.Char (Text.Rune 'q'), Quit ]           // ✅ allocated once
  ///   Subscribe = fun _ -> [ keyBindings ]
  ///
  /// Calling Keys.bind inside Subscribe allocates a new Dictionary on every model update:
  ///   Subscribe = fun _ -> [ Keys.bind [ Key.Char (Text.Rune 'q'), Quit ] ]    // ⚠️ allocs every update
  let bind (bindings: (Key * 'msg) list) : Sub<'msg> =
    let lookup = System.Collections.Generic.Dictionary(bindings.Length)
    for (k, msg) in bindings do
      lookup[k] <- msg
    KeySub(fun (k, _mods) ->
      match lookup.TryGetValue(k) with
      | true, msg -> Some msg
      | false, _ -> None)

  /// Create a KeySub from key+modifier bindings. Uses O(1) Dictionary lookup.
  ///
  /// IMPORTANT — call at module/let level, NOT inside the Subscribe lambda:
  ///   let keyBindings = Keys.bindWithMods [ (Key.Char (Text.Rune 's'), Modifiers.Ctrl), Save ]  // ✅ allocated once
  ///   Subscribe = fun _ -> [ keyBindings ]
  ///
  /// Calling Keys.bindWithMods inside Subscribe allocates a new Dictionary on every model update:
  ///   Subscribe = fun _ -> [ Keys.bindWithMods [...] ]   // ⚠️ allocs every update
  let bindWithMods (bindings: ((Key * Modifiers) * 'msg) list) : Sub<'msg> =
    let lookup = System.Collections.Generic.Dictionary(bindings.Length)
    for ((k, m), msg) in bindings do
      lookup[(k, m)] <- msg
    KeySub(fun (k, mods) ->
      match lookup.TryGetValue((k, mods)) with
      | true, msg -> Some msg
      | false, _ -> None)

/// Subscription combinators.
module Sub =
  /// Transform the message type of a subscription.
  let map (f: 'a -> 'b) (sub: Sub<'a>) : Sub<'b> =
    match sub with
    | KeySub handler -> KeySub(fun input -> handler input |> Option.map f)
    | MouseSub handler -> MouseSub(fun input -> handler input |> Option.map f)
    | ClickSub handler -> ClickSub(fun input -> handler input |> Option.map f)
    | DragSub handler -> DragSub(fun input -> handler input |> Option.map f)
    | TerminalFocusSub handler -> TerminalFocusSub(fun gained -> handler gained |> Option.map f)
    | PasteSub handler -> PasteSub(fun text -> handler text |> Option.map f)
    | FocusSub handler -> FocusSub(fun input -> handler input |> Option.map f)
    | TimerSub(id, interval, tick) -> TimerSub(id, interval, tick >> f)
    | ResizeSub handler -> ResizeSub(fun input -> handler input |> f)
    | CustomSub(id, start) ->
      CustomSub(id, fun dispatch ct -> start (f >> dispatch) ct)
    | FrameTimingsSub toMsg -> FrameTimingsSub(toMsg >> f)

  /// Subscribe to per-frame render timing measurements.
  /// Dispatches a FrameTimings message after each frame that produced visible output.
  /// Use to build perf overlays, frame rate monitors, or adaptive quality controls.
  ///
  /// Example:
  ///   Sub.frameTimings GotTimings
  let frameTimings (toMsg: FrameTimings -> 'msg) : Sub<'msg> = FrameTimingsSub toMsg

  /// Subscribe to an `IObservable<'msg>`. Dispatches each emitted value as a message.
  /// The subscription is automatically cancelled when the app leaves the state that
  /// returns this sub. Disposal is handled by the runtime via CancellationToken.
  ///
  /// Example:
  ///   `Sub.fromObservable "prices" priceStream`
  let fromObservable (id: string) (observable: System.IObservable<'msg>) : Sub<'msg> =
    CustomSub(id, fun dispatch ct ->
      async {
        use _ = observable.Subscribe(fun msg ->
          match ct.IsCancellationRequested with
          | false -> dispatch msg
          | true -> ())
        do! Async.AwaitWaitHandle(ct.WaitHandle) |> Async.Ignore
      })

  /// Subscribe to an `IAsyncEnumerable<'msg>`. Iterates the stream and dispatches each value.
  /// The subscription stops when the sequence completes or the token is cancelled.
  ///
  /// Example:
  ///   `Sub.fromStream "updates" myAsyncEnumerable`
  let fromStream (id: string) (stream: System.Collections.Generic.IAsyncEnumerable<'msg>) : Sub<'msg> =
    CustomSub(id, fun dispatch ct ->
      async {
        let enumerator = stream.GetAsyncEnumerator(ct)
        try
          let mutable running = true
          while running && not ct.IsCancellationRequested do
            let! hasMore = enumerator.MoveNextAsync().AsTask() |> Async.AwaitTask
            match hasMore && not ct.IsCancellationRequested with
            | true -> dispatch enumerator.Current
            | false -> running <- false
        finally
          enumerator.DisposeAsync().AsTask() |> Async.AwaitTask |> Async.RunSynchronously
      })

/// Generic undo/redo wrapper for any model type.
/// Wrap your model in `UndoableModel` and use `Undoable.commit` in Update
/// to get Ctrl+Z / Ctrl+Y for free.
///
/// Example:
///   type Model = UndoableModel<Counter>
///   let update msg model =
///     match msg with
///     | Increment -> Undoable.commit { model.Present with Count = model.Present.Count + 1 } model, NoCmd
///     | Undo      -> Undoable.undo model, NoCmd
type UndoableModel<'model> = {
  /// Previous states (head = most recent past state).
  Past: 'model list
  /// The current visible state.
  Present: 'model
  /// States that have been undone (head = next redo target).
  Future: 'model list
}

/// Operations on UndoableModel.
module Undoable =
  /// Initialise an undo stack with an initial state. Past and Future are empty.
  let init (model: 'model) : UndoableModel<'model> =
    { Past = []; Present = model; Future = [] }

  /// Returns true when there is at least one state to undo.
  let canUndo (m: UndoableModel<'model>) = not (List.isEmpty m.Past)

  /// Returns true when there is at least one state to redo.
  let canRedo (m: UndoableModel<'model>) = not (List.isEmpty m.Future)

  /// Undo the last committed change. No-op when Past is empty.
  let undo (m: UndoableModel<'model>) : UndoableModel<'model> =
    match m.Past with
    | [] -> m
    | p :: rest -> { Past = rest; Present = p; Future = m.Present :: m.Future }

  /// Redo the next change. No-op when Future is empty.
  let redo (m: UndoableModel<'model>) : UndoableModel<'model> =
    match m.Future with
    | [] -> m
    | f :: rest -> { Past = m.Present :: m.Past; Present = f; Future = rest }

  /// Commit a new present state. Pushes current Present to Past; clears Future.
  let commit (newPresent: 'model) (m: UndoableModel<'model>) : UndoableModel<'model> =
    { Past = m.Present :: m.Past; Present = newPresent; Future = [] }

  /// Commit only if `newPresent` differs from current Present (structural equality).
  let commitIfChanged (newPresent: 'model) (m: UndoableModel<'model>) : UndoableModel<'model> when 'model : equality =
    match m.Present = newPresent with
    | true -> m
    | false -> commit newPresent m

  /// Truncate the undo history to at most `maxDepth` entries. Oldest entries are discarded.
  /// O(maxDepth) — does not traverse the full history list.
  let truncate (maxDepth: int) (m: UndoableModel<'model>) : UndoableModel<'model> =
    { m with Past = m.Past |> List.truncate maxDepth }

/// The Elm Architecture program definition. Init/Update/View/Subscribe.
type Program<'model, 'msg> = {
  Init: unit -> 'model * Cmd<'msg>
  Update: 'msg -> 'model -> 'model * Cmd<'msg>
  View: 'model -> Element
  Subscribe: 'model -> Sub<'msg> list
}

/// Represents a child component's program lifted into a parent program's type space.
/// Produced by `Program.map`. Having a named type (rather than an anonymous record) enables
/// nominal type checking and allows callers to write helper functions against this type.
type MappedProgram<'parentModel, 'parentMsg, 'childMsg> = {
  /// Initialize the child component within a parent model.
  Init: 'parentModel -> 'parentModel * Cmd<'parentMsg>
  /// Update the parent model using a child message.
  Update: 'childMsg -> 'parentModel -> 'parentModel * Cmd<'parentMsg>
  /// Render the child component from the parent model.
  View: 'parentModel -> Element
  /// Subscribe the child component using the parent model.
  Subscribe: 'parentModel -> Sub<'parentMsg> list
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
    : MappedProgram<'parentModel, 'parentMsg, 'childMsg> =
    { Init = fun parentModel ->
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
        |> List.map (Sub.map toMsg) }

  /// Run a list of messages through a program's Update function and return all
  /// intermediate (model, cmd) states. The initial model comes from Init.
  /// Useful for unit-testing update logic without spinning up a terminal.
  ///
  /// Simulate running a list of messages through a program's Update function.
  /// Returns one `(model, cmd)` pair per message, in order.
  /// The first element is the state **after the first message** — the initial
  /// `(initModel, initCmd)` from `program.Init()` is not included.
  /// Use `List.last` to get the final model after all messages.
  ///
  /// Example:
  /// ```fsharp
  /// let states = Program.simulate [Increment; Increment; Decrement] myProgram
  /// let finalModel = states |> List.last |> fst
  /// // states.[0] is the state after Increment (not the initial state)
  /// ```
  let simulate (msgs: 'msg list) (program: Program<'model, 'msg>) : ('model * Cmd<'msg>) list =
    let initModel, initCmd = program.Init()
    msgs
    |> List.scan (fun (m, _) msg -> program.Update msg m) (initModel, initCmd)
    |> List.tail

/// A vocabulary type for asynchronous data loading states.
/// The most commonly reinvented type in F# async applications — provided here so
/// every user doesn't have to define their own `type LoadState`.
type RemoteData<'a> =
  /// No request has been made yet.
  | Idle
  /// A request is in flight.
  | Loading
  /// Data loaded successfully.
  | Loaded of 'a
  /// The request failed with an exception.
  | Failed of exn

/// Helpers for working with RemoteData values.
module RemoteData =
  /// Map a function over a Loaded value. Idle, Loading, and Failed pass through unchanged.
  let map (f: 'a -> 'b) (rd: RemoteData<'a>) : RemoteData<'b> =
    match rd with
    | Idle -> Idle
    | Loading -> Loading
    | Loaded a -> Loaded(f a)
    | Failed ex -> Failed ex

  /// Return the loaded value if present, otherwise a default.
  let defaultValue (def: 'a) (rd: RemoteData<'a>) : 'a =
    match rd with
    | Loaded a -> a
    | _ -> def

  /// Return true only when the data is fully loaded.
  let isLoaded (rd: RemoteData<'a>) : bool =
    match rd with
    | Loaded _ -> true
    | _ -> false

  /// Return true when a request is in flight.
  let isLoading (rd: RemoteData<'a>) : bool =
    match rd with
    | Loading -> true
    | _ -> false

  /// Monadic bind — chain operations on loaded data.
  let bind (f: 'a -> RemoteData<'b>) (rd: RemoteData<'a>) : RemoteData<'b> =
    match rd with
    | Idle -> Idle
    | Loading -> Loading
    | Loaded a -> f a
    | Failed ex -> Failed ex

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
