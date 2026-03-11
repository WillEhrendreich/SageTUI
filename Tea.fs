namespace SageTUI

open System
open System.ComponentModel
open System.Threading

/// A command to be executed by the runtime. Commands produce messages asynchronously.
type Cmd<'msg> =
  | NoCmd
  | Batch of Cmd<'msg> list
  | [<EditorBrowsable(EditorBrowsableState.Never)>] OfAsync of (('msg -> unit) -> Async<unit>)
  | [<EditorBrowsable(EditorBrowsableState.Never)>] OfCancellableAsync of id: string * (CancellationToken -> ('msg -> unit) -> Async<unit>)
  /// Use Cmd.cancel to cancel a running async by id.
  | [<EditorBrowsable(EditorBrowsableState.Never)>] CancelSub of string
  | Delay of milliseconds: int * 'msg
  /// Dispatch a message synchronously — processed in the same drain loop before the next render.
  /// Unlike Delay(0, msg), DirectMsg does not schedule an async task; it enqueues directly.
  | [<EditorBrowsable(EditorBrowsableState.Never)>] DirectMsg of 'msg
  /// Quit the application with the given exit code (0 = success).
  | Quit of exitCode: int
  /// Write a raw ANSI/escape sequence through the terminal backend.
  /// Written before the next frame flush. Use only for valid ANSI sequences
  /// (e.g., OSC 52 clipboard write). Arbitrary text will corrupt the display.
  | [<EditorBrowsable(EditorBrowsableState.Never)>] TerminalOutput of string

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
    | DirectMsg msg -> DirectMsg(f msg)
    | Quit code -> Quit code
    | TerminalOutput s -> TerminalOutput s

  // Execute a Cmd<'msg> with a given dispatch function (used by bind/andThen).
  let rec internal runCmdAsync (dispatch: 'msg -> unit) (cmd: Cmd<'msg>) : Async<unit> =
    match cmd with
    | NoCmd -> async.Return()
    | DirectMsg msg -> async { do dispatch msg }
    | OfAsync run -> run dispatch
    | Batch cmds -> async { for c in cmds do do! runCmdAsync dispatch c }
    | Delay(ms, msg) ->
      async {
        do! Async.Sleep ms
        do dispatch msg
      }
    | OfCancellableAsync(_, run) -> run CancellationToken.None dispatch
    | CancelSub _ | Quit _ | TerminalOutput _ -> async.Return()

  /// Monadic bind: when `cmd` dispatches an 'a, apply `f` to produce a Cmd<'b>,
  /// then run that command. The intermediate 'a is NOT dispatched to Update —
  /// this is for private async pipelines.
  /// For Batch cmds: bind applies to each sub-command independently.
  let bind (f: 'a -> Cmd<'b>) (cmd: Cmd<'a>) : Cmd<'b> =
    let intercept (dispatch: 'b -> unit) (msg: 'a) =
      runCmdAsync dispatch (f msg) |> Async.RunSynchronously
    let rec runA (dispatch: 'b -> unit) (c: Cmd<'a>) : Async<unit> =
      match c with
      | NoCmd -> async.Return()
      | DirectMsg msg -> async { do intercept dispatch msg }
      | OfAsync run -> async { do! run (intercept dispatch) }
      | Batch cmds -> async { for c in cmds do do! runA dispatch c }
      | Delay(ms, msg) ->
        async {
          do! Async.Sleep ms
          do intercept dispatch msg
        }
      | OfCancellableAsync(_, run) ->
        async { do! run CancellationToken.None (intercept dispatch) }
      | CancelSub _ | Quit _ | TerminalOutput _ -> async.Return()
    match cmd with
    | NoCmd -> NoCmd
    | DirectMsg msg -> f msg
    | CancelSub id -> CancelSub id
    | Quit code -> Quit code
    | TerminalOutput s -> TerminalOutput s
    | _ -> OfAsync(fun dispatch -> runA dispatch cmd)

  /// Sequential execution: run `cmd1` first (dispatching its messages), then `cmd2`.
  /// Both commands dispatch messages through the normal Update/View cycle.
  ///
  ///   Cmd.andThen cmd2 cmd1 = run cmd1, then run cmd2
  let andThen (cmd2: Cmd<'msg>) (cmd1: Cmd<'msg>) : Cmd<'msg> =
    match cmd1, cmd2 with
    | NoCmd, _ -> cmd2
    | _, NoCmd -> cmd1
    | _ ->
      OfAsync(fun dispatch ->
        async {
          do! runCmdAsync dispatch cmd1
          do! runCmdAsync dispatch cmd2
        })

  /// Run all commands in order, each dispatching its messages.
  /// Empty list → NoCmd. Single item → that item. Multiple → sequential via andThen.
  let sequence (cmds: Cmd<'msg> list) : Cmd<'msg> =
    match cmds with
    | [] -> NoCmd
    | [single] -> single
    | _ -> cmds |> List.reduce (fun acc cmd -> andThen cmd acc)

  /// Dispatch a message synchronously before the next render frame.
  ///
  /// Unlike Cmd.nextFrame, this does not schedule an async task — the message is
  /// placed directly in the drain queue and processed in the same drain pass before
  /// any render occurs. Chain multiple DirectMsg commands via Cmd.batch for
  /// ordered synchronous message sequences.
  ///
  ///   model, Cmd.batch [ Cmd.ofMsg Loaded; Cmd.ofMsg (SetTitle "foo") ]
  ///   // Both messages processed before the next frame — no intermediate renders.
  let ofMsg (msg: 'msg) : Cmd<'msg> = DirectMsg msg

  /// Dispatch a message in the next event loop iteration (one frame later).
  ///
  /// Equivalent to the old `Cmd.ofMsg` semantics: schedules a 0ms async task.
  /// Prefer `Cmd.ofMsg` for immediate dispatch; use `Cmd.nextFrame` only when you
  /// explicitly want the message deferred until the next iteration.
  let nextFrame (msg: 'msg) : Cmd<'msg> = Delay(0, msg)

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

  /// Run an `Async<Result<'ok,'err>>` and dispatch the appropriate message.
  /// On `Ok value`, dispatches `onOk value`. On `Error err`, dispatches `onError err`.
  /// Unlike `Cmd.ofAsync`, errors embedded in the Result are surfaced as messages —
  /// nothing is silently swallowed. The error type `'err` can be any type: exn, DU, string, etc.
  /// Outer async exceptions (if the computation throws instead of returning Error) propagate
  /// to the runtime — use `ofAsyncResultSafe` or `ofAsyncResultWith` if the computation
  /// may throw.
  ///
  /// Backward compatible: existing callers using `Async<Result<'a,exn>>` continue to work
  /// unchanged — `'err` simply infers as `exn`.
  ///
  /// Example:
  ///   let loadFile path = async {
  ///     try return Ok (File.ReadAllText path)
  ///     with ex -> return Error ex }
  ///   Cmd.ofAsyncResult (loadFile "data.txt") (fun text -> Loaded text) (fun ex -> LoadFailed ex.Message)
  let ofAsyncResult
    (computation: Async<Result<'ok,'err>>)
    (onOk: 'ok -> 'msg)
    (onError: 'err -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        let! result = computation
        match result with
        | Ok value -> dispatch (onOk value)
        | Error err -> dispatch (onError err)
      })

  /// Run an `Async<Result<'a, exn>>`, catching any exceptions thrown in the async body.
  /// On `Ok value`, dispatches `onOk value`. On `Error exn` or an exception escaping the
  /// async body, dispatches `onError exn`.
  ///
  /// Use this in preference to `ofAsyncResult` when the async computation may throw
  /// instead of returning `Error`. Unlike `ofAsyncResult`, no exception can escape
  /// to the .NET async runtime — all failures are dispatched as messages.
  ///
  /// Note: the error type is constrained to `exn` because both a `Result Error` and
  /// a caught outer exception share the same type. For typed domain errors + outer exception
  /// handling, use `ofAsyncResultWith`.
  let ofAsyncResultSafe
    (computation: Async<Result<'a, exn>>)
    (onOk: 'a -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = computation
          match result with
          | Ok value -> dispatch (onOk value)
          | Error ex -> dispatch (onError ex)
        with ex ->
          dispatch (onError ex)
      })

  /// Run an `Async<Result<'ok,'err>>` with typed domain errors AND a separate outer exception handler.
  /// On `Ok value`, dispatches `onOk value`.
  /// On `Error err`, dispatches `onError err`.
  /// If the async computation throws (instead of returning Error), dispatches `onException ex`.
  ///
  /// Use this when domain errors are a DU but the computation may also throw.
  let ofAsyncResultWith
    (computation: Async<Result<'ok,'err>>)
    (onOk: 'ok -> 'msg)
    (onError: 'err -> 'msg)
    (onException: exn -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = computation
          match result with
          | Ok value -> dispatch (onOk value)
          | Error err -> dispatch (onError err)
        with ex ->
          dispatch (onException ex)
      })

  /// Run a `Task<Result<'ok,'err>>` and dispatch the appropriate message.
  /// For C# interop where async methods return `Task<Result<_,_>>`.
  /// On `Ok value`, dispatches `onOk value`. On `Error err`, dispatches `onError err`.
  /// Outer task exceptions propagate to the runtime.
  let ofTaskFromResult
    (task: unit -> System.Threading.Tasks.Task<Result<'ok,'err>>)
    (onOk: 'ok -> 'msg)
    (onError: 'err -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        let! result = task() |> Async.AwaitTask
        match result with
        | Ok value -> dispatch (onOk value)
        | Error err -> dispatch (onError err)
      })

  /// Extract all synchronously-dispatchable messages from a Cmd tree.
  /// Returns messages from every Delay and DirectMsg case.
  /// Async operations and subscriptions are not executed.
  /// Useful for asserting what messages a command will eventually produce in unit tests.
  let rec toMessages (cmd: Cmd<'msg>) : 'msg list =
    match cmd with
    | NoCmd | OfAsync _ | OfCancellableAsync _ | CancelSub _ | Quit _ | TerminalOutput _ -> []
    | Batch cmds -> cmds |> List.collect toMessages
    | Delay(_, msg) -> [ msg ]
    | DirectMsg msg -> [ msg ]

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

  /// Debounce a message: wait `delayMs` milliseconds, then dispatch `msg`.
  /// If a new `Cmd.debounce` with the **same id** is issued before the delay
  /// completes, the previous pending dispatch is cancelled and the timer restarts.
  ///
  /// This is powered by `OfCancellableAsync` — App.run cancels any running
  /// async with the same id before starting the new one, giving cancel-and-restart
  /// semantics for free.
  ///
  /// Typical use: debounce a search filter on fast keystroke input.
  ///   | SearchChanged q ->
  ///       { model with Query = q }, Cmd.debounce "search" 300 (Search q)
  let debounce (id: string) (delayMs: int) (msg: 'msg) : Cmd<'msg> =
    OfCancellableAsync(id, fun ct dispatch -> async {
      do! System.Threading.Tasks.Task.Delay(delayMs, ct) |> Async.AwaitTask
      dispatch msg })

  // Module-level cache: id → (boxed input, boxed output)
  let private computeCache =
    System.Collections.Concurrent.ConcurrentDictionary<string, struct(obj * obj)>()

  /// Derive a value from `input` and dispatch `toMsg result` as a `Cmd`.
  ///
  /// When called with the **same `id` and structurally-equal `input`**, the cached
  /// result is dispatched immediately without re-running `transform` (no async hop).
  /// When `input` changes, the transformation is re-run on the thread pool via
  /// `OfCancellableAsync`, so a new call preempts any previous in-flight computation
  /// sharing the same `id`.
  ///
  /// Rules:
  ///   • `id` must be globally unique across your program (use a module-qualified string).
  ///   • `transform` should be pure and free of side effects (it may run on a thread-pool thread).
  ///   • `'input` must support structural equality (`when 'input : equality`).
  ///
  /// Example:
  ///   | FilterChanged text ->
  ///       model, Cmd.computeWhen "search.filter" text filterItems SearchResultsReady
  let computeWhen
    (id: string)
    (input: 'input)
    (transform: 'input -> 'output)
    (toMsg: 'output -> 'msg)
    : Cmd<'msg>
    when 'input : equality =
    match computeCache.TryGetValue(id) with
    | true, struct(cachedInput, cachedOutput) when (cachedInput :?> 'input) = input ->
      // Cache hit — enqueue directly, no async allocation needed
      let result = cachedOutput :?> 'output
      DirectMsg(toMsg result)
    | _ ->
      OfCancellableAsync(id, fun _ct dispatch -> async {
        let result = transform input
        computeCache.[id] <- struct(box input, box result)
        dispatch (toMsg result) })

  /// Clear the `computeWhen` process-level cache.
  ///
  /// In production code this is never needed — the cache is intentionally
  /// process-scoped. In tests, call this in test setup/teardown to prevent one
  /// test's cached result from silently short-circuiting a different test that
  /// shares the same id.
  ///
  /// Example (Expecto):
  ///   testCase "fresh computation" <| fun () ->
  ///     Cmd.clearComputeCache ()
  ///     let cmd = Cmd.computeWhen "my-id" input transform toMsg
  ///     ...
  let clearComputeCache () = computeCache.Clear()

  /// Race a command against a deadline.If the command dispatches before `timeout`
  /// elapses, that message goes through normally. If the timeout fires first,
  /// `onTimeout` is dispatched instead. Exactly one message is dispatched.
  ///
  /// Works on `OfAsync` and `OfCancellableAsync` commands. Other command types
  /// (NoCmd, Delay, Quit, etc.) pass through unchanged.
  ///
  /// Example:
  ///   Cmd.withTimeout (TimeSpan.FromSeconds 10.0) (LoadFailed "Request timed out") fetchCmd
  let rec withTimeout (timeout: System.TimeSpan) (onTimeout: 'msg) (cmd: Cmd<'msg>) : Cmd<'msg> =
    match cmd with
    | OfAsync run ->
      OfAsync(fun dispatch ->
        async {
          let mutable fired = 0
          let once (msg: 'msg) =
            if System.Threading.Interlocked.CompareExchange(&fired, 1, 0) = 0 then
              dispatch msg
          let workTask : System.Threading.Tasks.Task =
            Async.StartAsTask(run once) :> System.Threading.Tasks.Task
          let timeoutTask : System.Threading.Tasks.Task =
            System.Threading.Tasks.Task.Delay(timeout)
              .ContinueWith(System.Action<System.Threading.Tasks.Task>(fun _ -> once onTimeout))
            :> System.Threading.Tasks.Task
          do! System.Threading.Tasks.Task.WhenAny(workTask, timeoutTask)
              |> Async.AwaitTask
              |> Async.Ignore
        })
    | OfCancellableAsync(id, run) ->
      OfCancellableAsync(id, fun ct dispatch ->
        async {
          use innerCts = System.Threading.CancellationTokenSource.CreateLinkedTokenSource(ct)
          let mutable fired = 0
          let once (msg: 'msg) =
            if System.Threading.Interlocked.CompareExchange(&fired, 1, 0) = 0 then
              innerCts.Cancel()
              dispatch msg
          let workAsync = async {
            try do! run innerCts.Token once
            with :? System.OperationCanceledException -> () }
          let workTask : System.Threading.Tasks.Task =
            Async.StartAsTask(workAsync, cancellationToken = ct) :> System.Threading.Tasks.Task
          let timeoutTask : System.Threading.Tasks.Task =
            System.Threading.Tasks.Task.Delay(timeout, ct)
              .ContinueWith(System.Action<System.Threading.Tasks.Task>(fun _ ->
                if not ct.IsCancellationRequested then once onTimeout))
            :> System.Threading.Tasks.Task
          do! System.Threading.Tasks.Task.WhenAny(workTask, timeoutTask)
              |> Async.AwaitTask
              |> Async.Ignore
        })
    | Batch cmds -> Batch(List.map (withTimeout timeout onTimeout) cmds)
    | other -> other

  /// Run an `Async<'a>` computation and dispatch the result through a `Result<'a, exn>` mapper.
  /// If the async throws, the exception is wrapped in `Error` and passed to `toMsg`.
  /// This gives a single-callback API: the caller decides what both success and failure produce.
  ///
  /// Example:
  ///   Cmd.fromAsync (loadData url) (function Ok data -> DataLoaded data | Error ex -> LoadFailed ex.Message)
  let fromAsync (computation: Async<'a>) (toMsg: Result<'a, exn> -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = computation
          dispatch (toMsg (Ok result))
        with ex ->
          dispatch (toMsg (Error ex))
      })

  /// Run a `unit -> Task<'a>` function and dispatch the result through a `Result<'a, exn>` mapper.
  /// If the task throws, the exception is wrapped in `Error` and passed to `toMsg`.
  ///
  /// Example:
  ///   Cmd.fromTask (fun () -> httpClient.GetStringAsync url) (function Ok s -> GotResponse s | Error e -> RequestFailed e.Message)
  let fromTask (task: unit -> Threading.Tasks.Task<'a>) (toMsg: Result<'a, exn> -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let! result = task() |> Async.AwaitTask
          dispatch (toMsg (Ok result))
        with ex ->
          dispatch (toMsg (Error ex))
      })

  /// Write a raw ANSI/escape sequence through the terminal backend.
  /// Alias for `Cmd.terminalWrite` — prefer this name for clarity at call sites.
  /// Written before the next frame flush. Only use for valid ANSI sequences —
  /// arbitrary text will corrupt the display.
  let unsafeTerminalWrite (sequence: string) : Cmd<'msg> = TerminalOutput sequence


  /// - Windows:  `%APPDATA%\{appName}`
  /// - macOS:    `~/Library/Application Support/{appName}`
  /// - Linux:    `$XDG_DATA_HOME/{appName}` or `~/.local/share/{appName}`
  let appDataDir (appName: string) : string =
    let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    if Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows) then
      IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), appName)
    elif Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.OSX) then
      IO.Path.Combine(home, "Library", "Application Support", appName)
    else
      let xdg = Environment.GetEnvironmentVariable("XDG_DATA_HOME")
      let base' =
        match String.IsNullOrEmpty(xdg) with
        | false -> xdg
        | true  -> IO.Path.Combine(home, ".local", "share")
      IO.Path.Combine(base', appName)

  /// Persist raw bytes to `{appDataDir appName}/{key}.bin` asynchronously.
  /// Creates the directory if it doesn't exist.
  /// Dispatches `onDone ()` on success, `onError exn` on failure.
  let saveBytes
    (appName: string)
    (key: string)
    (data: byte array)
    (onDone: unit -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let dir = appDataDir appName
          IO.Directory.CreateDirectory(dir) |> ignore
          let path = IO.Path.Combine(dir, key + ".bin")
          let tmpPath = path + ".tmp"
          do! IO.File.WriteAllBytesAsync(tmpPath, data) |> Async.AwaitTask
          IO.File.Move(tmpPath, path, overwrite = true)
          dispatch (onDone ())
        with ex ->
          dispatch (onError ex)
      })

  /// Load raw bytes from `{appDataDir appName}/{key}.bin` asynchronously.
  /// Dispatches `onLoaded (Some bytes)` if the file exists, `onLoaded None` if it doesn't.
  /// Dispatches `onError exn` on I/O failure (other than file-not-found).
  let loadBytes
    (appName: string)
    (key: string)
    (onLoaded: byte array option -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    OfAsync(fun dispatch ->
      async {
        try
          let path = IO.Path.Combine(appDataDir appName, key + ".bin")
          match IO.File.Exists(path) with
          | false -> dispatch (onLoaded None)
          | true  ->
            let! data = IO.File.ReadAllBytesAsync(path) |> Async.AwaitTask
            dispatch (onLoaded (Some data))
        with ex ->
          dispatch (onError ex)
      })

  /// Persist a UTF-8 string to `{appDataDir appName}/{key}.bin` asynchronously.
  /// Creates the directory if it doesn't exist.
  /// Dispatches `onDone ()` on success, `onError exn` on failure.
  let saveString
    (appName: string)
    (key: string)
    (text: string)
    (onDone: unit -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    saveBytes appName key (Text.Encoding.UTF8.GetBytes(text)) onDone onError

  /// Load a UTF-8 string from `{appDataDir appName}/{key}.bin` asynchronously.
  /// Dispatches `onLoaded (Some string)` if the file exists, `onLoaded None` if it doesn't.
  /// Dispatches `onError exn` on I/O failure (other than file-not-found).
  let loadString
    (appName: string)
    (key: string)
    (onLoaded: string option -> 'msg)
    (onError: exn -> 'msg) : Cmd<'msg> =
    loadBytes appName key (Option.map (fun b -> Text.Encoding.UTF8.GetString(b)) >> onLoaded) onError

  /// Return a bound storage helper for a specific app name.
  /// Avoids repeating the `appName` argument at every call site.
  ///
  /// Example:
  /// ```fsharp
  /// let store = Cmd.Storage.bind "MyApp"
  /// store.save "prefs" settingsJson onSaved onError
  /// store.load "prefs" onLoaded onError
  /// ```
  module Storage =
    let bind (appName: string) =
      {| save = saveString appName
         load = loadString appName |}

  /// Low-level atomic file I/O helpers, path-based.
  /// Useful for tests and for users who manage their own file paths.
  module IO =
    /// Atomically write `data` to `path`. Writes to `path + ".tmp"` first,
    /// then renames to `path` with overwrite. If the process crashes during the
    /// write, the original file (if any) is preserved and no partial content is visible.
    let saveAtomicBytes (path: string) (data: byte array) : Async<unit> =
      async {
        let tmpPath = path + ".tmp"
        do! System.IO.File.WriteAllBytesAsync(tmpPath, data) |> Async.AwaitTask
        System.IO.File.Move(tmpPath, path, overwrite = true)
      }

    /// Load bytes from `path`, returning `None` if the file doesn't exist.
    let loadBytesOrNone (path: string) : Async<byte array option> =
      async {
        match System.IO.File.Exists(path) with
        | false -> return None
        | true ->
          let! data = System.IO.File.ReadAllBytesAsync(path) |> Async.AwaitTask
          return Some data
      }


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
  /// Motion events (button-event tracking via ?1002h) arrive via DragSub, not MouseSub.
  | MouseSub of (MouseEvent -> 'msg option)
  /// Fires for mouse press events only (Phase = Pressed) at a Keyed element.
  /// The second argument is the key of the innermost Keyed element at the cursor, or None.
  /// Does NOT fire for mouse releases or drag/motion events.
  | ClickSub of (MouseEvent * string option -> 'msg option)
  /// Internal: fires for mouse drag/motion events (?1002h button-event tracking is active).
  /// Use Sub.fromObservable if you need custom pointer tracking.
  | [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>] DragSub of (MouseEvent -> 'msg option)
  /// Fires when the terminal window gains or loses OS focus (?1004h must be enabled).
  /// The bool argument is true for focus-gained, false for focus-lost.
  | TerminalFocusSub of (bool -> 'msg option)
  /// Internal: fires when the terminal sends bracketed-paste content (?2004h must be enabled).
  /// The string argument contains the full pasted text.
  | [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>] PasteSub of (string -> 'msg option)
  | FocusSub of (FocusDirection -> 'msg option)
  | TimerSub of id: string * interval: TimeSpan * tick: (unit -> 'msg)
  | ResizeSub of (int * int -> 'msg option)
  | CustomSub of id: string * start: (('msg -> unit) -> CancellationToken -> Async<unit>)
  /// Delivers a FrameTimings record to the given message after each rendered frame.
  | FrameTimingsSub of (FrameTimings -> 'msg)

and FocusDirection =
  | FocusNext
  | FocusPrev

/// Key binding helpers for zero-ceremony keyboard subscriptions.
module Keys =
  /// Create a KeySub from a list of key-to-message bindings.
  /// Unmatched keys are ignored. The lookup dictionary is built lazily on first use.
  ///
  /// BEST PRACTICE — call at module/let level to share one allocation across all frames:
  ///   let keyBindings = Keys.bind [ Key.Char (Text.Rune 'q'), Quit ]           // ✅ one allocation
  ///   Subscribe = fun _ -> [ keyBindings ]
  ///
  /// Calling Keys.bind inside Subscribe creates a new Sub closure on every model update.
  /// With lazy initialization the Dictionary itself is still built only once per closure,
  /// but the closure allocation still occurs. Prefer module-level bindings for zero-alloc
  /// Subscribe lambdas on hot paths.
  let bind (bindings: (Key * 'msg) list) : Sub<'msg> =
    let lazyLookup =
      System.Lazy<System.Collections.Generic.Dictionary<Key,'msg>>(fun () ->
        let d = System.Collections.Generic.Dictionary(bindings.Length)
        for (k, msg) in bindings do d[k] <- msg
        d)
    KeySub(fun (k, _mods) ->
      match lazyLookup.Value.TryGetValue(k) with
      | true, msg -> Some msg
      | false, _ -> None)

  /// Create a KeySub from key+modifier bindings. Uses O(1) Dictionary lookup.
  /// The lookup dictionary is built lazily on first use.
  ///
  /// BEST PRACTICE — call at module/let level to share one allocation across all frames:
  ///   let keyBindings = Keys.bindWithMods [ (Key.Char (Text.Rune 's'), Modifiers.Ctrl), Save ]  // ✅ one allocation
  ///   Subscribe = fun _ -> [ keyBindings ]
  ///
  /// Calling Keys.bindWithMods inside Subscribe creates a new Sub closure on every model update.
  let bindWithMods (bindings: ((Key * Modifiers) * 'msg) list) : Sub<'msg> =
    let lazyLookup =
      System.Lazy<System.Collections.Generic.Dictionary<Key * Modifiers,'msg>>(fun () ->
        let d = System.Collections.Generic.Dictionary(bindings.Length)
        for ((k, m), msg) in bindings do d[(k, m)] <- msg
        d)
    KeySub(fun (k, mods) ->
      match lazyLookup.Value.TryGetValue((k, mods)) with
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
    | ResizeSub handler -> ResizeSub(fun input -> handler input |> Option.map f)
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

  /// Create a timer subscription that fires `tick ()` every `interval`.
  /// Convenience wrapper around `TimerSub` — avoids exposing the DU case at call sites.
  ///
  /// For namespacing in multi-component apps, use `Sub.prefix`:
  ///   Sub.interval "clock" (TimeSpan.FromSeconds 1.0) (fun () -> Tick)
  ///   |> Sub.prefix "header"
  let interval (id: string) (interval: TimeSpan) (tick: unit -> 'msg) : Sub<'msg> =
    TimerSub(id, interval, tick)

  /// Create a timer subscription that fires `tick ()` every `intervalMs` milliseconds.
  /// Convenience overload of `Sub.interval` accepting an integer millisecond count.
  ///
  /// Example:
  ///   Sub.intervalMs "frame" 16 (fun () -> Tick)  // ~60 fps
  let intervalMs (id: string) (intervalMs: int) (tick: unit -> 'msg) : Sub<'msg> =
    TimerSub(id, System.TimeSpan.FromMilliseconds(float intervalMs), tick)

  /// Prepend a namespace prefix to an identifier-bearing subscription.
  /// TimerSub and CustomSub carry string IDs used for reconciliation; two components
  /// that both register a TimerSub with id "refresh" would silently collide without
  /// namespacing. Use `Sub.prefix "componentName"` to avoid that.
  ///
  /// Subs without IDs (KeySub, MouseSub, ClickSub, DragSub, etc.) are returned unchanged.
  ///
  /// Example:
  ///   Sub.prefix "sidebar" (TimerSub("refresh", ...))
  ///   // → TimerSub("sidebar/refresh", ...)
  ///
  /// To prefix a whole list at once, use `Sub.prefixAll`.
  let prefix (ns: string) (sub: Sub<'msg>) : Sub<'msg> =
    match sub with
    | TimerSub(id, interval, tick) -> TimerSub(ns + "/" + id, interval, tick)
    | CustomSub(id, start)         -> CustomSub(ns + "/" + id, start)
    | other                        -> other

  /// Prepend a namespace prefix to all identifier-bearing subscriptions in a list.
  /// Equivalent to `List.map (Sub.prefix ns) subs`.
  ///
  /// Subs without IDs (KeySub, MouseSub, etc.) are returned unchanged.
  ///
  /// Example:
  ///   let subs = Sub.prefixAll "sidebar" [
  ///     TimerSub("refresh", TimeSpan.FromSeconds 1.0, fun () -> Tick)
  ///     CustomSub("worker", ...)
  ///   ]
  ///   // → ids become "sidebar/refresh" and "sidebar/worker"
  let prefixAll (ns: string) (subs: Sub<'msg> list) : Sub<'msg> list =
    List.map (prefix ns) subs

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
        let mutable running = true
        try
          while running && not ct.IsCancellationRequested do
            let! hasMore = enumerator.MoveNextAsync().AsTask() |> Async.AwaitTask
            match hasMore && not ct.IsCancellationRequested with
            | true -> dispatch enumerator.Current
            | false -> running <- false
        finally
          // Use IgnoreResult to avoid Async.RunSynchronously, which can deadlock
          // on .NET thread-pool schedulers that aggressively inline tasks.
          enumerator.DisposeAsync().AsTask() |> Async.AwaitTask |> Async.StartImmediate
      })

  /// Create a ClickSub from a list of element-key-to-message bindings.
  /// Symmetric with `Keys.bind` — O(1) Dictionary lookup on the element key string.
  ///
  /// IMPORTANT — call at module/let level, NOT inside the Subscribe lambda:
  ///   let clickBindings = Sub.clicks [ "submit-btn", Submit; "cancel-btn", Cancel ]  // ✅ allocated once
  ///   Subscribe = fun _ -> [ clickBindings; keyBindings ]
  ///
  /// Calling Sub.clicks inside Subscribe allocates a new Dictionary on every model update:
  ///   Subscribe = fun _ -> [ Sub.clicks [ "submit-btn", Submit ] ]    // ⚠️ allocs every update
  ///
  /// Pair with `El.clickRegion "submit-btn" submitButton` in your view.
  let clicks (bindings: (string * 'msg) list) : Sub<'msg> =
    let lookup = System.Collections.Generic.Dictionary(bindings.Length)
    for (k, msg) in bindings do
      lookup[k] <- msg
    ClickSub(fun (_, key) ->
      match key with
      | None -> None
      | Some k ->
        match lookup.TryGetValue(k) with
        | true, msg -> Some msg
        | false, _  -> None)

  /// Subscribe to bracketed-paste events from the terminal.
  ///
  /// `handler text` is called when the user pastes text (terminal sends ESC[200~ … ESC[201~).
  /// Return `Some msg` to dispatch a message, or `None` to ignore the paste.
  ///
  /// The runtime enables bracketed-paste mode (?2004h) automatically when a `PasteSub`
  /// is active. The mode is disabled again when the sub is removed.
  ///
  /// Example:
  ///   Sub.paste (fun text -> Some (TextPasted text))
  let paste (handler: string -> 'msg option) : Sub<'msg> = PasteSub handler

  /// Watch `path` for filesystem changes and dispatch `toMsg fullPath` after a quiet
  /// period of `debounceMs` milliseconds. Handles both file and directory paths.
  ///
  /// `debounceMs` is mandatory — most editors fire 2–4 events per save (atomic writes,
  /// metadata flushes). A debounce window of 100–300 ms collapses these into one message.
  /// There is no default; callers must choose explicitly.
  ///
  /// The message receives the full absolute path of the changed entry.
  /// For Renamed events, this is the new path. For Deleted events, the path no longer
  /// exists when the message is dispatched (dispatch happens after the debounce window).
  ///
  /// `id` is the reconciliation key — use `Sub.prefix` to namespace it when embedding
  /// in a component:
  ///   Sub.fileWatch "config" "/etc/myapp/config.toml" 200 FileChanged
  ///   |> Sub.prefix "app"   // id becomes "app/config"
  let fileWatch (id: string) (path: string) (debounceMs: int) (toMsg: string -> 'msg) : Sub<'msg> =
    CustomSub(id, fun dispatch ct ->
      async {
        // FileSystemWatcher requires a directory path + filter.
        // For a specific file, watch the parent directory and filter to that filename.
        let watchDir, watchFilter =
          match System.IO.Directory.Exists(path) with
          | true  -> path, "*.*"
          | false ->
            System.IO.Path.GetDirectoryName(path),
            System.IO.Path.GetFileName(path)

        // Resolve any symlinks in watchDir before creating the FileSystemWatcher.
        // On macOS, /var is a symlink to /private/var; FSEvents silently refuses to
        // watch paths that traverse unresolved symlinks, producing zero events.
        // We watch the resolved real path, but still dispatch with the original watchDir
        // so callers receive the path they originally provided.
        let resolvedWatchDir =
          try
            match (System.IO.DirectoryInfo(watchDir)).ResolveLinkTarget(true) with
            | null     -> watchDir
            | resolved -> resolved.FullName
          with _ -> watchDir

        use watcher =
          new System.IO.FileSystemWatcher(
            resolvedWatchDir, watchFilter,
            NotifyFilter =
              (System.IO.NotifyFilters.LastWrite |||
               System.IO.NotifyFilters.FileName  |||
               System.IO.NotifyFilters.Size),
            IncludeSubdirectories = false)

        // Single Timer instance reset via Timer.Change on each event — correct debounce.
        // Thread safety: pendingPath is written on the FSW event thread (thread-pool)
        // and read on the Timer callback thread (also thread-pool, different thread).
        // Volatile.Read/Write provides the memory barrier preventing stale CPU-cache reads.
        let mutable pendingPath = ""
        use debounce =
          new System.Threading.Timer(
            (fun _ ->
              let p = System.Threading.Volatile.Read(&pendingPath)
              if p <> "" && not ct.IsCancellationRequested then
                dispatch (toMsg p)),
            null,
            System.Threading.Timeout.Infinite,  // do not start until first event fires
            System.Threading.Timeout.Infinite)  // fire once; do not repeat

        // Build path from watchDir + e.Name rather than e.FullPath.
        // On macOS, FSEvents resolves symlinks in FullPath (e.g. /var → /private/var),
        // which would produce a different path than the caller supplied.
        // Reconstructing from watchDir preserves the original path representation.
        let onChanged (name: string) =
          if not ct.IsCancellationRequested then
            let p = System.IO.Path.Combine(watchDir, name)
            System.Threading.Volatile.Write(&pendingPath, p)
            // Reset the countdown; any event within debounceMs restarts the window
            debounce.Change(debounceMs, System.Threading.Timeout.Infinite) |> ignore

        // Register handlers BEFORE enabling events to avoid the race where an event
        // fires between EnableRaisingEvents=true and the first handler registration.
        watcher.Changed.Add(fun e -> onChanged e.Name)
        watcher.Created.Add(fun e -> onChanged e.Name)
        watcher.Renamed.Add(fun e -> onChanged e.Name)  // e.Name = new filename
        watcher.Deleted.Add(fun e -> onChanged e.Name)
        watcher.EnableRaisingEvents <- true

        // Block until App.fs cancels this subscription during reconciliation
        do! Async.AwaitWaitHandle(ct.WaitHandle) |> Async.Ignore
        // `use` bindings dispose watcher and debounce here in reverse declaration order
      })

  /// Rate-limit a subscription to fire at most once per `intervalMs` milliseconds.
  ///
  /// For option-returning Subs (KeySub, MouseSub, ClickSub, DragSub,
  /// TerminalFocusSub, PasteSub, FocusSub, ResizeSub): the inner handler is only
  /// called when enough time has elapsed since the last message was produced.
  /// The timer only advances when the inner handler returns `Some` — returning
  /// `None` does not consume the throttle window.
  ///
  /// TimerSub, CustomSub, and FrameTimingsSub pass through unchanged (they are
  /// already rate-limited or operate asynchronously).
  ///
  /// Example: throttle resize to at most once per 100ms
  ///   Sub.throttle 100 (ResizeSub(fun (w,h) -> Some (Resized(w,h))))
  let throttle (intervalMs: int) (sub: Sub<'msg>) : Sub<'msg> =
    let lastFiredAt = ref System.DateTime.MinValue
    let tryFire (handler: unit -> 'msg option) =
      let now = System.DateTime.UtcNow
      match (now - !lastFiredAt).TotalMilliseconds >= float intervalMs with
      | false -> None
      | true  ->
        match handler () with
        | None        -> None
        | Some m      -> lastFiredAt := now; Some m
    match sub with
    | KeySub f           -> KeySub           (fun x -> tryFire (fun () -> f x))
    | MouseSub f         -> MouseSub         (fun x -> tryFire (fun () -> f x))
    | ClickSub f         -> ClickSub         (fun x -> tryFire (fun () -> f x))
    | DragSub f          -> DragSub          (fun x -> tryFire (fun () -> f x))
    | TerminalFocusSub f -> TerminalFocusSub (fun x -> tryFire (fun () -> f x))
    | PasteSub f         -> PasteSub         (fun x -> tryFire (fun () -> f x))
    | FocusSub f         -> FocusSub         (fun x -> tryFire (fun () -> f x))
    | ResizeSub f        -> ResizeSub        (fun x -> tryFire (fun () -> f x))
    | TimerSub _ | CustomSub _ | FrameTimingsSub _ -> sub

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

/// Controls how the TEA loop responds to exceptions thrown by the `Update` function.
///
/// - `CrashOnError` — the exception propagates; the app terminates (default, explicit fail-fast).
/// - `LogAndContinue` — the exception is silently absorbed; the app keeps running.
/// - `RecoverWith handler` — `handler` is called with the exception; return `Some msg` to dispatch
///   a recovery message, or `None` to absorb silently.
type ErrorPolicy<'msg> =
  | CrashOnError
  | LogAndContinue
  | RecoverWith of (exn -> 'msg option)

/// The Elm Architecture program definition. Init/Update/View/Subscribe/OnError.
type Program<'model, 'msg> = {
  Init: unit -> 'model * Cmd<'msg>
  Update: 'msg -> 'model -> 'model * Cmd<'msg>
  View: 'model -> Element
  Subscribe: 'model -> Sub<'msg> list
  /// Error boundary policy. Controls behaviour when `Update` throws an exception.
  /// Defaults to `CrashOnError` (explicit fail-fast — no silent swallowing).
  /// Use `RecoverWith handler` to dispatch a recovery message.
  /// Use `LogAndContinue` to absorb exceptions silently.
  OnError: ErrorPolicy<'msg>
}

/// A pair of get/set functions forming a lawful lens from 'outer to 'inner.
/// Laws: (1) get (set i o) = i  (2) set (get o) o = o  (3) set i (set j o) = set i o
type Lens<'outer, 'inner> = {
  Get: 'outer -> 'inner
  Set: 'inner -> 'outer -> 'outer
}

/// A partial lens (prism) from 'outer to an optional 'inner.
/// Used for optional sub-components such as modals or conditional panels.
/// Laws: TryGet o = Some i ⟹ Set i o returns an 'outer where TryGet yields Some i.
type Prism<'outer, 'inner> = {
  TryGet: 'outer -> 'inner option
  Set: 'inner -> 'outer -> 'outer
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

/// The model type for a program wrapped with `Program.withHistory`.
/// Stores the present model plus undo/redo stacks capped at `MaxDepth`.
type HistoryModel<'model> = {
  Present:  'model
  Past:     'model list
  Future:   'model list
  MaxDepth: int
}

/// Messages for a program wrapped with `Program.withHistory`.
[<RequireQualifiedAccess>]
type HistoryMsg<'msg> =
  /// Wrap an inner message from the original program.
  | Inner of 'msg
  /// Undo the last checkpointed change. No-op when the undo stack is empty.
  | Undo
  /// Redo the last undone change. No-op when the redo stack is empty.
  | Redo
  /// Manually snapshot the current model onto the undo stack without dispatching an inner message.
  | Checkpoint
  /// Clear all undo and redo stacks, keeping only the current model.
  | ClearHistory

/// Pure helper functions for `HistoryModel<'model>`.
/// These are independent of the Program machinery and can be used directly in tests.
module History =
  /// Create a fresh `HistoryModel` wrapping `model` with empty undo/redo stacks.
  let init (maxDepth: int) (model: 'model) : HistoryModel<'model> =
    { Present = model; Past = []; Future = []; MaxDepth = maxDepth }

  /// Extract the current model from a HistoryModel.
  let present (h: HistoryModel<'model>) : 'model = h.Present

  /// True when there is at least one step to undo.
  let canUndo (h: HistoryModel<'model>) : bool =
    match h.Past with [] -> false | _ -> true

  /// True when there is at least one step to redo.
  let canRedo (h: HistoryModel<'model>) : bool =
    match h.Future with [] -> false | _ -> true

  /// Number of steps available in the undo stack.
  let undoDepth (h: HistoryModel<'model>) : int = h.Past.Length

  /// Number of steps available in the redo stack.
  let redoDepth (h: HistoryModel<'model>) : int = h.Future.Length

  /// Internal: push `newPresent` onto the history, capping Past at MaxDepth.
  /// Always clears Future (branching history discards the redo stack).
  let push (h: HistoryModel<'model>) (newPresent: 'model) : HistoryModel<'model> =
    let past = h.Present :: h.Past |> List.truncate h.MaxDepth
    { h with Present = newPresent; Past = past; Future = [] }

  /// Step backward in history (undo). No-op if Past is empty.
  let undo (h: HistoryModel<'model>) : HistoryModel<'model> =
    match h.Past with
    | []         -> h
    | prev :: rest -> { h with Present = prev; Past = rest; Future = h.Present :: h.Future }

  /// Step forward in history (redo). No-op if Future is empty.
  let redo (h: HistoryModel<'model>) : HistoryModel<'model> =
    match h.Future with
    | []         -> h
    | next :: rest -> { h with Present = next; Past = h.Present :: h.Past; Future = rest }

  /// Manually push the current model onto the undo stack without changing Present.
  /// Useful for explicit checkpoint calls when the auto-checkpoint predicate isn't sufficient.
  let checkpoint (h: HistoryModel<'model>) : HistoryModel<'model> =
    push h h.Present

// ---------------------------------------------------------------------------
// LogEvent<'model,'msg> — typed lifecycle event for Program.withLogging
// ---------------------------------------------------------------------------

/// A lifecycle event emitted by `Program.withLogging`.
///
/// `[<NoEquality; NoComparison>]` is required because 'model may contain functions.
[<NoEquality; NoComparison>]
type LogEvent<'model, 'msg> =
  /// Fired once when the program initialises, carrying the initial model.
  | AppStarted    of initialModel: 'model
  /// Fired after each `Update` call with the message, before/after models, and command count.
  | MsgDispatched of msg: 'msg * before: 'model * after: 'model * cmdCount: int
  /// Fired when a new subscription is registered (carries the subscription ID string).
  | SubAdded      of id: string
  /// Fired when a subscription is removed.
  | SubRemoved    of id: string
  /// Fired when an async command is launched (carries an optional tag).
  | AsyncStarted  of tag: string
  /// Fired when an async command completes (carries tag + elapsed milliseconds).
  | AsyncCompleted of tag: string * elapsedMs: int64
  /// Fired when the program quits with the given exit code.
  | AppStopped    of exitCode: int

/// Composable sinks for `Program.withLogging`.
module Logger =

  /// Sink that prints a human-readable summary to stdout.
  let console<'model, 'msg> (event: LogEvent<'model, 'msg>) : unit =
    match event with
    | AppStarted _              -> printfn "[SageTUI] AppStarted"
    | MsgDispatched(_, _, _, n) -> printfn "[SageTUI] MsgDispatched (%d cmd(s))" n
    | SubAdded id               -> printfn "[SageTUI] SubAdded %s" id
    | SubRemoved id             -> printfn "[SageTUI] SubRemoved %s" id
    | AsyncStarted tag          -> printfn "[SageTUI] AsyncStarted %s" tag
    | AsyncCompleted(tag, ms)   -> printfn "[SageTUI] AsyncCompleted %s (%d ms)" tag ms
    | AppStopped code           -> printfn "[SageTUI] AppStopped %d" code

  /// Sink that accumulates events into a thread-safe list.
  /// Returns `(sink, getSnapshot)` — call `getSnapshot ()` to retrieve all events so far.
  let toList<'model, 'msg> () : (LogEvent<'model, 'msg> -> unit) * (unit -> LogEvent<'model, 'msg> list) =
    let q = System.Collections.Concurrent.ConcurrentQueue<LogEvent<'model, 'msg>>()
    let sink evt = q.Enqueue evt
    let snapshot () = q |> Seq.toList
    sink, snapshot

  /// Sink that fans out a single event to multiple sinks in order.
  let combine (sinks: (LogEvent<'model, 'msg> -> unit) list) : LogEvent<'model, 'msg> -> unit =
    fun evt -> for s in sinks do s evt

  /// Sink that only passes events matching `predicate` to `inner`.
  let filter (predicate: LogEvent<'model, 'msg> -> bool) (inner: LogEvent<'model, 'msg> -> unit) : LogEvent<'model, 'msg> -> unit =
    fun evt -> match predicate evt with true -> inner evt | false -> ()

// ---------------------------------------------------------------------------
// PersistenceSpec<'model> — for Program.withPersistence
// ---------------------------------------------------------------------------

/// Configuration for `Program.withPersistence`.
type PersistenceSpec<'model> = {
  /// Absolute path to the persistence file.
  Path: string
  /// Convert the model to raw bytes for storage.
  Serialize: 'model -> byte[]
  /// Decode bytes to a model. Return `None` to silently fall back to the program's init model.
  Deserialize: byte[] -> 'model option
  /// Minimum milliseconds between writes. Use `0` in tests for synchronous writes.
  WriteThrottleMs: int
}

/// Helpers for building common `PersistenceSpec` values.
module Persistence =

  /// Build a `PersistenceSpec` backed by UTF-8 text with caller-provided string functions.
  /// `throttleMs` defaults to 100 ms in production use. Pass `0` for synchronous test writes.
  let text<'model>
    (path: string)
    (serialize: 'model -> string)
    (deserialize: string -> 'model option)
    : PersistenceSpec<'model> =
    { Path          = path
      Serialize     = fun m -> Text.Encoding.UTF8.GetBytes(serialize m)
      Deserialize   = fun b ->
        try   Text.Encoding.UTF8.GetString(b) |> deserialize
        with  _ -> None
      WriteThrottleMs = 100 }

// ---------------------------------------------------------------------------
// DebuggerConfig / DebuggerMsg — in-process model inspector overlay
// ---------------------------------------------------------------------------

/// Configuration for the in-process model inspector overlay.
/// Attach it to any program with `Program.withDebugger`.
///
/// `ToggleKey`    — key that shows/hides the overlay (default: F12).
/// `ModelPrinter` — optional custom model renderer; defaults to `sprintf "%A"`.
/// `MaxMessages`  — how many recent dispatched messages to display (default: 50).
[<NoEquality; NoComparison>]
type DebuggerConfig<'model> = {
  ToggleKey:    Key
  ModelPrinter: ('model -> string) option
  MaxMessages:  int
}

/// Helpers for building `DebuggerConfig<'model>` values.
module DebuggerConfig =

  /// Default configuration: F12 toggle, sprintf "%A" model printer, last 50 messages.
  let defaults<'model> : DebuggerConfig<'model> = {
    ToggleKey    = Key.F 12
    ModelPrinter = None
    MaxMessages  = 50
  }

/// Messages for a debugger-wrapped program.
/// App messages are passed through as `AppMsg`; the toggle action is `Toggle`.
[<RequireQualifiedAccess>]
type DebuggerMsg<'msg> =
  | AppMsg of 'msg
  | Toggle

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

  /// Embed a child program into a parent program via a `Lens`.
  /// `liftMsg` wraps child messages into the parent message DU case.
  /// The lens supplies the child-model extraction and update functions.
  ///
  /// Example:
  /// ```fsharp
  /// let counterLens = { Get = fun m -> m.Counter; Set = fun c m -> { m with Counter = c } }
  /// let embedded = Program.embed counterLens ParentMsg.Counter counterProgram
  /// // In parent Update:  | ParentMsg.Counter msg -> embedded.Update msg model
  /// // In parent View:    El.column [ embedded.View model; ... ]
  /// // In parent Subscribe: embedded.Subscribe model @ [ otherSubs ]
  /// ```
  let embed
    (lens: Lens<'parentModel, 'childModel>)
    (liftMsg: 'childMsg -> 'parentMsg)
    (child: Program<'childModel, 'childMsg>)
    : MappedProgram<'parentModel, 'parentMsg, 'childMsg> =
    map liftMsg lens.Get lens.Set child

  /// Embed an optional child program into a parent program via a `Prism`.
  /// When the prism yields `None`, `View` returns `El.empty` and `Update`/`Subscribe`
  /// are no-ops — making illegal states (message arrives for absent component) safe.
  ///
  /// Example:
  /// ```fsharp
  /// let modalPrism = { TryGet = fun m -> m.Modal; Set = fun md m -> { m with Modal = Some md } }
  /// let embedded = Program.embedOptional modalPrism ParentMsg.Modal modalProgram
  /// ```
  let embedOptional
    (prism: Prism<'parentModel, 'childModel>)
    (liftMsg: 'childMsg -> 'parentMsg)
    (child: Program<'childModel, 'childMsg>)
    : MappedProgram<'parentModel, 'parentMsg, 'childMsg> =
    { Init = fun parentModel -> parentModel, NoCmd
      Update = fun childMsg parentModel ->
        match prism.TryGet parentModel with
        | None -> parentModel, NoCmd
        | Some childModel ->
          let newChildModel, childCmd = child.Update childMsg childModel
          prism.Set newChildModel parentModel, Cmd.map liftMsg childCmd
      View = fun parentModel ->
        match prism.TryGet parentModel with
        | None -> Empty
        | Some childModel -> child.View childModel
      Subscribe = fun parentModel ->
        match prism.TryGet parentModel with
        | None -> []
        | Some childModel ->
          child.Subscribe childModel |> List.map (Sub.map liftMsg) }

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

  /// Install a `RecoverWith` error boundary on a program. When the `Update` function throws,
  /// `handler` is called with the exception. Return `Some msg` to dispatch a recovery
  /// message and keep the app running; return `None` to absorb the exception silently.
  ///
  /// The last call to `withOnError` wins — each call replaces the previous handler.
  ///
  /// Example:
  ///   let program = { ... } |> Program.withOnError (fun ex -> Some (ErrorOccurred ex.Message))
  let withOnError (handler: exn -> 'msg option) (program: Program<'model, 'msg>) : Program<'model, 'msg> =
    { program with OnError = RecoverWith handler }

  /// Wrap a program with a visual error banner overlay.
  ///
  /// When `Update` throws an unhandled exception the banner appears at the top of the
  /// terminal, showing the exception message. It auto-dismisses after 5 seconds.
  /// The wrapped model and message types are unchanged.
  ///
  /// Unlike `Program.withOnError`, the banner is rendered by the framework — no
  /// recovery message needs to be added to the application's own message DU.
  ///
  /// For a custom display duration, use `Program.withErrorBannerFor`.
  ///
  /// Example:
  ///   let program = { ... } |> Program.withErrorBanner
  let withErrorBanner (program: Program<'model, 'msg>) : Program<'model, 'msg> =
    let secs = 5.0
    // Mutable ref is acceptable here: this is a framework-boundary function called once
    // at startup, and the ref is closed over by both Update and View closures.
    let bannerRef : (string * System.DateTime) option ref = ref None
    { program with
        Update = fun msg model ->
          try
            program.Update msg model
          with ex ->
            bannerRef.Value <- Some(ex.Message, System.DateTime.UtcNow)
            model, NoCmd
        View = fun model ->
          let innerView = program.View model
          match bannerRef.Value with
          | None -> innerView
          | Some(errMsg, t) ->
            if (System.DateTime.UtcNow - t).TotalSeconds >= secs then
              bannerRef.Value <- None
              innerView
            else
              El.column [
                El.bg Color.red (El.fg Color.white (El.bold (El.text (sprintf " ⚠  %s  (auto-dismiss in %.0fs) " errMsg (secs - (System.DateTime.UtcNow - t).TotalSeconds)))))
                innerView
              ] }

  /// Like `Program.withErrorBanner` but with a configurable display duration in seconds.
  ///
  /// Example:
  ///   let program = { ... } |> Program.withErrorBannerFor 10.0
  let withErrorBannerFor (displaySeconds: float) (program: Program<'model, 'msg>) : Program<'model, 'msg> =
    let secs = displaySeconds
    let bannerRef : (string * System.DateTime) option ref = ref None
    { program with
        Update = fun msg model ->
          try
            program.Update msg model
          with ex ->
            bannerRef.Value <- Some(ex.Message, System.DateTime.UtcNow)
            model, NoCmd
        View = fun model ->
          let innerView = program.View model
          match bannerRef.Value with
          | None -> innerView
          | Some(errMsg, t) ->
            if (System.DateTime.UtcNow - t).TotalSeconds >= secs then
              bannerRef.Value <- None
              innerView
            else
              El.column [
                El.bg Color.red (El.fg Color.white (El.bold (El.text (sprintf " ⚠  %s  (auto-dismiss in %.0fs) " errMsg (secs - (System.DateTime.UtcNow - t).TotalSeconds)))))
                innerView
              ] }

  /// Install an observer that is called after every `Update` invocation.
  /// `interceptor msg oldModel newModel` receives the message that triggered the update,
  /// the model before the update, and the model after.
  ///
  /// Use cases: logging, debugging, time-travel/undo snapshots, hot-reload observation.
  /// The interceptor runs synchronously in the update path — keep it fast (no blocking I/O).
  ///
  /// Multiple interceptors can be chained: each call to `withUpdateInterceptor` wraps
  /// the previous Update function, so both interceptors fire (inner first, then outer).
  ///
  /// Example:
  ///   let program =
  ///     { ... }
  ///     |> Program.withUpdateInterceptor (fun msg old new' ->
  ///         printfn "[Update] %A: %A → %A" msg old new')
  let withUpdateInterceptor
    (interceptor: 'msg -> 'model -> 'model -> unit)
    (program: Program<'model, 'msg>) : Program<'model, 'msg> =
    { program with
        Update = fun msg model ->
          let newModel, cmd = program.Update msg model
          interceptor msg model newModel
          newModel, cmd }

  /// Combine two independent programs into a single program whose model is a tuple and
  /// whose message type is `Choice<'msg1,'msg2>`.
  ///
  /// `viewCompose` receives both sub-views and must produce a single `Element`
  /// (typically `El.row [view1; view2]` or `El.column [view1; view2]`).
  ///
  /// **Note**: For real applications, prefer defining your own discriminated union for messages
  /// and using `Program.embed` for composition. `Choice<>` call sites are hard to read — you
  /// must mentally map `Choice1Of2`/`Choice2Of2` to sub-program semantics. This combinator
  /// is most useful for prototypes and side-by-side demos.
  ///
  /// Example:
  /// ```fsharp
  /// let combined =
  ///   Program.combine
  ///     (fun v1 v2 -> El.row [v1 |> El.fill; v2 |> El.fill])
  ///     counterProgram
  ///     timerProgram
  /// ```
  [<System.Obsolete("Program.combine is a preview API. For production code, prefer Program.embed with your own DU to get readable call sites instead of Choice1Of2/Choice2Of2.")>]
  let combine
    (viewCompose: Element -> Element -> Element)
    (p1: Program<'m1, 'msg1>)
    (p2: Program<'m2, 'msg2>)
    : Program<'m1 * 'm2, Choice<'msg1, 'msg2>> =
    { Init = fun () ->
        let m1, cmd1 = p1.Init()
        let m2, cmd2 = p2.Init()
        (m1, m2), Batch [ Cmd.map Choice1Of2 cmd1; Cmd.map Choice2Of2 cmd2 ]
      Update = fun msg (m1, m2) ->
        match msg with
        | Choice1Of2 msg1 ->
          let m1', c = p1.Update msg1 m1
          (m1', m2), Cmd.map Choice1Of2 c
        | Choice2Of2 msg2 ->
          let m2', c = p2.Update msg2 m2
          (m1, m2'), Cmd.map Choice2Of2 c
      View = fun (m1, m2) ->
        viewCompose (p1.View m1) (p2.View m2)
      Subscribe = fun (m1, m2) ->
        [ yield! p1.Subscribe m1 |> List.map (Sub.map Choice1Of2)
          yield! p2.Subscribe m2 |> List.map (Sub.map Choice2Of2) ]
      OnError = CrashOnError }

  /// Wrap a program with undo/redo history.
  ///
  /// Every inner message for which `shouldCheckpoint msg` returns `true` will:
  ///   1. snapshot the **pre-update** model onto the undo stack, and
  ///   2. clear the redo stack (branching history discards the future).
  ///
  /// When `shouldCheckpoint` returns `false` the inner model updates normally but no
  /// snapshot is taken — useful for transient state changes such as cursor movement
  /// or hover highlights that you don't want cluttering the undo stack.
  ///
  /// The wrapped program's model type becomes `HistoryModel<'model>` and its message
  /// type becomes `HistoryMsg<'msg>`. Wire undo/redo keys in your `Subscribe` function:
  ///
  ///   Sub.keys [ (Key.Char (Text.Rune 'z'), Modifiers.Ctrl), HistoryMsg.Undo
  ///              (Key.Char (Text.Rune 'y'), Modifiers.Ctrl), HistoryMsg.Redo ]
  ///
  /// Access the current inner model via `History.present`:
  ///   TuiExpect.modelSatisfies "undone" (fun h -> (History.present h).Count = 1) app
  ///
  /// Note on memory: `'model` values on the past/future stacks are stored by reference.
  /// If `'model` is fully immutable (as required by correct Elm Architecture usage),
  /// this is zero-copy structural sharing. Mutable fields in `'model` will not be
  /// protected by undo.
  ///
  /// Example:
  ///   let app =
  ///     myProgram
  ///     |> Program.withHistory 50 (function
  ///         | ItemAdded _ | ItemDeleted _ | TextEdited _ -> true
  ///         | _ -> false)
  let withHistory
    (maxDepth: int)
    (shouldCheckpoint: 'msg -> bool)
    (program: Program<'model, 'msg>)
    : Program<HistoryModel<'model>, HistoryMsg<'msg>> =
    { Init = fun () ->
        let m, cmd = program.Init()
        History.init maxDepth m, Cmd.map HistoryMsg.Inner cmd
      Update = fun hmsg hmodel ->
        match hmsg with
        | HistoryMsg.Inner msg ->
          let checkpoint = shouldCheckpoint msg
          let present = History.present hmodel
          let newPresent, cmd = program.Update msg present
          let hmodel' =
            match checkpoint with
            | true  -> History.push hmodel newPresent
            | false -> { hmodel with Present = newPresent }
          hmodel', Cmd.map HistoryMsg.Inner cmd
        | HistoryMsg.Undo ->
          History.undo hmodel, NoCmd
        | HistoryMsg.Redo ->
          History.redo hmodel, NoCmd
        | HistoryMsg.Checkpoint ->
          History.checkpoint hmodel, NoCmd
        | HistoryMsg.ClearHistory ->
          { hmodel with Past = []; Future = [] }, NoCmd
      View = fun hmodel -> program.View (History.present hmodel)
      Subscribe = fun hmodel ->
        program.Subscribe (History.present hmodel)
        |> List.map (Sub.map HistoryMsg.Inner)
      OnError =
        match program.OnError with
        | CrashOnError    -> CrashOnError
        | LogAndContinue  -> LogAndContinue
        | RecoverWith f   -> RecoverWith (fun ex -> f ex |> Option.map HistoryMsg.Inner) }

  /// Wrap a program with a structured logger.
  ///
  /// `onEvent` is called synchronously after each lifecycle transition.
  /// For async/subscription events, use `Logger.toList` with a concurrent queue.
  ///
  /// Composes with `withHistory`: wrap first with `withHistory`, then `withLogging`.
  let withLogging
    (onEvent: LogEvent<'model,'msg> -> unit)
    (program: Program<'model,'msg>)
    : Program<'model,'msg> =
    let countCmds =
      let rec loop cmd =
        match cmd with
        | NoCmd           -> 0
        | Batch cmds      -> cmds |> List.sumBy loop
        | _               -> 1
      loop
    { Init = fun () ->
        let model, cmd = program.Init()
        onEvent (AppStarted model)
        model, cmd
      Update = fun msg model ->
        let newModel, cmd = program.Update msg model
        onEvent (MsgDispatched(msg, model, newModel, countCmds cmd))
        newModel, cmd
      View      = program.View
      Subscribe = program.Subscribe
      OnError   = program.OnError }

  /// Wrap a program with transparent model persistence.
  ///
  /// On `Init`, loads a previously-saved model from `spec.Path` (silently falls
  /// back to the program's own init on any error).  After every `Update`, writes
  /// the new model asynchronously unless `spec.WriteThrottleMs = 0`, in which case
  /// the write is synchronous (for testing).
  let withPersistence
    (spec: PersistenceSpec<'model>)
    (program: Program<'model,'msg>)
    : Program<'model,'msg> =
    let mutable lastWriteUtc = DateTime.MinValue
    let writeModel (model: 'model) =
      try
        let bytes = spec.Serialize model
        let dir = IO.Path.GetDirectoryName(spec.Path)
        if not (isNull dir) && not (IO.Directory.Exists(dir)) then
          IO.Directory.CreateDirectory(dir) |> ignore
        let tmp = IO.Path.Combine(dir, IO.Path.GetRandomFileName())
        IO.File.WriteAllBytes(tmp, bytes)
        IO.File.Move(tmp, spec.Path, overwrite = true)
      with _ -> ()
    { Init = fun () ->
        let defaultModel, cmd = program.Init()
        let model =
          try
            match IO.File.Exists(spec.Path) with
            | false -> defaultModel
            | true  -> spec.Deserialize(IO.File.ReadAllBytes(spec.Path)) |> Option.defaultValue defaultModel
          with _ -> defaultModel
        model, cmd
      Update = fun msg model ->
        let newModel, cmd = program.Update msg model
        let now = DateTime.UtcNow
        let elapsed = (now - lastWriteUtc).TotalMilliseconds
        if elapsed >= float (max 0 spec.WriteThrottleMs) then
          lastWriteUtc <- now
          match spec.WriteThrottleMs with
          | 0 -> writeModel newModel
          | _ -> async { writeModel newModel } |> Async.Start
        newModel, cmd
      View      = program.View
      Subscribe = program.Subscribe
      OnError   = program.OnError }

  /// Attach a live model-inspector overlay to any program.
  ///
  /// The overlay is off by default. Press `cfg.ToggleKey` (F12) to toggle it on/off.
  /// When visible, the overlay shows:
  ///   • Current model (via `cfg.ModelPrinter`, or `sprintf "%A"` by default)
  ///   • Count of recently dispatched messages
  ///
  /// App messages are wrapped in `DebuggerMsg.AppMsg`; the toggle fires `DebuggerMsg.Toggle`.
  ///
  /// Compose AFTER other combinators like `withLogging` to instrument the full pipeline:
  ///   program |> Program.withLogging logger |> Program.withDebugger cfg
  let withDebugger
    (cfg: DebuggerConfig<'model>)
    (program: Program<'model,'msg>)
    : Program<'model, DebuggerMsg<'msg>> =
    let mutable visible = false
    let mutable msgLog : string list = []
    { Init = fun () ->
        let m, cmd = program.Init()
        m, Cmd.map DebuggerMsg.AppMsg cmd
      Update = fun msg model ->
        match msg with
        | DebuggerMsg.Toggle ->
          visible <- not visible
          model, NoCmd
        | DebuggerMsg.AppMsg inner ->
          let newModel, cmd = program.Update inner model
          let label = sprintf "%A" inner
          let updated = label :: msgLog
          msgLog <- if updated.Length > cfg.MaxMessages then List.take cfg.MaxMessages updated else updated
          newModel, Cmd.map DebuggerMsg.AppMsg cmd
      View = fun model ->
        let appView = program.View model
        match visible with
        | false -> appView
        | true ->
          let printer = cfg.ModelPrinter |> Option.defaultValue (fun m -> sprintf "%A" m)
          let modelStr = printer model
          let msgCountStr = sprintf "%d msg" (List.length msgLog)
          let panel =
            El.column [
              El.text "── Debug ──────────────────────────────────────"
              El.text (sprintf "Model: %s" modelStr)
              El.text msgCountStr
            ]
          El.column [ appView; panel ]
      Subscribe = fun model ->
        let innerSubs = program.Subscribe model |> List.map (Sub.map DebuggerMsg.AppMsg)
        let toggleSub =
          KeySub (fun (key, _) ->
            if key = cfg.ToggleKey then Some DebuggerMsg.Toggle
            else None)
        toggleSub :: innerSubs
      OnError =
        match program.OnError with
        | CrashOnError   -> CrashOnError
        | LogAndContinue -> LogAndContinue
        | RecoverWith f  -> RecoverWith (fun ex -> f ex |> Option.map DebuggerMsg.AppMsg) }
/// The most commonly reinvented type in F# async applications — provided here so
/// every user doesn't have to define their own `type LoadState`.
///
/// The `'e` type parameter is the error type stored in the `Failed` case.
/// Use `RemoteDataExn<'a>` for the common case where errors are exceptions.
type RemoteData<'a, 'e> =
  /// No request has been made yet.
  | Idle
  /// A request is in flight.
  | Loading
  /// Data loaded successfully.
  | Loaded of 'a
  /// The request failed with an error of type `'e`.
  | Failed of 'e

/// Convenience alias: `RemoteData` with exceptions as the error type.
/// Equivalent to the pre-0.10 single-parameter `RemoteData<'a>`.
type RemoteDataExn<'a> = RemoteData<'a, exn>

/// Convenience alias: `RemoteData` with string messages as the error type.
type RemoteDataStr<'a> = RemoteData<'a, string>

/// Helpers for working with RemoteData values.
module RemoteData =
  /// Map a function over a Loaded value. Idle, Loading, and Failed pass through unchanged.
  let map (f: 'a -> 'b) (rd: RemoteData<'a, 'e>) : RemoteData<'b, 'e> =
    match rd with
    | Idle -> Idle
    | Loading -> Loading
    | Loaded a -> Loaded(f a)
    | Failed e -> Failed e

  /// Return the loaded value if present, otherwise a default.
  let defaultValue (def: 'a) (rd: RemoteData<'a, 'e>) : 'a =
    match rd with
    | Loaded a -> a
    | _ -> def

  /// Return true only when the data is fully loaded.
  let isLoaded (rd: RemoteData<'a, 'e>) : bool =
    match rd with
    | Loaded _ -> true
    | _ -> false

  /// Return true when a request is in flight.
  let isLoading (rd: RemoteData<'a, 'e>) : bool =
    match rd with
    | Loading -> true
    | _ -> false

  /// Monadic bind — chain operations on loaded data.
  let bind (f: 'a -> RemoteData<'b, 'e>) (rd: RemoteData<'a, 'e>) : RemoteData<'b, 'e> =
    match rd with
    | Idle -> Idle
    | Loading -> Loading
    | Loaded a -> f a
    | Failed e -> Failed e

  /// Transform the error stored in a `Failed` case, potentially changing its type.
  /// All other cases pass through unchanged.
  ///
  /// Useful for converting exceptions to human-readable strings:
  ///   model.Data
  ///   |> RemoteData.mapError (fun e -> sprintf "Failed to load: %s" e.Message)
  ///   |> Deferred.view spinner (fun msg -> El.text msg) renderData
  let mapError (f: 'e -> 'e2) (rd: RemoteData<'a, 'e>) : RemoteData<'a, 'e2> =
    match rd with
    | Idle -> Idle
    | Loading -> Loading
    | Loaded a -> Loaded a
    | Failed e -> Failed(f e)

/// Helpers for the common async data-loading pattern using RemoteData.
///
/// Usage:
///   type Msg = GotPosts of RemoteData<Post list>
///   // In Init or on user action:
///   let fetchPosts () = Deferred.load (fun () -> Api.getPosts()) GotPosts
///   // In View:
///   let postView =
///     Deferred.view (El.text "Loading…") (fun ex -> El.text ex.Message) renderPosts model.Posts
module Deferred =

  /// Start loading remote data. Dispatches `toMsg Loading` immediately (on the next frame),
  /// then dispatches `toMsg (Loaded data)` or `toMsg (Failed ex)` when the async completes.
  ///
  /// Example:
  ///   type Msg = GotPosts of RemoteDataExn<Post list>
  ///   let load () = Deferred.load (fun () -> Api.getPosts()) GotPosts
  let load (fetch: unit -> Async<'a>) (toMsg: RemoteData<'a, exn> -> 'msg) : Cmd<'msg> =
    Batch [
      Delay(0, toMsg Loading)
      OfAsync (fun dispatch -> async {
        let! result = Async.Catch(fetch())
        match result with
        | Choice1Of2 data -> dispatch (toMsg (Loaded data))
        | Choice2Of2 ex   -> dispatch (toMsg (Failed ex))
      })
    ]

  /// Re-trigger a load (e.g., on "Refresh" button). Semantically equivalent to `load`
  /// but communicates intent: this is a user-initiated reload of already-attempted data.
  let reload (fetch: unit -> Async<'a>) (toMsg: RemoteData<'a, exn> -> 'msg) : Cmd<'msg> =
    load fetch toMsg

  /// Standard view pattern for remote data loaded with exceptions as errors.
  ///
  /// - While Idle or Loading: renders `loading`
  /// - On failure: renders `error ex`
  /// - When data is available: renders `render data`
  ///
  /// Example:
  ///   Deferred.view spinner errorView renderPosts model.Posts
  let view (loading: Element) (error: exn -> Element) (render: 'a -> Element) (rd: RemoteData<'a, exn>) : Element =
    match rd with
    | Idle | Loading -> loading
    | Failed ex -> error ex
    | Loaded a -> render a

  /// Like `Deferred.view` but the error handler receives `ex.Message` (a string)
  /// instead of the raw exception. Eliminates the common boilerplate of writing
  /// `(fun ex -> El.text ex.Message)` at every call site.
  ///
  /// Example:
  ///   Deferred.viewString spinner (fun msg -> El.text ("Error: " + msg)) renderPosts model.Posts
  let viewString (loading: Element) (error: string -> Element) (render: 'a -> Element) (rd: RemoteData<'a, exn>) : Element =
    match rd with
    | Idle | Loading -> loading
    | Failed ex -> error ex.Message
    | Loaded a -> render a

  /// Generic view that works with any error type `'e`.
  /// Prefer `Deferred.view` (for exn) or `Deferred.viewString` (for exn→Message)
  /// when loading via `Deferred.load`.
  let viewWith (loading: Element) (error: 'e -> Element) (render: 'a -> Element) (rd: RemoteData<'a, 'e>) : Element =
    match rd with
    | Idle | Loading -> loading
    | Failed e -> error e
    | Loaded a -> render a

// ---------------------------------------------------------------------------
// Diff<'a> — Myers/LCS-based structural diff for any comparable list
// ---------------------------------------------------------------------------

/// Represents a single change in a diff between two sequences.
[<RequireQualifiedAccess>]
type DiffChange<'a> =
  /// Present only in the target sequence (insertion).
  | Added of 'a
  /// Present only in the source sequence (deletion).
  | Removed of 'a
  /// Present in both sequences (no change).
  | Unchanged of 'a

/// Structural diff operations over `'a list` values.
/// Uses the Longest Common Subsequence algorithm (O(N*M) time, O(N*M) space).
/// Suitable for short-to-medium length lists typical in TUI data models.
module Diff =

  // Build the LCS length table via dynamic programming.
  let private lcsTable (a: 'a array) (b: 'a array) =
    let n = a.Length
    let m = b.Length
    let dp = Array2D.zeroCreate<int> (n + 1) (m + 1)
    for i = 1 to n do
      for j = 1 to m do
        if a.[i-1] = b.[j-1] then dp.[i,j] <- dp.[i-1,j-1] + 1
        else dp.[i,j] <- max dp.[i-1,j] dp.[i,j-1]
    dp

  /// Compute the diff between `src` and `tgt`.
  ///
  /// The result is a list of `DiffChange<'a>` values. Applying all `Unchanged`
  /// and `Added` items (in order, skipping `Removed`) reconstructs `tgt` exactly.
  ///
  /// Example:
  ///   Diff.compute ["a";"b";"c"] ["a";"X";"c"]
  ///   // → [Unchanged "a"; Removed "b"; Added "X"; Unchanged "c"]
  let compute (src: 'a list) (tgt: 'a list) : DiffChange<'a> list =
    let a = Array.ofList src
    let b = Array.ofList tgt
    let n = a.Length
    let m = b.Length
    if n = 0 && m = 0 then []
    elif n = 0 then b |> Array.toList |> List.map DiffChange.Added
    elif m = 0 then a |> Array.toList |> List.map DiffChange.Removed
    else
      let dp = lcsTable a b
      let result = System.Collections.Generic.List<DiffChange<'a>>()
      let rec backtrack i j =
        match i, j with
        | 0, 0 -> ()
        | i, 0 ->
          result.Insert(0, DiffChange.Removed a.[i-1])
          backtrack (i-1) 0
        | 0, j ->
          result.Insert(0, DiffChange.Added b.[j-1])
          backtrack 0 (j-1)
        | i, j ->
          if a.[i-1] = b.[j-1] then
            result.Insert(0, DiffChange.Unchanged a.[i-1])
            backtrack (i-1) (j-1)
          elif dp.[i-1,j] >= dp.[i,j-1] then
            result.Insert(0, DiffChange.Removed a.[i-1])
            backtrack (i-1) j
          else
            result.Insert(0, DiffChange.Added b.[j-1])
            backtrack i (j-1)
      backtrack n m
      result |> Seq.toList

  /// Count the number of `Added` changes.
  let countAdded (changes: DiffChange<'a> list) : int =
    changes |> List.sumBy (function DiffChange.Added _ -> 1 | _ -> 0)

  /// Count the number of `Removed` changes.
  let countRemoved (changes: DiffChange<'a> list) : int =
    changes |> List.sumBy (function DiffChange.Removed _ -> 1 | _ -> 0)

  /// Count the number of `Unchanged` items.
  let countUnchanged (changes: DiffChange<'a> list) : int =
    changes |> List.sumBy (function DiffChange.Unchanged _ -> 1 | _ -> 0)

  /// Reconstruct the target sequence from a diff result.
  /// Equivalent to keeping `Added` and `Unchanged` items in order.
  let applyPatch (changes: DiffChange<'a> list) : 'a list =
    changes |> List.choose (function
      | DiffChange.Unchanged x | DiffChange.Added x -> Some x
      | DiffChange.Removed _ -> None)

// ---------------------------------------------------------------------------
// KeyMap<'msg> — composable, zero-alloc-at-runtime modal key bindings
// ---------------------------------------------------------------------------

/// Named input modes for modal keybinding (Vim-style or custom).
[<RequireQualifiedAccess>]
type KeyMode =
  | Normal
  | Insert
  | Visual
  | Custom of string

/// A key sequence: either a single key press or a two-key chord.
/// Recursive so chords can be composed from simpler sequences.
[<RequireQualifiedAccess>]
type KeySeq =
  | Press of key: Key * mods: Modifiers
  | Chord of first: KeySeq * second: KeySeq

/// Composable, immutable keybinding map.
/// The internal `Map` is created once and closed over by `toSub`,
/// eliminating the `Keys.bind` allocation footgun (Dictionary created
/// on every Subscribe call = every model update).
type KeyMap<'msg> = private KeyMap of Map<KeyMode * KeySeq, 'msg>

/// Functions for building and using `KeyMap<'msg>` values.
module KeyMap =
  /// An empty KeyMap with no bindings.
  let empty<'msg> : KeyMap<'msg> = KeyMap Map.empty

  /// Bind a key sequence in a given mode to a message.
  let bind (mode: KeyMode) (seq: KeySeq) (msg: 'msg) (KeyMap m) : KeyMap<'msg> =
    KeyMap (Map.add (mode, seq) msg m)

  /// Convenience: bind a key (no modifiers) in Normal mode.
  let normal (key: Key) (msg: 'msg) (km: KeyMap<'msg>) : KeyMap<'msg> =
    bind KeyMode.Normal (KeySeq.Press(key, Modifiers.None)) msg km

  /// Convenience: bind a key (no modifiers) in Insert mode.
  let insert (key: Key) (msg: 'msg) (km: KeyMap<'msg>) : KeyMap<'msg> =
    bind KeyMode.Insert (KeySeq.Press(key, Modifiers.None)) msg km

  /// Merge two KeyMaps. Right-biased: when both maps bind the same (mode, seq),
  /// the right-hand value wins.
  let merge (KeyMap a) (KeyMap b) : KeyMap<'msg> =
    KeyMap (Map.fold (fun acc k v -> Map.add k v acc) a b)

  /// Right-biased merge operator. `km1 <+> km2` is `merge km1 km2`.
  let (<+>) km1 km2 = merge km1 km2

  /// Transform all bound messages with `f`.
  let map (f: 'a -> 'b) (KeyMap m) : KeyMap<'b> =
    KeyMap (Map.map (fun _ v -> f v) m)

  /// Produce a `Sub<'msg>` that dispatches messages for direct key presses in
  /// the given mode. The closure captures the immutable `Map` — **zero allocation
  /// at subscription time**, unlike `Keys.bind` which allocates a Dictionary.
  let toSub (mode: KeyMode) (KeyMap m) : Sub<'msg> =
    KeySub (fun (key, mods) -> Map.tryFind (mode, KeySeq.Press(key, mods)) m)

  /// Advance a chord-aware key handler.
  ///
  /// - `mode`: current input mode
  /// - `incoming`: the key just pressed (as a `KeySeq.Press`)
  /// - `pending`: the previous unresolved `KeySeq.Press`, if any (for chord tracking)
  /// - Returns `(msg option, hasPendingChord)`:
  ///   - `(Some msg, false)` — key/chord resolved to a message
  ///   - `(None, true)`      — incoming is a chord prefix; store it and wait for next key
  ///   - `(None, false)`     — no match; clear pending state
  let advance (mode: KeyMode) (incoming: KeySeq) (pending: KeySeq option) (KeyMap m) : 'msg option * bool =
    match pending with
    | Some prefix ->
      let chord = KeySeq.Chord(prefix, incoming)
      match Map.tryFind (mode, chord) m with
      | Some msg -> Some msg, false
      | None ->
        match Map.tryFind (mode, incoming) m with
        | Some msg -> Some msg, false
        | None -> None, false
    | None ->
      match Map.tryFind (mode, incoming) m with
      | Some msg -> Some msg, false
      | None ->
        let isPrefix =
          m |> Map.exists (fun (km, seq) _ ->
            km = mode &&
            match seq with
            | KeySeq.Chord(first, _) -> first = incoming
            | KeySeq.Press _ -> false)
        None, isPrefix

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
      Platform = platform
      SupportsOsc8 = false }

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
