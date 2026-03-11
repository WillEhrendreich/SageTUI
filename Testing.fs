namespace SageTUI

open System

/// A Delay(n>0, msg) command waiting to fire at a specific virtual time.
/// Captured by TestHarness when Update returns Cmd.delay and fired by advanceTime.
[<Struct>]
type PendingDelay<'msg> = {
  /// Absolute virtual time when this delay fires.
  FireAt  : TimeSpan
  /// Tie-breaking sequence number assigned on arrival (monotonically increasing).
  Seq     : int
  /// The message dispatched when this delay fires.
  Message : 'msg
}

/// Represents a SageTUI program under test. Immutable — every TestHarness function
/// returns a new TestApp. Pipe operations together to simulate user interaction.
///
/// Example:
///   let app =
///     TestHarness.init 80 24 myProgram
///     |> TestHarness.pressKey (Key.Char (Text.Rune 'j'))
///     |> TestHarness.pressKey (Key.Char (Text.Rune 'j'))
///   app.Model |> Expect.equal "should be 2" 2
///   TestHarness.render app |> Expect.stringContains "rendered" "Count: 2"
type TestApp<'model, 'msg> = {
  /// Current model state after all dispatched messages have been processed.
  Model: 'model
  /// The program under test.
  Program: Program<'model, 'msg>
  /// Simulated terminal width (used for rendering).
  Width: int
  /// Simulated terminal height (used for rendering).
  Height: int
  /// True if any Quit command has been processed.
  HasQuit: bool
  /// The exit code from the most recent Quit command, if any.
  ExitCode: int option
  /// Virtual time elapsed since initialization. Advances with advanceTime.
  VirtualTime: TimeSpan
  /// Tracks the next scheduled fire time for each TimerSub, keyed by timer ID.
  TimerNextFire: Map<string, TimeSpan>
  /// Delay(n>0, msg) commands from Update waiting to fire. Drained by advanceTime.
  PendingDelays: PendingDelay<'msg> list
  /// Monotonic counter for assigning Seq to new PendingDelays entries.
  DelaySeq: int
  /// Async commands (OfAsync, OfCancellableAsync) returned by Init or Update.
  /// Not executed automatically — run them explicitly with TestHarness.runCapturedAsync,
  /// or inspect with TestHarness.capturedCmds. Clear with TestHarness.clearCapturedCmds.
  CapturedAsyncCmds: Cmd<'msg> list
}

/// Testing utilities for SageTUI programs. No real terminal required.
///
/// Design: TestHarness functions route simulated events through your program's
/// Subscribe function, then process resulting messages through Update, handling
/// synchronous commands (Quit, Delay 0ms, Batch) inline. Non-zero Delay commands
/// are captured in PendingDelays and fired by advanceTime with causal semantics:
/// a child delay's fire time is computed relative to its parent's fire time.
/// Async commands (OfAsync, OfCancellableAsync) are NOT executed — assert their
/// presence with Cmd.hasAsync in unit tests of Update.
module TestHarness =

  // Process commands synchronously. Handles Quit, Delay(0,msg), Batch, DirectMsg,
  // and Delay(n>0,msg) — which is enqueued rather than dropped.
  // OfAsync and OfCancellableAsync are collected into capturedAsync.
  // Returns (model, hasQuit, exitCode, newPendingDelays, capturedAsync, nextSeq).
  let private processCmd
    (program: Program<'model, 'msg>)
    (effectiveNow: TimeSpan)
    (seqStart: int)
    (model: 'model)
    (cmd: Cmd<'msg>)
    : 'model * bool * int option * PendingDelay<'msg> list * Cmd<'msg> list * int =
    let rec loop seqN model cmd asyncAcc =
      match cmd with
      | Quit code -> model, true, Some code, [], asyncAcc, seqN
      | Delay(0, msg) ->
        let newModel, nextCmd = program.Update msg model
        loop seqN newModel nextCmd asyncAcc
      | DirectMsg msg ->
        let newModel, nextCmd = program.Update msg model
        loop seqN newModel nextCmd asyncAcc
      | Delay(n, msg) ->
        let delay = { FireAt = effectiveNow + TimeSpan.FromMilliseconds(float n); Seq = seqN; Message = msg }
        model, false, None, [ delay ], asyncAcc, seqN + 1
      | OfAsync _ | OfCancellableAsync _ ->
        model, false, None, [], asyncAcc @ [ cmd ], seqN
      | Batch cmds ->
        cmds
        |> List.fold
          (fun (m, q, ec, delays, asynAcc2, sn) c ->
            match q with
            | true -> m, true, ec, delays, asynAcc2, sn
            | false ->
              let m', q', ec', newDelays, newAsync, sn' = loop sn m c asynAcc2
              m', q', ec', delays @ newDelays, newAsync, sn')
          (model, false, None, [], asyncAcc, seqN)
      | _ -> model, false, None, [], asyncAcc, seqN
    loop seqStart model cmd []

  // Apply a list of messages in order, stopping on quit.
  // effectiveNow is used as the base time for computing child delay fire times.
  let private applyMsgs
    (effectiveNow: TimeSpan)
    (app: TestApp<'model, 'msg>)
    (msgs: 'msg list)
    : TestApp<'model, 'msg> =
    let model, hasQuit, exitCode, newDelays, capturedAsync, seqN =
      msgs
      |> List.fold
        (fun (m, q, ec, delays, asyncAcc, sn) msg ->
          match q with
          | true -> m, true, ec, delays, asyncAcc, sn
          | false ->
            let newModel, cmd = app.Program.Update msg m
            let newModel', quit, ec', cmdDelays, newAsync, sn' =
              processCmd app.Program effectiveNow sn newModel cmd
            newModel', quit, ec', delays @ cmdDelays, asyncAcc @ newAsync, sn')
        (app.Model, app.HasQuit, app.ExitCode, [], [], app.DelaySeq)
    { app with
        Model = model
        HasQuit = hasQuit
        ExitCode = exitCode
        PendingDelays = app.PendingDelays @ newDelays
        CapturedAsyncCmds = app.CapturedAsyncCmds @ capturedAsync
        DelaySeq = seqN }

  /// Initialize a test app from a program. Runs Init and processes any synchronous commands.
  let init (width: int) (height: int) (program: Program<'model, 'msg>) : TestApp<'model, 'msg> =
    let model, cmd = program.Init()
    let app0 = {
      Model = model
      Program = program
      Width = width
      Height = height
      HasQuit = false
      ExitCode = None
      VirtualTime = TimeSpan.Zero
      TimerNextFire = Map.empty
      PendingDelays = []
      DelaySeq = 0
      CapturedAsyncCmds = []
    }
    let model', hasQuit, exitCode, pendingDelays, capturedAsync, seqN =
      processCmd program TimeSpan.Zero 0 model cmd
    { app0 with
        Model = model'
        HasQuit = hasQuit
        ExitCode = exitCode
        PendingDelays = pendingDelays
        CapturedAsyncCmds = capturedAsync
        DelaySeq = seqN }

  /// Simulate pressing a key (no modifier).
  let pressKey (key: Key) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | KeySub handler ->
            match handler (key, Modifiers.None) with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate pressing a key with modifier keys held (e.g., Ctrl, Shift, Alt).
  let pressKeyWith (key: Key) (mods: Modifiers) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | KeySub handler ->
            match handler (key, mods) with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate typing a string character by character (no modifiers).
  let typeText (text: string) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    text |> Seq.fold (fun app c -> pressKey (Key.Char (Text.Rune c)) app) app

  /// Directly send a message to Update, bypassing subscriptions.
  /// Useful for testing Update logic in isolation.
  let sendMsg (msg: 'msg) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    applyMsgs app.VirtualTime app [ msg ]

  /// Simulate a focus-next event (e.g., Tab key) via FocusSub handlers.
  let focusNext (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | FocusSub handler ->
            match handler FocusNext with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate a focus-previous event (e.g., Shift+Tab) via FocusSub handlers.
  let focusPrev (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | FocusSub handler ->
            match handler FocusPrev with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  // Render to arena + buffer (internal, keeps arena alive for hit testing).
  let private renderArena (app: TestApp<'model, 'msg>) =
    let elem = app.Program.View app.Model
    let arena = FrameArena.create 1024 16384 1024
    FrameArena.reset arena
    let handle = Arena.lower arena elem
    let buf = Buffer.create app.Width app.Height
    let area = { X = 0; Y = 0; Width = app.Width; Height = app.Height }
    ArenaRender.renderRoot arena handle area buf
    arena, buf

  /// Simulate a left-mouse-button click at screen coordinates (x, y).
  /// Renders the current view to build the hit map, then dispatches to ClickSub
  /// and MouseSub handlers. If (x, y) overlaps a Keyed element, its key is passed
  /// to ClickSub handlers; otherwise None is passed.
  let clickAt (x: int) (y: int) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let arena, _ = renderArena app
    let hitKey = ArenaRender.hitTest arena x y
    let mouseEvent = { Button = LeftButton; X = x; Y = y; Modifiers = Modifiers.None; Phase = Pressed }
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | ClickSub handler ->
            match handler (mouseEvent, hitKey) with
            | Some msg -> yield msg
            | None -> ()
          | MouseSub handler ->
            match handler mouseEvent with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate a mouse button release at (x, y). Routes to MouseSub handlers.
  /// Note: ClickSub receives presses only; release events go to MouseSub.
  let releaseAt (x: int) (y: int) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let mouseEvent = { Button = LeftButton; X = x; Y = y; Modifiers = Modifiers.None; Phase = Released }
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | MouseSub handler ->
            match handler mouseEvent with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate a mouse drag motion event at (x, y). Routes to DragSub handlers only.
  /// Button-event tracking (?1002h) must be enabled in a real terminal for motion events
  /// to be received; in the test harness this fires DragSub regardless.
  let dragAt (x: int) (y: int) (button: MouseButton) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let mouseEvent = { Button = button; X = x; Y = y; Modifiers = Modifiers.None; Phase = Motion }
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | DragSub handler ->
            match handler mouseEvent with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate a scroll wheel event at (x, y) with the given button (ScrollUp or ScrollDown).
  /// Routes to both MouseSub and ClickSub handlers with Phase = Pressed, just as a real
  /// terminal would deliver wheel events. No hit-testing is performed — all matching
  /// subscribers receive the event regardless of position.
  ///
  /// Use this to test Viewport.handleMouse, VirtualList.moveDown/Up, Select.handleMouse,
  /// and any other component that reacts to scroll wheel input.
  let scrollAt (x: int) (y: int) (button: MouseButton) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let arena, _ = renderArena app
    let hitKey = ArenaRender.hitTest arena x y
    let mouseEvent = { Button = button; X = x; Y = y; Modifiers = Modifiers.None; Phase = Pressed }
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | ClickSub handler ->
            match handler (mouseEvent, hitKey) with
            | Some msg -> yield msg
            | None -> ()
          | MouseSub handler ->
            match handler mouseEvent with
            | Some msg -> yield msg
            | None -> ()
          | _ -> () ]
    applyMsgs app.VirtualTime app msgs

  /// Simulate a terminal resize. Routes through ResizeSub handlers and updates
  /// Width/Height so subsequent renders use the new dimensions.
  let resize (width: int) (height: int) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | ResizeSub handler ->
            match handler (width, height) with
            | Some msg -> yield msg
            | None     -> ()
          | _ -> () ]
    let app' = applyMsgs app.VirtualTime app msgs
    { app' with Width = width; Height = height }

  /// Advance virtual time by dt, firing any TimerSub whose interval has elapsed
  /// AND any Delay(n>0, msg) commands whose absolute fire time has been reached.
  ///
  /// Firing rules:
  /// - Pending delays (from Update) and timer fires are processed in temporal order.
  /// - At the same virtual time, pending delays fire before timer subs.
  /// - Causal semantics: a child delay's fire time is relative to its parent's
  ///   fire time, not the advanceTime call's end time. A Delay(100,A) that fires
  ///   at T=100 and whose Update returns Delay(50,B) will have B fire at T=150.
  /// - advanceTime fully drains all fires within [currentTime, newTime], including
  ///   cascading chains generated by fired delays.
  /// - TimerSub: if dt > interval, the timer fires multiple times.
  ///   Multiple timers at the same virtual time are sorted by ID for determinism.
  let advanceTime (dt: TimeSpan) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let newTime = app.VirtualTime + dt
    let subs = app.Program.Subscribe app.Model

    // Collect all timer fires sorted by (time, id)
    let sortedTimerFires =
      [ for sub in subs do
          match sub with
          | TimerSub(id, interval, tick) ->
            let nextFire =
              app.TimerNextFire
              |> Map.tryFind id
              |> Option.defaultWith (fun () -> interval)
            let mutable t = nextFire
            while t <= newTime do
              yield (t, id, interval, tick ())
              t <- t + interval
          | _ -> () ]
      |> List.sortWith (fun (t1, id1, _, _) (t2, id2, _, _) ->
        let tc = TimeSpan.Compare(t1, t2)
        if tc <> 0 then tc
        else String.Compare(id1, id2, StringComparison.Ordinal))

    let updatedNextFire =
      sortedTimerFires
      |> List.fold (fun acc (t, id, interval, _) -> Map.add id (t + interval) acc) app.TimerNextFire

    // Unified drain: process pending delays and timer fires in temporal order.
    // At each step, process whichever fires earliest. Pending delays at the same
    // time as a timer fire take priority (arrivalOrder < Int32.MaxValue).
    // Each fired message may produce new pending delays; the loop continues
    // until no pending delays and no timer fires remain within newTime.
    let rec drain
      (current: TestApp<'model, 'msg>)
      (remainingTimers: (TimeSpan * string * TimeSpan * 'msg) list)
      : TestApp<'model, 'msg> =

      let earliestDelay =
        current.PendingDelays
        |> List.filter (fun d -> d.FireAt <= newTime)
        |> List.sortWith (fun a b ->
          let tc = TimeSpan.Compare(a.FireAt, b.FireAt)
          if tc <> 0 then tc else compare a.Seq b.Seq)
        |> List.tryHead

      let earliestTimer = List.tryHead remainingTimers

      match earliestDelay, earliestTimer with
      | None, None -> current
      | Some d, None ->
        let pending' = current.PendingDelays |> List.filter (fun x -> not (x.FireAt = d.FireAt && x.Seq = d.Seq))
        drain (applyMsgs d.FireAt { current with PendingDelays = pending' } [ d.Message ]) []
      | None, Some (timerTime, _, _, msg) ->
        drain (applyMsgs timerTime current [ msg ]) (List.tail remainingTimers)
      | Some d, Some (timerTime, _, _, timerMsg) ->
        if TimeSpan.Compare(d.FireAt, timerTime) <= 0 then
          // Pending delay fires first (or same time — pending delays take priority)
          let pending' = current.PendingDelays |> List.filter (fun x -> not (x.FireAt = d.FireAt && x.Seq = d.Seq))
          drain (applyMsgs d.FireAt { current with PendingDelays = pending' } [ d.Message ]) remainingTimers
        else
          drain (applyMsgs timerTime current [ timerMsg ]) (List.tail remainingTimers)

    let finalApp = drain app sortedTimerFires
    { finalApp with VirtualTime = newTime; TimerNextFire = updatedNextFire }

  /// Render the current view to a Buffer. Use Buffer.get to inspect individual cells.
  let renderBuffer (app: TestApp<'model, 'msg>) =
    snd (renderArena app)

  /// Render the current view and return keyed element areas from the arena hit map.
  /// Useful for asserting layout-scoped interactive regions in tests.
  let keyAreas (app: TestApp<'model, 'msg>) : Map<string, Area> =
    let arena, _ = renderArena app
    ArenaRender.keyAreas arena

  /// Try to get the rendered area for a keyed element in the current view.
  let tryFindKeyArea (key: string) (app: TestApp<'model, 'msg>) : Area option =
    keyAreas app |> Map.tryFind key

  /// Render the current view to a plain string (rows separated by '\n').
  let render (app: TestApp<'model, 'msg>) : string =
    renderBuffer app |> Buffer.toString

  /// Render the current view and split into lines.
  let renderLines (app: TestApp<'model, 'msg>) : string array =
    render app |> fun s -> s.Split('\n')

  /// Render any Element to a plain string without a TestApp.
  /// Useful in unit tests of view functions.
  ///
  /// Example:
  ///   let output = TestHarness.renderElement 80 5 (myView model)
  ///   output |> Expect.stringContains "shows title" "My App"
  let renderElement (width: int) (height: int) (elem: Element) : string =
    let arena = FrameArena.create 1024 16384 1024
    FrameArena.reset arena
    let handle = Arena.lower arena elem
    let buf = Buffer.create width height
    let area = { X = 0; Y = 0; Width = width; Height = height }
    ArenaRender.renderRoot arena handle area buf
    Buffer.toString buf

  /// Render any Element to a plain string and split into lines.
  let renderElementLines (width: int) (height: int) (elem: Element) : string array =
    renderElement width height elem |> fun s -> s.Split('\n')

  /// Returns the number of Delay(n>0, msg) commands currently enqueued,
  /// waiting to be fired by advanceTime.
  let pendingDelayCount (app: TestApp<'model, 'msg>) : int =
    List.length app.PendingDelays

  /// Send multiple messages in order, equivalent to chaining sendMsg calls.
  let sendMsgs (msgs: 'msg list) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    List.fold (fun a m -> sendMsg m a) app msgs

  /// Returns the list of OfAsync / OfCancellableAsync commands that were returned by
  /// Init or Update but not yet executed. Use runCapturedAsync to execute them.
  let capturedCmds (app: TestApp<'model, 'msg>) : Cmd<'msg> list =
    app.CapturedAsyncCmds

  /// Remove all captured async commands from the test app without running them.
  let clearCapturedCmds (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    { app with CapturedAsyncCmds = [] }

  /// Run all captured async commands (OfAsync / OfCancellableAsync) synchronously,
  /// dispatching any resulting messages through Update, and return the updated TestApp.
  ///
  /// Captured async cmds are cleared before execution. Any new async cmds produced
  /// by Update in response to dispatched messages are captured for the next round.
  ///
  /// Example:
  ///   let app = TestHarness.init 80 24 program          // Init returns OfAsync
  ///   let! app2 = TestHarness.runCapturedAsync app       // runs the async, dispatches result
  ///   app2.Model |> Expect.equal "loaded" expectedModel
  let runCapturedAsync (app: TestApp<'model, 'msg>) : Async<TestApp<'model, 'msg>> =
    async {
      let toRun = app.CapturedAsyncCmds
      let app' = { app with CapturedAsyncCmds = [] }
      let mutable current = app'
      for cmd in toRun do
        match cmd with
        | OfAsync run ->
          let msgs = System.Collections.Generic.List<'msg>()
          do! run (fun msg -> msgs.Add(msg))
          current <- applyMsgs current.VirtualTime current (List.ofSeq msgs)
        | OfCancellableAsync(_, run) ->
          use cts = new System.Threading.CancellationTokenSource()
          let msgs = System.Collections.Generic.List<'msg>()
          do! run cts.Token (fun msg -> msgs.Add(msg))
          current <- applyMsgs current.VirtualTime current (List.ofSeq msgs)
        | _ -> ()
      return current
    }

  /// Run all `OfAsync` commands in a `Cmd` tree, collecting the dispatched messages.
  /// Does not require a full `TestApp` — useful for directly testing the output of a `Cmd`.
  ///
  /// Example:
  ///   let cmd = Cmd.ofAsyncResult (async { return Ok 42 }) id (fun _ -> -1)
  ///   let! msgs = TestHarness.runCmdAsync cmd
  ///   msgs |> Expect.equal "one ok message" [42]
  let runCmdAsync (cmd: Cmd<'msg>) : Async<'msg list> =
    async {
      let messages = System.Collections.Generic.List<'msg>()
      let rec run (c: Cmd<'msg>) =
        async {
          match c with
          | OfAsync runFn ->
            do! runFn messages.Add
          | OfCancellableAsync(_, runFn) ->
            use cts = new System.Threading.CancellationTokenSource()
            do! runFn cts.Token messages.Add
          | Batch cmds ->
            for c2 in cmds do
              do! run c2
          | _ -> ()
        }
      do! run cmd
      return messages |> Seq.toList
    }

/// Assertion helpers for SageTUI test programs.
///
/// Functions return unit and throw System.Exception on failure, so they work
/// with any .NET test framework (Expecto, xUnit, NUnit, etc.).
/// Failure messages include the rendered output framed in a box and the label,
/// so failing tests are immediately understandable without grepping string dumps.
///
/// Usage at the end of a pipeline:
///   TestHarness.init 80 24 myProgram
///   |> TestHarness.pressKey (Key.Char (Text.Rune 'j'))
///   |> TuiExpect.viewContains "shows item 2" "Item 2"
///   |> TuiExpect.modelSatisfies "selection advanced" (fun m -> m.Selected = 1)
module TuiExpect =

  let private frameOutput (width: int) (height: int) (output: string) : string =
    let lines = output.Split('\n')
    let top = "╔" + String.replicate width "═" + "╗"
    let bottom = "╚" + String.replicate width "═" + "╝"
    let body =
      lines
      |> Array.map (fun line ->
        let trimmed = if line.Length > width then line.[..width - 1] else line
        sprintf "║%s║" (trimmed.PadRight(width)))
      |> String.concat "\n"
    sprintf "Rendered output (%dx%d):\n%s\n%s\n%s" width height top body bottom

  /// Assert rendered output contains needle.
  /// On failure: shows label, needle, and full render framed in a box.
  let viewContains (label: string) (needle: string) (app: TestApp<'m,'msg>) : unit =
    let output = TestHarness.render app
    if not (output.Contains(needle)) then
      let framed = frameOutput app.Width app.Height output
      failwith (sprintf "Test '%s': render output did not contain \"%s\"\n\n%s" label needle framed)

  /// Assert rendered output does NOT contain needle.
  /// On failure: shows label, needle, and full render framed in a box.
  let viewNotContains (label: string) (needle: string) (app: TestApp<'m,'msg>) : unit =
    let output = TestHarness.render app
    if output.Contains(needle) then
      let framed = frameOutput app.Width app.Height output
      failwith (sprintf "Test '%s': render output unexpectedly contained \"%s\"\n\n%s" label needle framed)

  /// Assert on a raw rendered string (works outside TestApp).
  /// On failure: shows label, needle, and the string framed in a box.
  let stringViewContains (label: string) (needle: string) (output: string) : unit =
    if not (output.Contains(needle)) then
      let width = output.Split('\n') |> Array.map (fun l -> l.Length) |> Array.fold max 0
      let height = output.Split('\n').Length
      let framed = frameOutput width height output
      failwith (sprintf "Test '%s': rendered string did not contain \"%s\"\n\n%s" label needle framed)

  /// Assert model satisfies predicate.
  /// On failure: shows label and model value via sprintf "%A".
  let modelSatisfies (label: string) (predicate: 'm -> bool) (app: TestApp<'m,'msg>) : unit =
    if not (predicate app.Model) then
      failwith (sprintf "Test '%s': model predicate failed.\nModel: %A" label app.Model)

  /// Assert app has not quit.
  let isRunning (label: string) (app: TestApp<'m,'msg>) : unit =
    if app.HasQuit then
      failwith (sprintf "Test '%s': expected app to be running but it has quit (exit code: %A)" label app.ExitCode)

  /// Assert app has quit with the given exit code.
  let hasQuitWith (exitCode: int) (label: string) (app: TestApp<'m,'msg>) : unit =
    match app.HasQuit, app.ExitCode with
    | true, Some code when code = exitCode -> ()
    | true, Some code ->
      failwith (sprintf "Test '%s': expected quit with code %d but got code %d" label exitCode code)
    | true, None ->
      failwith (sprintf "Test '%s': app has quit but ExitCode is None (expected %d)" label exitCode)
    | false, _ ->
      failwith (sprintf "Test '%s': expected app to have quit but it is still running" label)

  /// Assert exactly count pending delays are enqueued.
  let hasPendingDelays (label: string) (count: int) (app: TestApp<'m,'msg>) : unit =
    let actual = List.length app.PendingDelays
    if actual <> count then
      failwith (sprintf "Test '%s': expected %d pending delay(s) but found %d" label count actual)

  /// Assert virtual time has reached at least the given span.
  let timeIsAtLeast (label: string) (expected: TimeSpan) (app: TestApp<'m,'msg>) : unit =
    if app.VirtualTime < expected then
      failwith (sprintf "Test '%s': expected VirtualTime >= %A but got %A" label expected app.VirtualTime)

  /// Assert that the app's model satisfies `predicate`, showing a diff-style message on failure.
  ///
  /// Provides richer output than `modelSatisfies` by printing both current and previous model state.
  /// The `app` itself carries the current model — use `TestHarness.sendMsgs` to advance state first.
  ///
  /// Example:
  ///   let app = TestHarness.init 80 3 program |> TestHarness.sendMsg Login
  ///   TuiExpect.modelChanged "user logged in" (fun m -> m.IsLoggedIn) app
  let modelChanged (label: string) (predicate: 'm -> bool) (app: TestApp<'m,'msg>) : unit =
    if not (predicate app.Model) then
      failwith (sprintf "Test '%s': model predicate failed.\nCurrent model: %A" label app.Model)

/// Session replay and minimal-reproduction shrinking for SageTUI programs.
///
/// replaySession replays a sequence of messages against a TestApp (already initialized),
/// returning the final TestApp state.
///
/// shrinkReplay finds the minimal subsequence of messages that still causes a predicate
/// to return true (i.e. the predicate represents a failing assertion). Uses a
/// prefix-shrink pass followed by a ddmin-style subset reduction:
///   1. Binary-search for the shortest failing prefix.
///   2. Remove chunks of the prefix until no further reduction is possible.
///
/// sessionToLines / replayFromLines: serialize/deserialize a message list to/from
/// string arrays (JSONL-style). Users provide serialize/deserialize functions —
/// the library is serialization-format agnostic.
///
/// Example (session replay):
///   let app = TestHarness.init 80 24 myProgram
///   let result = Testing.replaySession recordedMsgs app
///   result.Model |> Expect.equal "expected state" expectedModel
///
/// Example (shrinking):
///   let minimal = Testing.shrinkReplay longMsgList app (fun a -> a.Model.Count > 100)
///   // minimal = Some [msgThatCausesCount100]
module Testing =

  /// Replay a sequence of messages against an already-initialized TestApp,
  /// processing each through Update in order. Stops early if a Quit command fires.
  let replaySession (msgs: 'msg list) (app: TestApp<'model,'msg>) : TestApp<'model,'msg> =
    TestHarness.sendMsgs msgs app

  /// Find the minimal subsequence of messages that still causes `predicate` to return true.
  ///
  /// Returns None if `predicate` does not hold for the full `msgs` sequence.
  /// Returns Some(minimalSubsequence) otherwise.
  ///
  /// Algorithm:
  ///   Phase 1 — prefix shrink: binary-search for shortest failing prefix.
  ///   Phase 2 — ddmin: iteratively remove half-sized chunks from the prefix until
  ///             no further reduction is possible (O(n log n) predicate evaluations).
  let shrinkReplay
    (msgs: 'msg list)
    (app: TestApp<'model,'msg>)
    (predicate: TestApp<'model,'msg> -> bool)
    : 'msg list option =
    let arr = Array.ofList msgs
    let isFailing (indices: int array) =
      let subset = indices |> Array.map (fun i -> arr.[i]) |> Array.toList
      let result = replaySession subset app
      predicate result
    let n = arr.Length
    // Phase 1: binary search for shortest failing prefix
    let prefixShrink (maxIdx: int) : int =
      let rec bsearch lo hi =
        if lo >= hi then lo
        else
          let mid = (lo + hi) / 2
          match isFailing (Array.init (mid + 1) id) with
          | true  -> bsearch lo mid
          | false -> bsearch (mid + 1) hi
      bsearch 0 maxIdx
    // Phase 2: ddmin on index array — operates on indices to handle duplicate messages
    let rec ddminIndices (indices: int array) (granularity: int) =
      let m = indices.Length
      if m <= 1 then indices
      else
        let chunkSize = max 1 (m / granularity)
        let chunks =
          [| for i in 0 .. (granularity - 1) do
               let start = i * chunkSize
               let len   = min chunkSize (m - start)
               if len > 0 then yield indices.[start .. start + len - 1] |]
        let tryRemove =
          chunks |> Array.tryPick (fun chunk ->
            let chunkSet = System.Collections.Generic.HashSet<int>(chunk)
            let complement = indices |> Array.filter (fun idx -> not (chunkSet.Contains idx))
            match isFailing complement with
            | true  -> Some complement
            | false -> None)
        match tryRemove with
        | Some reduced -> ddminIndices reduced 2
        | None ->
          match granularity >= m with
          | true  -> indices
          | false -> ddminIndices indices (min m (granularity * 2))
    match n = 0 || not (isFailing (Array.init n id)) with
    | true  -> None
    | false ->
      let prefixEnd = prefixShrink (n - 1)
      let shortened  = Array.init (prefixEnd + 1) id
      let minIndices = ddminIndices shortened 2
      Some (minIndices |> Array.map (fun i -> arr.[i]) |> Array.toList)

  /// Serialize a list of messages to an array of strings, one per message.
  /// The `serialize` function converts a message to a string representation.
  /// Use with replayFromLines to persist/restore sessions.
  let sessionToLines (serialize: 'msg -> string) (msgs: 'msg list) : string array =
    msgs |> List.map serialize |> Array.ofList

  /// Deserialize an array of strings back to a list of messages.
  /// Lines for which `deserialize` returns None are silently skipped.
  /// Use with sessionToLines to persist/restore sessions.
  let replayFromLines (deserialize: string -> 'msg option) (lines: string array) : 'msg list =
    lines |> Array.choose deserialize |> Array.toList

  /// Route a single `TerminalEvent` through the appropriate TestHarness handler.
  ///
  /// Mapping:
  ///   KeyPressed(key, mods)  → TestHarness.pressKeyWith
  ///   Resized(w, h)          → TestHarness.resize
  ///   MouseInput me          → TestHarness.clickAt (LeftButton only)
  ///   FocusGained/FocusLost  → no-op (no Sub equivalent in test harness)
  let routeTerminalEvent (event: TerminalEvent) (app: TestApp<'model,'msg>) : TestApp<'model,'msg> =
    match event with
    | KeyPressed(key, mods) -> TestHarness.pressKeyWith key mods app
    | Resized(w, h)         -> TestHarness.resize w h app
    | MouseInput me         ->
      match me.Phase with
      | Pressed  -> TestHarness.clickAt me.X me.Y app
      | Released -> TestHarness.releaseAt me.X me.Y app
      | Motion   -> TestHarness.dragAt me.X me.Y me.Button app
    | FocusGained           -> app
    | FocusLost             -> app
    | Pasted _              -> app  // No PasteSub in TestHarness yet

  /// Replay a recorded session file against a TestApp.
  ///
  /// Reads the file at `path` using `Recording.readAllLines`, decodes each line as a
  /// `TerminalEvent`, and routes it through `routeTerminalEvent`. Lines that fail to
  /// decode are silently skipped (same behaviour as `Recording.playback`).
  ///
  /// Combine with `shrinkReplay` to find minimal reproducing event sequences.
  let replayRecording (path: string) (app: TestApp<'model,'msg>) : TestApp<'model,'msg> =
    Recording.readAllLines path
    |> List.choose Recording.decodeEvent
    |> List.fold (fun a evt -> routeTerminalEvent evt a) app

  /// Shrink `msgs` to the minimal subset that still causes `predicate` to fire,
  /// write a self-contained Expecto `.fsx` snippet to `outputPath`, and return
  /// `Some outputPath`. Returns `None` if the predicate never fires.
  ///
  /// The generated file contains:
  ///   • The serialized minimal message sequence as an F# list literal.
  ///   • A stub `testCase` that replays it and asserts the predicate holds.
  ///
  /// Example:
  ///   let! path =
  ///     Testing.exportShrunkTest recordedMsgs app (fun a -> a.Model.HasCrash) "repro.fsx" string
  let exportShrunkTest
    (msgs: 'msg list)
    (app: TestApp<'model,'msg>)
    (predicate: TestApp<'model,'msg> -> bool)
    (outputPath: string)
    (serializeMsg: 'msg -> string)
    : Async<string option> =
    async {
      match shrinkReplay msgs app predicate with
      | None -> return None
      | Some minimal ->
        let msgLiteral =
          minimal
          |> List.map serializeMsg
          |> List.map (fun s -> sprintf "        %s" s)
          |> String.concat "\n"
        let content =
          sprintf
            """// Auto-generated minimal reproduction — do not edit by hand.
// Replay these messages into your program to trigger the predicate.
// Generated by Testing.exportShrunkTest.

let minimalMsgs = [
%s
]
"""
            msgLiteral
        do! System.IO.File.WriteAllTextAsync(outputPath, content) |> Async.AwaitTask
        return Some outputPath
    }

