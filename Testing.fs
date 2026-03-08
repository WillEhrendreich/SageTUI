namespace SageTUI

open System

/// Represents a SageTUI program under test. Immutable — every TestHarness function
/// returns a new TestApp. Pipe operations together to simulate user interaction.
///
/// Example:
///   let app =
///     TestHarness.init 80 24 myProgram
///     |> TestHarness.pressKey (Key.Char 'j')
///     |> TestHarness.pressKey (Key.Char 'j')
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
  /// Delay(n>0, msg) commands returned from Update, waiting to fire at their
  /// absolute virtual time. Drained by advanceTime. Each entry is
  /// (absoluteFireTime, arrivalOrder, msg) — arrivalOrder is a monotonic counter
  /// used as a tiebreaker when multiple delays share the same fire time.
  PendingDelays: (TimeSpan * int * 'msg) list
  /// Monotonic counter for assigning arrivalOrder to new PendingDelays entries.
  DelaySeq: int
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

  // Process commands synchronously. Handles Quit, Delay(0,msg), Batch, and
  // Delay(n>0,msg) — which is enqueued rather than dropped.
  // Returns (model, hasQuit, exitCode, newPendingDelays, nextSeq).
  let private processCmd
    (program: Program<'model, 'msg>)
    (effectiveNow: TimeSpan)
    (seqStart: int)
    (model: 'model)
    (cmd: Cmd<'msg>)
    : 'model * bool * int option * (TimeSpan * int * 'msg) list * int =
    let rec loop seqN model cmd =
      match cmd with
      | Quit code -> model, true, Some code, [], seqN
      | Delay(0, msg) ->
        let newModel, nextCmd = program.Update msg model
        loop seqN newModel nextCmd
      | Delay(n, msg) ->
        let fireAt = effectiveNow + TimeSpan.FromMilliseconds(float n)
        model, false, None, [ (fireAt, seqN, msg) ], seqN + 1
      | Batch cmds ->
        cmds
        |> List.fold
          (fun (m, q, ec, delays, sn) c ->
            match q with
            | true -> m, true, ec, delays, sn
            | false ->
              let m', q', ec', newDelays, sn' = loop sn m c
              m', q', ec', delays @ newDelays, sn')
          (model, false, None, [], seqN)
      | _ -> model, false, None, [], seqN
    loop seqStart model cmd

  // Apply a list of messages in order, stopping on quit.
  // effectiveNow is used as the base time for computing child delay fire times.
  let private applyMsgs
    (effectiveNow: TimeSpan)
    (app: TestApp<'model, 'msg>)
    (msgs: 'msg list)
    : TestApp<'model, 'msg> =
    let model, hasQuit, exitCode, newDelays, seqN =
      msgs
      |> List.fold
        (fun (m, q, ec, delays, sn) msg ->
          match q with
          | true -> m, true, ec, delays, sn
          | false ->
            let newModel, cmd = app.Program.Update msg m
            let newModel', quit, ec', cmdDelays, sn' =
              processCmd app.Program effectiveNow sn newModel cmd
            newModel', quit, ec', delays @ cmdDelays, sn')
        (app.Model, app.HasQuit, app.ExitCode, [], app.DelaySeq)
    { app with
        Model = model
        HasQuit = hasQuit
        ExitCode = exitCode
        PendingDelays = app.PendingDelays @ newDelays
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
    }
    let model', hasQuit, exitCode, pendingDelays, seqN =
      processCmd program TimeSpan.Zero 0 model cmd
    { app0 with
        Model = model'
        HasQuit = hasQuit
        ExitCode = exitCode
        PendingDelays = pendingDelays
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
    text |> Seq.fold (fun app c -> pressKey (Key.Char c) app) app

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
    let mouseEvent = { Button = LeftButton; X = x; Y = y; Modifiers = Modifiers.None }
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
          | ResizeSub handler -> yield handler (width, height)
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
        |> List.filter (fun (t, _, _) -> t <= newTime)
        |> List.sortWith (fun (t1, s1, _) (t2, s2, _) ->
          let tc = TimeSpan.Compare(t1, t2)
          if tc <> 0 then tc else compare s1 s2)
        |> List.tryHead

      let earliestTimer = List.tryHead remainingTimers

      match earliestDelay, earliestTimer with
      | None, None -> current
      | Some (delayTime, seqN, msg), None ->
        let pending' =
          current.PendingDelays
          |> List.filter (fun (t, s, _) -> not (t = delayTime && s = seqN))
        drain (applyMsgs delayTime { current with PendingDelays = pending' } [ msg ]) []
      | None, Some (timerTime, _, _, msg) ->
        drain (applyMsgs timerTime current [ msg ]) (List.tail remainingTimers)
      | Some (delayTime, seqN, msg), Some (timerTime, _, _, timerMsg) ->
        if TimeSpan.Compare(delayTime, timerTime) <= 0 then
          // Pending delay fires first (or same time — pending delays take priority)
          let pending' =
            current.PendingDelays
            |> List.filter (fun (t, s, _) -> not (t = delayTime && s = seqN))
          drain (applyMsgs delayTime { current with PendingDelays = pending' } [ msg ]) remainingTimers
        else
          drain (applyMsgs timerTime current [ timerMsg ]) (List.tail remainingTimers)

    let finalApp = drain app sortedTimerFires
    { finalApp with VirtualTime = newTime; TimerNextFire = updatedNextFire }

  /// Render the current view to a Buffer. Use Buffer.get to inspect individual cells.
  let renderBuffer (app: TestApp<'model, 'msg>) =
    snd (renderArena app)

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

