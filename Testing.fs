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
}

/// Testing utilities for SageTUI programs. No real terminal required.
///
/// Design: TestHarness functions route simulated events through your program's
/// Subscribe function, then process resulting messages through Update, handling
/// synchronous commands (Quit, Delay 0ms, Batch) inline. Async commands are NOT
/// executed — assert their presence with Cmd.hasAsync in unit tests of Update.
module TestHarness =

  // Process commands synchronously. Handles Quit, Delay(0,msg), and Batch.
  // All other Cmd cases (OfAsync, TimerSub, etc.) are intentionally skipped.
  let private processCmd
    (program: Program<'model, 'msg>)
    (model: 'model)
    (cmd: Cmd<'msg>)
    : 'model * bool * int option =
    let rec loop model cmd =
      match cmd with
      | Quit code -> model, true, Some code
      | Delay(0, msg) ->
        let newModel, nextCmd = program.Update msg model
        loop newModel nextCmd
      | Batch cmds ->
        cmds
        |> List.fold
          (fun (m, q, ec) c ->
            match q with
            | true -> m, true, ec
            | false -> loop m c)
          (model, false, None)
      | _ -> model, false, None
    loop model cmd

  // Apply a list of messages in order, stopping on quit.
  let private applyMsgs (app: TestApp<'model, 'msg>) (msgs: 'msg list) : TestApp<'model, 'msg> =
    let model, hasQuit, exitCode =
      msgs
      |> List.fold
        (fun (m, q, ec) msg ->
          match q with
          | true -> m, true, ec
          | false ->
            let newModel, cmd = app.Program.Update msg m
            let newModel', quit, ec' = processCmd app.Program newModel cmd
            newModel', quit, ec')
        (app.Model, app.HasQuit, app.ExitCode)
    { app with Model = model; HasQuit = hasQuit; ExitCode = exitCode }

  /// Initialize a test app from a program. Runs Init and processes any synchronous commands.
  let init (width: int) (height: int) (program: Program<'model, 'msg>) : TestApp<'model, 'msg> =
    let model, cmd = program.Init()
    let model', hasQuit, exitCode = processCmd program model cmd
    { Model = model'
      Program = program
      Width = width
      Height = height
      HasQuit = hasQuit
      ExitCode = exitCode
      VirtualTime = TimeSpan.Zero
      TimerNextFire = Map.empty }

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
    applyMsgs app msgs

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
    applyMsgs app msgs

  /// Simulate typing a string character by character (no modifiers).
  let typeText (text: string) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    text |> Seq.fold (fun app c -> pressKey (Key.Char c) app) app

  /// Directly send a message to Update, bypassing subscriptions.
  /// Useful for testing Update logic in isolation.
  let sendMsg (msg: 'msg) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    applyMsgs app [ msg ]

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
    applyMsgs app msgs

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
    applyMsgs app msgs

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
    applyMsgs app msgs

  /// Simulate a terminal resize. Routes through ResizeSub handlers and updates
  /// Width/Height so subsequent renders use the new dimensions.
  let resize (width: int) (height: int) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let msgs =
      [ for sub in app.Program.Subscribe app.Model do
          match sub with
          | ResizeSub handler -> yield handler (width, height)
          | _ -> () ]
    let app' = applyMsgs app msgs
    { app' with Width = width; Height = height }

  /// Advance virtual time by dt, firing any TimerSub whose interval has elapsed.
  ///
  /// Firing rules:
  /// - If dt > interval, the timer fires multiple times (once per elapsed interval).
  /// - Multiple timers firing at the same virtual time are processed in timer-ID order
  ///   (alphabetical) for determinism.
  /// - Each fired message is processed (and its commands handled) before the next fires.
  ///
  /// Use with Init-time virtual time = TimeSpan.Zero. First fire is at t = interval.
  let advanceTime (dt: TimeSpan) (app: TestApp<'model, 'msg>) : TestApp<'model, 'msg> =
    let newTime = app.VirtualTime + dt
    let subs = app.Program.Subscribe app.Model
    // Collect all (fireTime, id, interval, tick) tuples for timers that fire within [now, newTime]
    let fires =
      [ for sub in subs do
          match sub with
          | TimerSub(id, interval, tick) ->
            let nextFire =
              app.TimerNextFire
              |> Map.tryFind id
              |> Option.defaultWith (fun () -> interval)
            let mutable t = nextFire
            while t <= newTime do
              yield (t, id, interval, tick)
              t <- t + interval
          | _ -> () ]
    // Sort by fire time, then by ID for determinism
    let sorted =
      fires
      |> List.sortWith (fun (t1, id1, _, _) (t2, id2, _, _) ->
        let tc = TimeSpan.Compare(t1, t2)
        if tc <> 0 then tc else String.Compare(id1, id2, StringComparison.Ordinal))
    // Update next-fire map to reflect the last fire time for each timer
    let updatedNextFire =
      sorted
      |> List.fold (fun acc (t, id, interval, _) -> Map.add id (t + interval) acc) app.TimerNextFire
    // Process messages sequentially
    let app' = sorted |> List.fold (fun app (_, _, _, tick) -> applyMsgs app [ tick () ]) app
    { app' with VirtualTime = newTime; TimerNextFire = updatedNextFire }

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
