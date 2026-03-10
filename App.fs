namespace SageTUI

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Threading

type AppConfig =
  { ArenaNodes: int
    ArenaChars: int
    ArenaLayout: int
    /// Maximum number of messages drained in a single frame's Update loop.
    /// Raising this allows bursty message chains; lowering it tightens loop-detection.
    /// Default: 10,000. Infinite self-dispatch cycles always trigger the guard.
    MaxDrainMessages: int
    /// Called when an async command or subscription encounters an unhandled exception.
    /// Receives a pre-formatted log line with timestamp, message, and stack trace.
    /// Defaults to appending to "sagetui-errors.log" in the working directory.
    /// Override to integrate with your application's structured logging infrastructure.
    LogSink: string -> unit
    /// When true, wraps every rendered Element tree with El.debugLayout before rendering.
    /// Draws depth-cycling colored borders (Cyan/Magenta/Yellow/Green/Red/Blue) around
    /// every layout node. Use during development to visualise how layout constraints nest.
    /// Default: false.
    DebugLayout: bool }

module AppConfig =
  let private defaultLogSink (msg: string) =
    try System.IO.File.AppendAllText("sagetui-errors.log", msg) with _ -> ()

  let defaults =
    { ArenaNodes = 4096
      ArenaChars = 65536
      ArenaLayout = 4096
      MaxDrainMessages = 10_000
      LogSink = defaultLogSink
      DebugLayout = false }

module App =
  let private isTruthyEnvVar (value: string option) =
    match value with
    | Some text ->
      match text.Trim().ToLowerInvariant() with
      | "1"
      | "true"
      | "yes"
      | "on" -> true
      | _ -> false
    | None -> false

  type AutomationSettings =
    { UseAltScreen: bool
      UseRawMode: bool }

  let automationSettings (envReader: string -> string option) =
    { UseAltScreen =
        envReader "SAGETUI_DISABLE_ALT_SCREEN"
        |> isTruthyEnvVar
        |> not
      UseRawMode =
        envReader "SAGETUI_DISABLE_RAW_MODE"
        |> isTruthyEnvVar
        |> not }

  /// Drain all queued messages for one frame, calling Update for each.
  /// Returns the updated model and a flag indicating whether any message was processed.
  /// Both runWith and runInlineWith share this single implementation.
  let private drainMessages
    (msgChannel  : System.Collections.Concurrent.ConcurrentQueue<'msg>)
    (program     : Program<'model, 'msg>)
    (maxDrain    : int)
    (interpretCmd: Cmd<'msg> -> unit)
    (dispatch    : 'msg -> unit)
    (model       : 'model)
    : 'model * bool =
    let mutable m       = model
    let mutable changed = false
    let mutable count   = 0
    let mutable msg     = Unchecked.defaultof<'msg>
    let mutable stop    = false
    while not stop && msgChannel.TryDequeue(&msg) do
      count <- count + 1
      if count > maxDrain then
        // Route overflow through OnError instead of hard-crashing — correctness invariant.
        // Clear remaining messages first so the next frame starts clean.
        msgChannel.Clear()
        let drainEx =
          System.InvalidOperationException(
            sprintf "[SageTUI] Message drain loop exceeded %d messages in one frame. \
                     Possible Cmd.ofMsg cycle detected. Check your Update function for \
                     infinite message chains." maxDrain)
        match program.OnError with
        | RecoverWith handler ->
          match handler drainEx with
          | Some recoveryMsg -> dispatch recoveryMsg
          | None             -> ()
        | LogAndContinue -> ()
        | CrashOnError -> raise drainEx
        stop <- true
      else
        try
          let nm, cmd = program.Update msg m
          m       <- nm
          changed <- true
          interpretCmd cmd
        with ex ->
          match program.OnError with
          | RecoverWith handler ->
            // RecoverWith: Some recoveryMsg → dispatch recovery; None → absorb silently.
            match handler ex with
            | Some recoveryMsg -> dispatch recoveryMsg
            | None             -> ()
          | LogAndContinue -> ()
          | CrashOnError -> reraise()
    m, changed

  let runWith (config: AppConfig) (backend: TerminalBackend) (program: Program<'model, 'msg>) =
    let mutable width, height = backend.Size()
    // Mutable program reference enables live-reload: caller may swap the program record
    // (Init/Update/View/Subscribe) at runtime without restarting the loop.
    let mutable program = program
    let mutable model, initCmd = program.Init()
    let mutable frontBuf = Buffer.create width height
    let mutable backBuf = Buffer.create width height
    let arena = FrameArena.create config.ArenaNodes config.ArenaChars config.ArenaLayout
    let mutable running = true
    let mutable needsFullRedraw = true
    let mutable prevKeyedElements = Map.empty<string, Element>
    let mutable prevKeyAreas = Map.empty<string, Area>
    let mutable activeTransitions: ActiveTransition list = []
    let mutable exitCode = 0
    // Track mouse tracking escape state.
    // mouseTrackingActive: ?1000h+?1006h — click/move events (MouseSub, ClickSub)
    // dragTrackingActive:  ?1002h+?1006h — button-motion events (DragSub)
    // pasteTrackingActive: ?2004h        — bracketed paste wrapping (PasteSub)
    let mutable mouseTrackingActive = false
    let mutable dragTrackingActive = false
    let mutable pasteTrackingActive = false
    let frameSw = System.Diagnostics.Stopwatch()

    let msgChannel = ConcurrentQueue<'msg>()
    let dispatch msg = msgChannel.Enqueue(msg)

    let activeSubs = Dictionary<string, CancellationTokenSource>()

    let rec interpretCmd (cmd: Cmd<'msg>) =
      match cmd with
      | NoCmd -> ()
      | Batch cmds -> cmds |> List.iter interpretCmd
      | OfAsync run ->
        async {
          try do! run dispatch
          with ex ->
            let msg = sprintf "[SageTUI] OfAsync error at %s: %s\n%s\n" (DateTime.UtcNow.ToString("u")) ex.Message ex.StackTrace
            config.LogSink msg
        } |> Async.Start
      | OfCancellableAsync(id, run) ->
        match activeSubs.TryGetValue(id) with
        | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
        | _ -> ()
        let cts = new CancellationTokenSource()
        activeSubs.[id] <- cts
        Async.Start(async {
          try do! run cts.Token dispatch
          with :? OperationCanceledException -> ()
        }, cts.Token)
      | CancelSub id ->
        match activeSubs.TryGetValue(id) with
        | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
        | _ -> ()
      | Delay(ms, msg) ->
        async {
          do! Async.Sleep ms
          dispatch msg
        } |> Async.Start
      | DirectMsg msg -> dispatch msg
      | TerminalOutput s -> backend.Write s
      | Quit code ->
        for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
        activeSubs.Clear()
        exitCode <- code
        running <- false

    let fullScreenArea () =
      { X = 0
        Y = 0
        Width = width
        Height = height }

    let reconcileSubs (currentSubs: Sub<'msg> list) =
      // Only TimerSub and CustomSub need lifecycle management (CancellationTokenSource).
      // Event-driven subs (KeySub, MouseSub, ClickSub, FocusSub, ResizeSub) need no
      // lifecycle tracking here — they are matched against events in processEvent below.
      // Warn on duplicate IDs — the second sub silently won't start (the ContainsKey guard below).

      // Auto-enable any-click tracking (?1000h+?1006h) when MouseSub or ClickSub is registered.
      // Auto-disable when the last one is removed.
      let hasMouseSub = currentSubs |> List.exists (function MouseSub _ | ClickSub _ -> true | _ -> false)
      match hasMouseSub, mouseTrackingActive with
      | true, false ->
        backend.Write Ansi.enableMouseTracking
        mouseTrackingActive <- true
      | false, true ->
        backend.Write Ansi.disableMouseTracking
        mouseTrackingActive <- false
      | _ -> ()
      // Auto-enable button-event mouse tracking (?1002h) when a DragSub is registered.
      // Auto-disable when the last DragSub is removed. This lets apps opt in to drag
      // tracking simply by returning a DragSub — no manual terminal escape needed.
      let hasDragSub = currentSubs |> List.exists (function DragSub _ -> true | _ -> false)
      match hasDragSub, dragTrackingActive with
      | true, false ->
        backend.Write Ansi.enableButtonTracking
        dragTrackingActive <- true
      | false, true ->
        backend.Write Ansi.disableButtonTracking
        dragTrackingActive <- false
      | _ -> ()
      // Auto-enable bracketed paste mode (?2004h) when a PasteSub is registered.
      let hasPasteSub = currentSubs |> List.exists (function PasteSub _ -> true | _ -> false)
      match hasPasteSub, pasteTrackingActive with
      | true, false ->
        backend.Write Ansi.enableBracketedPaste
        pasteTrackingActive <- true
      | false, true ->
        backend.Write Ansi.disableBracketedPaste
        pasteTrackingActive <- false
      | _ -> ()
      let ids =
        currentSubs
        |> List.choose (function
          | TimerSub(id, _, _) -> Some id
          | CustomSub(id, _) -> Some id
          | _ -> None)
      let duplicates = ids |> List.groupBy id |> List.choose (fun (k, vs) -> if vs.Length > 1 then Some k else None)
      for dupId in duplicates do
        eprintfn "[SageTUI] Warning: duplicate subscription ID '%s' — only the first will run." dupId
      let currentIds = ids |> Set.ofList
      for KeyValue(id, cts) in activeSubs |> Seq.toList do
        match Set.contains id currentIds with
        | true -> ()
        | false -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
      for sub in currentSubs do
        match sub with
        | TimerSub(id, interval, tick) ->
          match activeSubs.ContainsKey(id) with
          | true -> ()
          | false ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(async {
              while not cts.Token.IsCancellationRequested do
                do! Async.Sleep (int interval.TotalMilliseconds)
                dispatch (tick())
            }, cts.Token)
        | CustomSub(id, start) ->
          match activeSubs.ContainsKey(id) with
          | true -> ()
          | false ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(start dispatch cts.Token, cts.Token)
        | _ -> ()

    let automation =
      automationSettings (fun name ->
        System.Environment.GetEnvironmentVariable(name)
        |> Option.ofObj)

    match automation.UseRawMode with
    | true -> backend.EnterRawMode()
    | false -> ()
    match automation.UseAltScreen with
    | true -> backend.Write(Ansi.enterAltScreen + Ansi.hideCursor)
    | false -> backend.Write(Ansi.hideCursor)

    // Support automated recording: SAGETUI_EXIT_AFTER_MS=N quits after N ms.
    let exitAfterMs =
      System.Environment.GetEnvironmentVariable("SAGETUI_EXIT_AFTER_MS")
      |> Option.ofObj
      |> Option.bind (fun s -> match System.Int32.TryParse(s) with true, n -> Some n | _ -> None)
    exitAfterMs |> Option.iter (fun ms ->
      async {
        do! Async.Sleep ms
        for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
        activeSubs.Clear()
        running <- false
      } |> Async.Start
    )

    // Register belt-and-suspenders cleanup: if an async exception escapes to the
    // thread pool's unhandled handler, restore terminal state before the process dies.
    System.AppDomain.CurrentDomain.UnhandledException.AddHandler(fun _ _ ->
      try
        match automation.UseAltScreen with
        | true -> backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
        | false -> backend.Write(Ansi.showCursor)
        backend.Flush()
        match automation.UseRawMode with
        | true -> backend.LeaveRawMode()
        | false -> ()
      with _ -> ())

    // Ctrl-C handler: cancel subscriptions, restore terminal, exit with code 130 (SIGINT convention).
    // args.Cancel=true prevents immediate process kill so cleanup runs first.
    let cleanup () =
      try
        for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
        activeSubs.Clear()
        running <- false
        match mouseTrackingActive with
        | true -> backend.Write Ansi.disableMouseTracking; mouseTrackingActive <- false
        | false -> ()
        match dragTrackingActive with
        | true -> backend.Write Ansi.disableButtonTracking; dragTrackingActive <- false
        | false -> ()
        match pasteTrackingActive with
        | true -> backend.Write Ansi.disableBracketedPaste; pasteTrackingActive <- false
        | false -> ()
        match automation.UseAltScreen with
        | true -> backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
        | false -> backend.Write(Ansi.showCursor)
        backend.Flush()
        match automation.UseRawMode with
        | true -> backend.LeaveRawMode()
        | false -> ()
      with _ -> ()
    let cancelHandler =
      System.ConsoleCancelEventHandler(fun _ args ->
        args.Cancel <- true
        cleanup()
        System.Environment.Exit(130))
    Console.CancelKeyPress.AddHandler(cancelHandler)

    // POSIX suspend/resume (SIGTSTP / SIGCONT): restore the terminal before the
    // process is stopped, then re-enter raw/alt-screen when it resumes.
    // Windows does not have SIGTSTP so we guard on the OS platform.
    let _sigHandlers =
      match RuntimeInformation.IsOSPlatform(OSPlatform.Windows) with
      | true -> []
      | false ->
        let tstp =
          PosixSignalRegistration.Create(PosixSignal.SIGTSTP, fun _ ->
            try
              match automation.UseAltScreen with
              | true -> backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
              | false -> backend.Write(Ansi.showCursor)
              backend.Flush()
              match automation.UseRawMode with
              | true -> backend.LeaveRawMode()
              | false -> ()
            with _ -> ()
          )
        let cont =
          PosixSignalRegistration.Create(PosixSignal.SIGCONT, fun _ ->
            try
              match automation.UseRawMode with
              | true -> backend.EnterRawMode()
              | false -> ()
              match automation.UseAltScreen with
              | true -> backend.Write(Ansi.enterAltScreen + Ansi.hideCursor)
              | false -> backend.Write(Ansi.hideCursor)
              backend.Flush()
              needsFullRedraw <- true
            with _ -> ()
          )
        // SIGTERM: graceful shutdown on `docker stop`, `systemd stop`, `kill <pid>`.
        // Exit code 143 = 128 + SIGTERM(15), POSIX convention.
        let term =
          PosixSignalRegistration.Create(PosixSignal.SIGTERM, fun ctx ->
            ctx.Cancel <- true   // prevent default kill so cleanup() runs first
            cleanup()
            System.Environment.Exit(143))
        [ tstp; cont; term ]

    try
      interpretCmd initCmd
      let mutable subs = program.Subscribe model
      reconcileSubs subs
      // Pre-allocate hot-path buffers once. Each is cleared and reused every frame.
      // This eliminates the per-frame heap allocations that would trigger GC gen0.
      let diffChanges = ResizeArray<int>(256)
      let presentBuf = System.Text.StringBuilder(65536)
      // Input burst coalescing: pre-allocate collections used in the event polling loop.
      let burstEvents = System.Collections.Generic.List<TerminalEvent>(32)
      let lastMotionIdx = System.Collections.Generic.Dictionary<MouseButton, int>(8)
      let suppressedEvents = System.Collections.Generic.HashSet<int>(32)

      while running do
        let model', modelChanged = drainMessages msgChannel program config.MaxDrainMessages interpretCmd dispatch model
        model <- model'

        match modelChanged with
        | true ->
          subs <- program.Subscribe model
          reconcileSubs subs
        | false -> ()

        // Skip the full render pipeline when the model is unchanged, no transitions are active,
        // and no full-redraw was requested. This eliminates wasted CPU on idle frames (e.g., apps
        // waiting for input) where nothing has changed since the last rendered frame.
        let shouldRender = modelChanged || not (List.isEmpty activeTransitions) || needsFullRedraw

        match shouldRender with
        | false -> ()
        | true ->

          let rawElem = program.View model
          let elem = match config.DebugLayout with true -> El.debugLayout rawElem | false -> rawElem
  
          // Reconcile keyed elements for transitions. Exits use the previous frame's
          // keyed areas; enters use the current frame's keyed areas after rendering.
          let nowMs = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
          let newKeyed = Reconcile.findKeyedElements elem
          let (entering, exiting) = Reconcile.reconcile prevKeyedElements newKeyed
  
          // Capture snapshots for exiting elements and start exit transitions
          // hasDissolve: true if the transition (possibly nested in Sequence) contains a Dissolve.
          // When true, DissolvePayload is pre-computed once here to avoid per-frame allocation.
          let rec hasDissolve trans =
            match trans with
            | Dissolve _ -> true
            | Sequence ts -> ts |> List.exists hasDissolve
            | _ -> false
          let computePayload (key: string) (transition: Transition) (area: Area) =
            match hasDissolve transition with
            | true  -> DissolvePayload (TransitionFx.fisherYatesShuffle (key.GetHashCode()) (area.Width * area.Height))
            | false -> NoPayload
          for (key, oldElem) in exiting do
            match oldElem with
            | Keyed(_, _, exitTransition, _) ->
              let area = prevKeyAreas |> Map.tryFind key |> Option.defaultValue (fullScreenArea ())
              let snapshot = TransitionFx.extractArea area frontBuf
              activeTransitions <-
                { Key = key
                  Transition = exitTransition
                  StartMs = nowMs
                  DurationMs = TransitionDuration.get exitTransition
                  Easing = Ease.cubicInOut
                  SnapshotBefore = snapshot
                  Area = area
                  Payload = computePayload key exitTransition area
                  PhaseCaptures = Map.empty }
                :: activeTransitions
            | _ -> ()
  
          prevKeyedElements <- newKeyed
  
          FrameArena.reset arena
          let rootHandle = Arena.lower arena elem
  
          match needsFullRedraw with
          | true ->
            frontBuf <- Buffer.create width height
            backBuf <- Buffer.create width height
            backend.Write(Ansi.clearScreen)
            backend.Flush()
            needsFullRedraw <- false
          | false -> ()
  
          Buffer.clear backBuf
          let area = { X = 0; Y = 0; Width = width; Height = height }
          frameSw.Restart()
          ArenaRender.renderRoot arena rootHandle area backBuf
          let renderMs = frameSw.Elapsed.TotalMilliseconds
          // Rebuild keyed-area map every frame that has keyed elements.
          // We CANNOT gate this on 'transitions active' — a 'staying' element (present
          // in both prev and current frames) may reposition silently with no entering/exiting
          // fires. When it later exits, App.run uses prevKeyAreas for the exit-transition Area.
          // If prevKeyAreas was never updated during the staying frames, the exit transition
          // fires at the stale entry position. Gating on HitMap.Count=0 correctly skips the
          // Map allocation only for apps with zero El.keyed elements.
          let currentKeyAreas =
            match arena.HitMap.Count > 0 with
            | true -> ArenaRender.keyAreas arena
            | false -> prevKeyAreas
  
          // Start enter transitions once we know the rendered keyed areas.
          for (key, newElem) in entering do
            match newElem with
            | Keyed(_, enterTransition, _, _) ->
              let transArea = currentKeyAreas |> Map.tryFind key |> Option.defaultValue (fullScreenArea ())
              activeTransitions <-
                { Key = key
                  Transition = enterTransition
                  StartMs = nowMs
                  DurationMs = TransitionDuration.get enterTransition
                  Easing = Ease.cubicInOut
                  SnapshotBefore = Array.create (transArea.Width * transArea.Height) PackedCell.empty
                  Area = transArea
                  Payload = computePayload key enterTransition transArea
                  PhaseCaptures = Map.empty }
                :: activeTransitions
            | _ -> ()
  
          // Apply active transitions via a recursive dispatcher so Sequence can recurse.
          // Each Transition case is explicit — compiler warns when a new case is added.
          let rec applyTransition (t: float) (transition: Transition) (at: ActiveTransition) =
            let currentSlice = TransitionFx.extractArea at.Area backBuf
            match transition with
            | Fade _ ->
              TransitionFx.applyFade t currentSlice at.Area backBuf
            | ColorMorph _ ->
              TransitionFx.applyColorMorph t at.SnapshotBefore currentSlice at.Area backBuf
            | Wipe(dir, _) ->
              TransitionFx.applyWipe t dir at.SnapshotBefore currentSlice at.Area backBuf
            | Dissolve _ ->
              // DissolvePayload pre-computed at transition start (via computePayload).
              // Also covers Dissolve nested inside Sequence — computePayload recurses via hasDissolve.
              match at.Payload with
              | DissolvePayload order ->
                TransitionFx.applyDissolve t order at.SnapshotBefore currentSlice at.Area backBuf
              | NoPayload ->
                failwithf "Dissolve transition '%s' has NoPayload — shuffle order must be pre-computed at transition start" at.Key
            | SlideIn(dir, _) ->
              TransitionFx.applySlideIn t dir at.SnapshotBefore currentSlice at.Area backBuf
            | Grow _ ->
              TransitionFx.applyGrow t at.SnapshotBefore currentSlice at.Area backBuf
            | Sequence ts ->
              // Play sub-transitions sequentially. Each phase starts from a snapshot of the
              // buffer at the END of the previous phase, enabling true visual chaining
              // (e.g., Fade → SlideIn slides from the faded state, not the original snapshot).
              let totalMs = TransitionDuration.get (Sequence ts)
              match totalMs with
              | 0 -> ()
              | _ ->
                let elapsedMs = t * float totalMs
                let (subIdx, localT) = SequencePhase.phaseAt elapsedMs ts
                let sub = ts |> List.item subIdx
                // Use per-phase snapshot for phases after the first.
                // On first frame of a new phase: capture current backBuf (shows end-of-prev-phase rendering).
                let phaseAt =
                  match subIdx with
                  | 0 -> at  // phase 0 uses the original SnapshotBefore
                  | _ ->
                    match at.PhaseCaptures |> Map.tryFind subIdx with
                    | Some snap -> { at with SnapshotBefore = snap }
                    | None ->
                      // First frame of this phase — capture the current area state (area-sliced)
                      let snap = TransitionFx.extractArea at.Area backBuf
                      at.PhaseCaptures <- at.PhaseCaptures |> Map.add subIdx snap
                      { at with SnapshotBefore = snap }
                applyTransition localT sub phaseAt
            | Custom(_, f) ->
              TransitionFx.applyCustom t f at.SnapshotBefore currentSlice at.Area backBuf
  
          activeTransitions |> List.iter (fun at ->
            let t = ActiveTransition.progress nowMs at
            applyTransition t at.Transition at)
  
          // Remove completed transitions
          activeTransitions <- activeTransitions |> List.filter (fun at -> not (ActiveTransition.isDone nowMs at))
  
          Buffer.diffInto diffChanges frontBuf backBuf
          let diffMs = frameSw.Elapsed.TotalMilliseconds - renderMs
  
          match diffChanges.Count > 0 with
          | true ->
            let presentStart = frameSw.Elapsed.TotalMilliseconds
            presentBuf.Clear() |> ignore
            Presenter.presentInto presentBuf diffChanges backBuf
            backend.Write(presentBuf.ToString())
            backend.Flush()
            let presentMs = frameSw.Elapsed.TotalMilliseconds - presentStart
            let timings =
              { RenderMs = renderMs
                DiffMs = diffMs
                PresentMs = presentMs
                TotalMs = frameSw.Elapsed.TotalMilliseconds
                ChangedCells = diffChanges.Count }
            for sub in subs do
              match sub with
              | FrameTimingsSub toMsg -> dispatch (toMsg timings)
              | _ -> ()
          | false -> ()
  
          let temp = frontBuf
          frontBuf <- backBuf
          backBuf <- temp
          prevKeyAreas <- currentKeyAreas

        // Drain all available events per frame (burst input, paste)
        let processEvent (event: TerminalEvent) =
          match event with
          | Resized(w, h) ->
            width <- w
            height <- h
            needsFullRedraw <- true
          | _ -> ()
          for sub in subs do
            match sub, event with
            | KeySub handler, KeyPressed(key, mods) ->
              handler (key, mods) |> Option.iter dispatch
            | FocusSub handler, KeyPressed(Key.Tab, mods) ->
              let dir = match mods.HasFlag(Modifiers.Shift) with true -> FocusPrev | false -> FocusNext
              handler dir |> Option.iter dispatch
            // MouseSub: press and release only — Motion events go to DragSub
            | MouseSub handler, MouseInput me when me.Phase <> Motion ->
              handler me |> Option.iter dispatch
            // ClickSub: press only, with hit-test against keyed elements
            | ClickSub handler, MouseInput me when me.Phase = Pressed ->
              let hitKey = ArenaRender.hitTest arena me.X me.Y
              handler (me, hitKey) |> Option.iter dispatch
            // DragSub: motion only — fires when button-event tracking (?1002h) is enabled
            | DragSub handler, MouseInput me when me.Phase = Motion ->
              handler me |> Option.iter dispatch
            // TerminalFocusSub: OS-level focus gained/lost (?1004h)
            | TerminalFocusSub handler, FocusGained -> handler true  |> Option.iter dispatch
            | TerminalFocusSub handler, FocusLost   -> handler false |> Option.iter dispatch
            | PasteSub handler, Pasted text ->
              handler text |> Option.iter dispatch
            | ResizeSub handler, Resized(w, h) ->
              handler (w, h) |> Option.iter dispatch
            | _ -> ()

        match backend.PollEvent 16 with
        | Some firstEvent ->
          // Collect all available events in this burst (pre-allocated list, cleared each poll)
          burstEvents.Clear()
          burstEvents.Add(firstEvent)
          let mutable more = true
          while more do
            match backend.PollEvent 0 with
            | Some next -> burstEvents.Add(next)
            | None -> more <- false
          // Coalesce: for Motion events with the same button, keep only the last one.
          // Use pre-allocated Dictionary and HashSet — clear rather than reallocate.
          lastMotionIdx.Clear()
          suppressedEvents.Clear()
          for i in 0 .. burstEvents.Count - 1 do
            match burstEvents.[i] with
            | MouseInput me when me.Phase = Motion -> lastMotionIdx.[me.Button] <- i
            | _ -> ()
          for i in 0 .. burstEvents.Count - 1 do
            match burstEvents.[i] with
            | MouseInput me when me.Phase = Motion ->
              match lastMotionIdx.TryGetValue(me.Button) with
              | true, last when last <> i -> suppressedEvents.Add(i) |> ignore
              | _ -> ()
            | _ -> ()
          for i in 0 .. burstEvents.Count - 1 do
            match suppressedEvents.Contains(i) with
            | false -> processEvent burstEvents.[i]
            | true -> ()
        | None ->
          Thread.Sleep 1

      match mouseTrackingActive with
      | true -> backend.Write Ansi.disableMouseTracking
      | false -> ()
      match dragTrackingActive with
      | true -> backend.Write Ansi.disableButtonTracking
      | false -> ()
      match pasteTrackingActive with
      | true -> backend.Write Ansi.disableBracketedPaste
      | false -> ()
      match automation.UseAltScreen with
      | true -> backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
      | false -> backend.Write(Ansi.showCursor)
      backend.Flush()
      match automation.UseRawMode with
      | true -> backend.LeaveRawMode()
      | false -> ()
      Console.CancelKeyPress.RemoveHandler(cancelHandler)
      if exitCode <> 0 then
        System.Environment.Exit exitCode
    with ex ->
      // Cancel background subscriptions so timers/custom subs don't outlive the crash.
      for kvp in activeSubs do try kvp.Value.Cancel(); kvp.Value.Dispose() with _ -> ()
      activeSubs.Clear()
      match mouseTrackingActive with
      | true -> backend.Write Ansi.disableMouseTracking
      | false -> ()
      match dragTrackingActive with
      | true -> backend.Write Ansi.disableButtonTracking
      | false -> ()
      match pasteTrackingActive with
      | true -> backend.Write Ansi.disableBracketedPaste
      | false -> ()
      match automation.UseAltScreen with
      | true -> backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
      | false -> backend.Write(Ansi.showCursor)
      backend.Flush()
      match automation.UseRawMode with
      | true -> backend.LeaveRawMode()
      | false -> ()
      Console.CancelKeyPress.RemoveHandler(cancelHandler)
      reraise()

  /// Run with an explicit backend (for testing or custom backends).
  let runWithBackend (backend: TerminalBackend) (program: Program<'model, 'msg>) =
    runWith AppConfig.defaults backend program

  /// Run a program. Auto-detects terminal capabilities. This is the main entry point.
  let run (program: Program<'model, 'msg>) =
    let backend = Backend.auto()
    runWith AppConfig.defaults backend program

  /// Create a Program from init/update/view (no subscriptions).
  let simple
    (init: unit -> 'model * Cmd<'msg>)
    (update: 'msg -> 'model -> 'model * Cmd<'msg>)
    (view: 'model -> Element)
    : Program<'model, 'msg> =
    { Init = init; Update = update; View = view; Subscribe = (fun _ -> []); OnError = CrashOnError }

  /// Display a static element. Press Escape to quit.
  let display (view: unit -> Element) =
    let program : Program<unit, Key> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with Key.Escape -> (), Quit 0 | _ -> (), NoCmd
        View = fun () -> view ()
        Subscribe = fun _ -> [KeySub (fun (k, _) -> Some k)]
        OnError = CrashOnError }
    run program

  /// Run a program inline below the current cursor position (no alt-screen).
  ///
  /// `height` lines are reserved below the cursor. The program renders into that
  /// fixed-height strip. Unlike `App.run`, the terminal is NOT switched to
  /// alternate-screen — existing content above the strip is preserved.
  ///
  /// `clearOnExit` (default true): when true, clears the reserved lines and moves
  /// the cursor to the first reserved line on exit (useful for pickers/menus that
  /// should leave no trace). When false, the final frame content remains visible
  /// below the original cursor position (useful for commands that output a result).
  ///
  /// Transition effects are not applied in inline mode.
  let runInlineWith
    (config: AppConfig)
    (height: int)
    (clearOnExit: bool)
    (backend: TerminalBackend)
    (program: Program<'model, 'msg>) =

    let termW, _ = backend.Size()
    let mutable width = termW
    let inlineHeight = max 1 height

    // Record cursor row before reserving lines, then print blank lines to
    // push the terminal content up and reserve the area.
    let mutable startRow =
      try Console.CursorTop
      with _ -> 0
    let reservedOutput = String.replicate inlineHeight "\n"
    Console.Write(reservedOutput)
    // After printing N newlines the cursor is below the reserved area.
    // Recalculate startRow: if scrolling occurred, CursorTop will reflect it.
    let endRow =
      try Console.CursorTop
      // If CursorTop is unavailable (non-TTY), assume we're at screen bottom.
      with _ -> startRow + inlineHeight
    startRow <- max 0 (endRow - inlineHeight)

    // Mutable program reference enables live-reload: caller may swap the program record at runtime.
    let mutable program = program
    let mutable model, initCmd = program.Init()
    let mutable frontBuf = Buffer.create width inlineHeight
    let mutable backBuf = Buffer.create width inlineHeight
    let arena = FrameArena.create config.ArenaNodes config.ArenaChars config.ArenaLayout
    let mutable running = true
    let mutable needsFullRedraw = true
    let mutable exitCode = 0
    let mutable mouseTrackingActive = false
    let mutable dragTrackingActive = false
    let mutable pasteTrackingActive = false
    let frameSw = System.Diagnostics.Stopwatch()

    let msgChannel = ConcurrentQueue<'msg>()
    let dispatch msg = msgChannel.Enqueue(msg)

    let activeSubs = Dictionary<string, CancellationTokenSource>()

    let rec interpretCmd (cmd: Cmd<'msg>) =
      match cmd with
      | NoCmd -> ()
      | Batch cmds -> cmds |> List.iter interpretCmd
      | OfAsync run ->
        async {
          try do! run dispatch
          with ex ->
            let msg = sprintf "[SageTUI] OfAsync error at %s: %s\n%s\n" (DateTime.UtcNow.ToString("u")) ex.Message ex.StackTrace
            config.LogSink msg
        } |> Async.Start
      | OfCancellableAsync(id, run) ->
        match activeSubs.TryGetValue(id) with
        | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
        | _ -> ()
        let cts = new CancellationTokenSource()
        activeSubs.[id] <- cts
        Async.Start(async {
          try do! run cts.Token dispatch
          with :? OperationCanceledException -> ()
        }, cts.Token)
      | CancelSub id ->
        match activeSubs.TryGetValue(id) with
        | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
        | _ -> ()
      | Delay(ms, msg) ->
        async {
          do! Async.Sleep ms
          dispatch msg
        } |> Async.Start
      | DirectMsg msg -> dispatch msg
      | TerminalOutput s -> backend.Write s
      | Quit code ->
        for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
        activeSubs.Clear()
        exitCode <- code
        running <- false

    let reconcileSubs (currentSubs: Sub<'msg> list) =
      let hasMouseSub = currentSubs |> List.exists (function MouseSub _ | ClickSub _ -> true | _ -> false)
      match hasMouseSub, mouseTrackingActive with
      | true, false -> backend.Write Ansi.enableMouseTracking; mouseTrackingActive <- true
      | false, true -> backend.Write Ansi.disableMouseTracking; mouseTrackingActive <- false
      | _ -> ()
      let hasDragSub = currentSubs |> List.exists (function DragSub _ -> true | _ -> false)
      match hasDragSub, dragTrackingActive with
      | true, false -> backend.Write Ansi.enableButtonTracking; dragTrackingActive <- true
      | false, true -> backend.Write Ansi.disableButtonTracking; dragTrackingActive <- false
      | _ -> ()
      let hasPasteSub = currentSubs |> List.exists (function PasteSub _ -> true | _ -> false)
      match hasPasteSub, pasteTrackingActive with
      | true, false -> backend.Write Ansi.enableBracketedPaste; pasteTrackingActive <- true
      | false, true -> backend.Write Ansi.disableBracketedPaste; pasteTrackingActive <- false
      | _ -> ()
      let ids =
        currentSubs
        |> List.choose (function
          | TimerSub(id, _, _) -> Some id
          | CustomSub(id, _) -> Some id
          | _ -> None)
      let currentIds = ids |> Set.ofList
      for KeyValue(id, cts) in activeSubs |> Seq.toList do
        match Set.contains id currentIds with
        | true -> ()
        | false -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
      for sub in currentSubs do
        match sub with
        | TimerSub(id, interval, tick) ->
          match activeSubs.ContainsKey(id) with
          | true -> ()
          | false ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(async {
              while not cts.Token.IsCancellationRequested do
                do! Async.Sleep (int interval.TotalMilliseconds)
                dispatch (tick())
            }, cts.Token)
        | CustomSub(id, start) ->
          match activeSubs.ContainsKey(id) with
          | true -> ()
          | false ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(start dispatch cts.Token, cts.Token)
        | _ -> ()

    let inline clearInlineArea () =
      let sb = System.Text.StringBuilder()
      for row in 0 .. inlineHeight - 1 do
        sb.Append(Ansi.moveCursor (startRow + row) 0) |> ignore
        sb.Append(Ansi.clearToEol) |> ignore
      backend.Write(sb.ToString())

    let cleanup () =
      for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
      activeSubs.Clear()
      match mouseTrackingActive with
      | true -> backend.Write Ansi.disableMouseTracking; mouseTrackingActive <- false
      | false -> ()
      match dragTrackingActive with
      | true -> backend.Write Ansi.disableButtonTracking; dragTrackingActive <- false
      | false -> ()
      match pasteTrackingActive with
      | true -> backend.Write Ansi.disableBracketedPaste; pasteTrackingActive <- false
      | false -> ()
      match clearOnExit with
      | true ->
        clearInlineArea()
        backend.Write(Ansi.moveCursor startRow 0)
      | false ->
        backend.Write(Ansi.moveCursor (startRow + inlineHeight) 0)
      backend.Write(Ansi.showCursor)
      backend.Flush()
      backend.LeaveRawMode()

    let cancelHandler =
      System.ConsoleCancelEventHandler(fun _ args ->
        args.Cancel <- true   // prevent immediate process termination
        cleanup()
        System.Environment.Exit(130))

    // POSIX suspend/resume + graceful SIGTERM for runInlineWith.
    // runWith has the same registrations; inline mode adds them here.
    let _sigHandlers =
      match RuntimeInformation.IsOSPlatform(OSPlatform.Windows) with
      | true -> []
      | false ->
        let tstp =
          PosixSignalRegistration.Create(PosixSignal.SIGTSTP, fun _ ->
            try
              backend.Write(Ansi.showCursor)
              backend.Flush()
              backend.LeaveRawMode()
            with _ -> ())
        let cont =
          PosixSignalRegistration.Create(PosixSignal.SIGCONT, fun _ ->
            try
              backend.EnterRawMode()
              backend.Write(Ansi.hideCursor)
              backend.Flush()
              needsFullRedraw <- true
            with _ -> ())
        let term =
          PosixSignalRegistration.Create(PosixSignal.SIGTERM, fun ctx ->
            ctx.Cancel <- true
            cleanup()
            System.Environment.Exit(143))
        [ tstp; cont; term ]

    try
      Console.CancelKeyPress.AddHandler(cancelHandler)
      backend.EnterRawMode()
      backend.Write(Ansi.hideCursor)
      interpretCmd initCmd
      let mutable subs = program.Subscribe model
      reconcileSubs subs
      let diffChanges = ResizeArray<int>(256)
      let presentBuf = System.Text.StringBuilder(65536)

      while running do
        let model', modelChanged = drainMessages msgChannel program config.MaxDrainMessages interpretCmd dispatch model
        model <- model'

        match modelChanged with
        | true ->
          subs <- program.Subscribe model
          reconcileSubs subs
        | false -> ()

        let shouldRender = modelChanged || needsFullRedraw

        match shouldRender with
        | false -> ()
        | true ->
          let rawElem = program.View model
          let elem = match config.DebugLayout with true -> El.debugLayout rawElem | false -> rawElem
          FrameArena.reset arena
          let rootHandle = Arena.lower arena elem

          match needsFullRedraw with
          | true ->
            frontBuf <- Buffer.create width inlineHeight
            backBuf <- Buffer.create width inlineHeight
            clearInlineArea()
            needsFullRedraw <- false
          | false -> ()

          Buffer.clear backBuf
          let area = { X = 0; Y = 0; Width = width; Height = inlineHeight }
          frameSw.Restart()
          ArenaRender.renderRoot arena rootHandle area backBuf
          Buffer.diffInto diffChanges frontBuf backBuf
          match diffChanges.Count with
          | 0 -> ()
          | _ ->
            presentBuf.Clear() |> ignore
            Presenter.presentAtInto startRow presentBuf diffChanges backBuf
            backend.Write(presentBuf.ToString())
            backend.Flush()
            Array.blit backBuf.Cells 0 frontBuf.Cells 0 frontBuf.Cells.Length

        // Process terminal events
        let event = backend.PollEvent 16
        match event with
        | Some e ->
          for sub in subs do
            match sub, e with
            | KeySub handler, KeyPressed(k, m) ->
              handler (k, m) |> Option.iter dispatch
            | ResizeSub handler, Resized(w, h) ->
              width <- w
              // Reallocate buffers for new width so cells are never out of bounds.
              // height is ignored — inline area keeps its reserved inlineHeight.
              frontBuf <- Buffer.create w inlineHeight
              backBuf <- Buffer.create w inlineHeight
              needsFullRedraw <- true
              handler (w, h) |> Option.iter dispatch
            | _ -> ()
        | None -> Thread.Sleep 1

      cleanup()
      Console.CancelKeyPress.RemoveHandler(cancelHandler)
      match exitCode with
      | 0 -> ()
      | code -> System.Environment.Exit code
    with ex ->
      Console.CancelKeyPress.RemoveHandler(cancelHandler)
      // Cancel background subscriptions so timers/custom subs don't outlive the crash.
      for kvp in activeSubs do try kvp.Value.Cancel(); kvp.Value.Dispose() with _ -> ()
      activeSubs.Clear()
      match mouseTrackingActive with
      | true -> backend.Write Ansi.disableMouseTracking
      | false -> ()
      match dragTrackingActive with
      | true -> backend.Write Ansi.disableButtonTracking
      | false -> ()
      match pasteTrackingActive with
      | true -> backend.Write Ansi.disableBracketedPaste
      | false -> ()
      backend.Write(Ansi.showCursor)
      backend.Flush()
      backend.LeaveRawMode()
      reraise()

  /// Run a program inline below the current cursor (no alt-screen).
  /// `height` terminal lines are reserved. Clears on exit. Auto-detects backend.
  let runInline (height: int) (program: Program<'model, 'msg>) =
    let backend = Backend.auto()
    runInlineWith AppConfig.defaults height true backend program

  /// Run inline with explicit exit behavior.
  /// `clearOnExit = false` leaves the final frame content visible after exit.
  let runInlinePersist (height: int) (program: Program<'model, 'msg>) =
    let backend = Backend.auto()
    runInlineWith AppConfig.defaults height false backend program

  /// Run an inline program that produces a typed result on exit.
  ///
  /// The program's `Update` function should call `Cmd.quit` or return a result
  /// by storing it in the model. Extract the result by reading the returned model
  /// after the program exits, or use `resultFn` to project from the final model.
  ///
  ///   let result =
  ///     App.runInlineResult 10 (fun m -> m.Selected) program
  ///   // Returns Some selectedItem if the user confirmed, None if they cancelled.
  ///
  /// `cancelMsg` is dispatched when the result function returns None on quit — useful
  /// for signalling cancellation cleanly. Omit for simple pickers that return model state.
  let runInlineResult
    (height: int)
    (resultFn: 'model -> 'result option)
    (program: Program<'model, 'msg>) : 'result option =
    let resultRef = ref None
    let wrapped : Program<'model, 'msg> =
      { program with
          Update = fun msg model ->
            let newModel, cmd = program.Update msg model
            let result = resultFn newModel
            resultRef.Value <- result
            newModel, cmd }
    let backend = Backend.auto()
    runInlineWith AppConfig.defaults height true backend wrapped
    !resultRef
