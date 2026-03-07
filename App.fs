namespace SageTUI

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading

module App =
  let run (backend: TerminalBackend) (program: Program<'model, 'msg>) =
    let mutable width, height = backend.Size()
    let mutable model, initCmd = program.Init()
    let mutable frontBuf = Buffer.create width height
    let mutable backBuf = Buffer.create width height
    let arena = FrameArena.create 4096 65536 4096
    let mutable running = true
    let mutable needsFullRedraw = true
    let mutable prevKeyedElements = Map.empty<string, Element>
    let mutable activeTransitions: ActiveTransition list = []

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
            eprintfn "OfAsync error: %s" ex.Message
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
      | Quit ->
        for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
        activeSubs.Clear()
        running <- false

    let reconcileSubs (currentSubs: Sub<'msg> list) =
      let currentIds =
        currentSubs
        |> List.choose (function
          | TimerSub(id, _, _) -> Some id
          | CustomSub(id, _) -> Some id
          | _ -> None)
        |> Set.ofList
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

    backend.EnterRawMode()
    backend.Write(Ansi.enterAltScreen + Ansi.hideCursor)

    interpretCmd initCmd
    let mutable prevModel = model
    let mutable subs = program.Subscribe model
    reconcileSubs subs

    while running do
      let mutable msg = Unchecked.defaultof<'msg>
      let mutable modelChanged = false
      while msgChannel.TryDequeue(&msg) do
        let newModel, cmd = program.Update msg model
        model <- newModel
        modelChanged <- true
        interpretCmd cmd

      match modelChanged with
      | true ->
        subs <- program.Subscribe model
        reconcileSubs subs
      | false -> ()

      let elem = program.View model

      // Reconcile keyed elements for transitions
      let nowMs = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
      let newKeyed = Reconcile.findKeyedElements elem
      let (entering, exiting, _) = Reconcile.reconcile prevKeyedElements newKeyed

      // Capture snapshots for exiting elements and start exit transitions
      for (key, oldElem) in exiting do
        match oldElem with
        | Keyed(_, _, exitTransition, _) ->
          let snapshot = Array.copy frontBuf.Cells
          activeTransitions <-
            { Key = key
              Transition = exitTransition
              StartMs = nowMs
              DurationMs = TransitionDuration.get exitTransition
              Easing = Ease.cubicInOut
              SnapshotBefore = snapshot
              Area = { X = 0; Y = 0; Width = width; Height = height } }
            :: activeTransitions
        | _ -> ()

      // Start enter transitions
      for (key, newElem) in entering do
        match newElem with
        | Keyed(_, enterTransition, _, _) ->
          activeTransitions <-
            { Key = key
              Transition = enterTransition
              StartMs = nowMs
              DurationMs = TransitionDuration.get enterTransition
              Easing = Ease.cubicInOut
              SnapshotBefore = Array.create (width * height) PackedCell.empty
              Area = { X = 0; Y = 0; Width = width; Height = height } }
            :: activeTransitions
        | _ -> ()

      prevKeyedElements <- newKeyed

      FrameArena.reset arena
      let rootHandle = Arena.lower arena elem

      match needsFullRedraw with
      | true ->
        frontBuf <- Buffer.create width height
        backBuf <- Buffer.create width height
        needsFullRedraw <- false
      | false -> ()

      Buffer.clear backBuf
      let area = { X = 0; Y = 0; Width = width; Height = height }
      ArenaRender.renderRoot arena rootHandle area backBuf

      // Apply active transitions
      activeTransitions |> List.iter (fun at ->
        let t = ActiveTransition.progress nowMs at
        match at.Transition with
        | Fade _ ->
          TransitionFx.applyFade t backBuf.Cells at.Area.Y at.Area.Width at.Area.Height backBuf
        | ColorMorph _ ->
          TransitionFx.applyColorMorph t at.SnapshotBefore backBuf.Cells at.Area.Y at.Area.Width at.Area.Height backBuf
        | Wipe(dir, _) ->
          TransitionFx.applyWipe t dir at.SnapshotBefore backBuf.Cells at.Area.Y at.Area.Width at.Area.Height backBuf
        | Dissolve _ ->
          let order = TransitionFx.fisherYatesShuffle (at.Key.GetHashCode()) (at.Area.Width * at.Area.Height)
          TransitionFx.applyDissolve t order at.SnapshotBefore backBuf.Cells at.Area.Y at.Area.Width at.Area.Height backBuf
        | _ -> ())

      // Remove completed transitions
      activeTransitions <- activeTransitions |> List.filter (fun at -> not (ActiveTransition.isDone nowMs at))

      let changes = Buffer.diff frontBuf backBuf

      match changes.Count > 0 with
      | true ->
        let output = Presenter.present changes backBuf
        backend.Write(output)
        backend.Flush()
      | false -> ()

      let temp = frontBuf
      frontBuf <- backBuf
      backBuf <- temp

      match backend.PollEvent 16 with
      | Some event ->
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
          | ResizeSub handler, Resized(w, h) ->
            handler (w, h) |> dispatch
          | _ -> ()
      | None ->
        Thread.Sleep 1

    backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
    backend.LeaveRawMode()
