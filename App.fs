namespace SageTUI

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading

module App =
  let run (backend: TerminalBackend) (program: Program<'model, 'msg>) =
    let width, height = backend.Size()
    let mutable model, initCmd = program.Init()
    let mutable frontBuf = Buffer.create width height
    let mutable backBuf = Buffer.create width height
    let arena = FrameArena.create 4096 65536 4096
    let mutable running = true

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
          with _ -> ()
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

    while running do
      let mutable msg = Unchecked.defaultof<'msg>
      while msgChannel.TryDequeue(&msg) do
        let newModel, cmd = program.Update msg model
        model <- newModel
        interpretCmd cmd

      let subs = program.Subscribe model
      reconcileSubs subs

      let elem = program.View model

      FrameArena.reset arena
      let _rootHandle = Arena.lower arena elem

      Buffer.clear backBuf
      let area = { X = 0; Y = 0; Width = width; Height = height }
      Render.render area Style.empty backBuf elem

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
        for sub in subs do
          match sub, event with
          | KeySub handler, KeyPressed(key, mods) ->
            handler (key, mods) |> Option.iter dispatch
          | ResizeSub handler, Resized(w, h) ->
            handler (w, h) |> dispatch
          | _ -> ()
      | None -> ()

    backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
    backend.LeaveRawMode()
