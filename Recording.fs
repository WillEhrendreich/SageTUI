namespace SageTUI

open System
open System.Text.Json

/// Session recording and replay for SageTUI apps.
///
/// Recording captures terminal input events (and optionally rendered frames)
/// to a JSONL file. Replay feeds those events back deterministically — enabling
/// regression testing, demo playback, and asciinema-compatible cast files.
///
/// Usage via environment variables (zero code changes):
///   SAGETUI_RECORD=session.jsonl   → record input events
///   SAGETUI_RECORD=session.cast    → record asciinema v2 format
///   SAGETUI_REPLAY=session.jsonl   → replay from recording
module Recording =

  // ── Serialization ─────────────────────────────────────────────────────────

  [<Struct>]
  type RecordLine = { T: string; D: string; Ms: int64 }

  let private opts = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

  let serialize (line: RecordLine) : string = JsonSerializer.Serialize(line, opts)
  let deserialize (json: string) : RecordLine = JsonSerializer.Deserialize<RecordLine>(json, opts)

  let readAllLines (path: string) : RecordLine list =
    match IO.File.Exists(path) with
    | false -> []
    | true ->
      use fs = new IO.FileStream(path, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
      use sr = new IO.StreamReader(fs, Text.Encoding.UTF8)
      [ while not sr.EndOfStream do
          let line = sr.ReadLine()
          match line.Trim() with
          | "" -> ()
          | l  -> yield deserialize l ]

  // ── Key encoding (human-readable, roundtrippable) ─────────────────────────

  let encodeKey (k: Key) : string =
    match k with
    | Char r    -> sprintf "char:%d" r.Value
    | Enter     -> "enter"
    | Escape    -> "escape"
    | Backspace -> "backspace"
    | Tab       -> "tab"
    | Up        -> "up"    | Down    -> "down"  | Left -> "left" | Right -> "right"
    | Home      -> "home"  | End     -> "end"   | PageUp -> "pgup" | PageDown -> "pgdn"
    | Insert    -> "ins"   | Delete  -> "del"
    | F n       -> sprintf "f%d" n

  let decodeKey (s: string) : Key option =
    match s with
    | "enter"     -> Some Enter
    | "escape"    -> Some Escape
    | "backspace" -> Some Backspace
    | "tab"       -> Some Tab
    | "up"        -> Some Up     | "down"  -> Some Down    | "left"  -> Some Left  | "right" -> Some Right
    | "home"      -> Some Home   | "end"   -> Some End     | "pgup"  -> Some PageUp | "pgdn" -> Some PageDown
    | "ins"       -> Some Insert | "del"   -> Some Delete
    | s when s.StartsWith("char:") ->
      match Int32.TryParse(s.[5..]) with
      | true, n when Text.Rune.IsValid(n) -> Some (Key.Char (Text.Rune n))
      | _ -> None
    | s when s.Length >= 2 && s.[0] = 'f' ->
      match Int32.TryParse(s.[1..]) with
      | true, n -> Some (F n)
      | _ -> None
    | _ -> None

  // ── TerminalEvent encoding ─────────────────────────────────────────────────

  let private encodeButton (b: MouseButton) =
    match b with
    | LeftButton   -> "L"
    | RightButton  -> "R"
    | MiddleButton -> "M"
    | ScrollUp     -> "SU"
    | ScrollDown   -> "SD"

  let private decodeButton (s: string) =
    match s with
    | "L"  -> Some LeftButton
    | "R"  -> Some RightButton
    | "M"  -> Some MiddleButton
    | "SU" -> Some ScrollUp
    | "SD" -> Some ScrollDown
    | _    -> None

  let private encodePhase (p: MousePhase) =
    match p with
    | Pressed  -> "P"
    | Released -> "R"
    | Motion   -> "M"

  let private decodePhase (s: string) =
    match s with
    | "P" -> Some Pressed
    | "R" -> Some Released
    | "M" -> Some Motion
    | _   -> None

  let encodeEvent (ms: int64) (event: TerminalEvent) : RecordLine =
    match event with
    | KeyPressed(key, mods) ->
      { T = "key"; D = sprintf "%s,%d" (encodeKey key) (int mods); Ms = ms }
    | Resized(w, h) ->
      { T = "resize"; D = sprintf "%dx%d" w h; Ms = ms }
    | MouseInput me ->
      { T = "mouse"
        D = sprintf "%s:%d:%d:%d:%s" (encodeButton me.Button) me.X me.Y (int me.Modifiers) (encodePhase me.Phase)
        Ms = ms }
    | FocusGained -> { T = "focus"; D = "gained"; Ms = ms }
    | FocusLost   -> { T = "focus"; D = "lost";   Ms = ms }
    | Pasted text -> { T = "paste"; D = text;      Ms = ms }

  let decodeEvent (line: RecordLine) : TerminalEvent option =
    match line.T with
    | "key" ->
      let parts = line.D.Split(',')
      match parts with
      | [| keyStr; modsStr |] ->
        match decodeKey keyStr, System.Int32.TryParse modsStr with
        | Some k, (true, m) -> Some (KeyPressed(k, enum<Modifiers> m))
        | _ -> None
      | _ -> None
    | "resize" ->
      let parts = line.D.Split('x')
      match parts with
      | [| ws; hs |] ->
        match System.Int32.TryParse ws, System.Int32.TryParse hs with
        | (true, w), (true, h) -> Some (Resized(w, h))
        | _ -> None
      | _ -> None
    | "mouse" ->
      let parts = line.D.Split(':')
      match parts with
      | [| btnStr; xStr; yStr; modsStr; phaseStr |] ->
        match decodeButton btnStr, System.Int32.TryParse xStr, System.Int32.TryParse yStr,
              System.Int32.TryParse modsStr, decodePhase phaseStr with
        | Some btn, (true, x), (true, y), (true, m), Some phase ->
          Some (MouseInput { Button = btn; X = x; Y = y; Modifiers = enum<Modifiers> m; Phase = phase })
        | _ -> None
      | _ -> None
    | "focus" ->
      match line.D with
      | "gained" -> Some FocusGained
      | "lost"   -> Some FocusLost
      | _ -> None
    | "paste" -> Some (Pasted line.D)
    | _ -> None

  // ── Asciinema v2 cast format ──────────────────────────────────────────────

  /// Write an asciinema v2 cast file from a list of (secondsFloat, ansiOutput) frames.
  let writeAsciinema (path: string) (width: int) (height: int) (title: string) (frames: (float * string) list) =
    use sw = new IO.StreamWriter(path, append = false, encoding = Text.Encoding.UTF8)
    let ts = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
    let header = sprintf """{"version":2,"width":%d,"height":%d,"timestamp":%d,"title":"%s"}""" width height ts title
    sw.WriteLine(header)
    for (t, data) in frames do
      let escaped = data.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r")
      sw.WriteLine(sprintf """[%.6f,"o","%s"]""" t escaped)
    sw.Flush()

  // ── Recording backend ──────────────────────────────────────────────────────

  type RecordMode = InputOnly | FullCapture

  /// Wraps a TerminalBackend to record all events to a JSONL file.
  /// The returned backend is a drop-in replacement — all calls are forwarded.
  let wrapRecording (mode: RecordMode) (path: string) (inner: TerminalBackend) : TerminalBackend =
    let startTicks = DateTime.UtcNow.Ticks
    let writer = new IO.StreamWriter(path, append = false, encoding = Text.Encoding.UTF8)
    let castFrames = System.Collections.Generic.List<float * string>()
    let startMs = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
    let w, h = inner.Size()

    let elapsedMs () =
      (DateTime.UtcNow.Ticks - startTicks) / TimeSpan.TicksPerMillisecond

    { inner with
        PollEvent = fun timeout ->
          let evt = inner.PollEvent timeout
          evt |> Option.iter (fun e ->
            let ms = elapsedMs ()
            writer.WriteLine(serialize (encodeEvent ms e))
            writer.Flush())
          evt
        Write = fun output ->
          inner.Write output
          match mode with
          | FullCapture ->
            let t = float (DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() - startMs) / 1000.0
            castFrames.Add((t, output))
          | InputOnly -> ()
        Flush = fun () ->
          inner.Flush()
          match mode with
          | FullCapture ->
            writeAsciinema (IO.Path.ChangeExtension(path, ".cast")) w h "SageTUI recording" (castFrames |> Seq.toList)
          | InputOnly -> () }

  // ── Replay backend ─────────────────────────────────────────────────────────

  type ReplaySpeed = Instant | Realtime | Factor of float

  /// Creates a TerminalBackend that replays recorded events from a JSONL file.
  /// Events are decoded and queued; PollEvent delivers them in order.
  /// The size is taken from the first Resize event, or defaults to (80, 24).
  let replayBackend (speed: ReplaySpeed) (path: string) (inner: TerminalBackend) : TerminalBackend =
    let lines = readAllLines path
    let events =
      lines
      |> List.choose (fun line ->
        match decodeEvent line with
        | Some evt -> Some (line.Ms, evt)
        | None -> None)
    let queue = System.Collections.Generic.Queue(events)
    let startMs = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

    let waitUntil (targetMs: int64) =
      match speed with
      | Instant -> ()
      | Realtime ->
        let elapsed = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() - startMs
        let remaining = targetMs - elapsed
        match remaining > 0L with
        | true -> Threading.Thread.Sleep(int remaining)
        | false -> ()
      | Factor f ->
        let elapsed = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() - startMs
        let scaledTarget = int64 (float targetMs / f)
        let remaining = scaledTarget - elapsed
        match remaining > 0L with
        | true -> Threading.Thread.Sleep(int remaining)
        | false -> ()

    { inner with
        PollEvent = fun _ ->
          match queue.Count > 0 with
          | false -> None
          | true ->
            let (ms, evt) = queue.Dequeue()
            waitUntil ms
            Some evt }

/// Extensions on App for recording/replay.
module AppRecording =

  /// Wrap the given program's run to record events.
  /// Activated automatically when SAGETUI_RECORD env var is set.
  let runRecording (recordPath: string) (program: Program<'model, 'msg>) =
    let mode =
      match IO.Path.GetExtension(recordPath).ToLowerInvariant() with
      | ".cast" -> Recording.FullCapture
      | _       -> Recording.InputOnly
    let inner = Backend.auto ()
    let recordingBackend = Recording.wrapRecording mode recordPath inner
    App.runWith AppConfig.defaults recordingBackend program

  /// Replay a previously recorded session, feeding its events into the program.
  let runReplay (replayPath: string) (speed: Recording.ReplaySpeed) (program: Program<'model, 'msg>) =
    let inner = Backend.auto ()
    let replayBackend = Recording.replayBackend speed replayPath inner
    App.runWith AppConfig.defaults replayBackend program

  /// Run a program, auto-activating recording or replay based on environment variables:
  ///   SAGETUI_RECORD=path  → record to path (.jsonl = input-only, .cast = full asciinema)
  ///   SAGETUI_REPLAY=path  → replay from path
  let run (program: Program<'model, 'msg>) =
    let recordPath = Environment.GetEnvironmentVariable("SAGETUI_RECORD") |> Option.ofObj
    let replayPath = Environment.GetEnvironmentVariable("SAGETUI_REPLAY") |> Option.ofObj
    match recordPath, replayPath with
    | Some rp, _ -> runRecording rp program
    | _, Some rp -> runReplay rp Recording.Instant program
    | None, None -> App.run program
