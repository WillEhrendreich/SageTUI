module RecordingTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ── Key encode/decode ─────────────────────────────────────────────────────────

let keyRoundtripTests = testList "Key roundtrip" [
  testProperty "all printable ASCII chars roundtrip" <| fun (c: char) ->
    let c = char (int c % 95 + 32) // printable ASCII 32-126
    let k = Key.Char (System.Text.Rune c)
    match Recording.encodeKey k |> Recording.decodeKey with
    | Some (Key.Char back) -> back = System.Text.Rune c
    | _ -> false

  let namedKeys = [
    Key.Enter; Key.Escape; Key.Backspace; Key.Tab
    Key.Up; Key.Down; Key.Left; Key.Right
    Key.Home; Key.End; Key.PageUp; Key.PageDown
    Key.Insert; Key.Delete
  ]
  for k in namedKeys do
    test (sprintf "named key %A roundtrips" k) {
      k |> Recording.encodeKey |> Recording.decodeKey |> Expect.equal "roundtrip" (Some k)
    }

  test "F1 roundtrips" {
    Key.F 1 |> Recording.encodeKey |> Recording.decodeKey |> Expect.equal "F1" (Some (Key.F 1))
  }

  test "F12 roundtrips" {
    Key.F 12 |> Recording.encodeKey |> Recording.decodeKey |> Expect.equal "F12" (Some (Key.F 12))
  }

  test "unknown key string returns None" {
    Recording.decodeKey "xyz123" |> Expect.isNone "unknown"
  }
]

// ── TerminalEvent encode/decode ───────────────────────────────────────────────

let eventRoundtripTests = testList "Event roundtrip" [
  test "KeyPressed roundtrips with no modifiers" {
    let evt = KeyPressed(Key.Enter, Modifiers.None)
    let line = Recording.encodeEvent 100L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "KeyPressed roundtrips with Ctrl modifier" {
    let evt = KeyPressed(Key.Char (System.Text.Rune 'c'), Modifiers.Ctrl)
    let line = Recording.encodeEvent 200L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "Resized roundtrips" {
    let evt = Resized(120, 40)
    let line = Recording.encodeEvent 0L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "FocusGained roundtrips" {
    let evt = FocusGained
    let line = Recording.encodeEvent 0L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "FocusLost roundtrips" {
    let evt = FocusLost
    let line = Recording.encodeEvent 0L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "Pasted roundtrips" {
    let evt = Pasted "hello world"
    let line = Recording.encodeEvent 0L evt
    Recording.decodeEvent line |> Expect.equal "roundtrip" (Some evt)
  }

  test "ms timestamp is stored correctly" {
    let line = Recording.encodeEvent 42L FocusGained
    line.Ms |> Expect.equal "ms" 42L
  }

  test "encodeEvent tag for key is 'key'" {
    let line = Recording.encodeEvent 0L (KeyPressed(Key.Enter, Modifiers.None))
    line.T |> Expect.equal "tag" "key"
  }

  test "encodeEvent tag for resize is 'resize'" {
    let line = Recording.encodeEvent 0L (Resized(80, 24))
    line.T |> Expect.equal "tag" "resize"
  }
]

// ── Serialization roundtrip ───────────────────────────────────────────────────

let serializationTests = testList "JSONL serialization" [
  test "RecordLine serialize/deserialize roundtrips" {
    let line = { Recording.T = "key"; Recording.D = "enter:0"; Recording.Ms = 123L }
    let json = Recording.serialize line
    let back = Recording.deserialize json
    back.T |> Expect.equal "T" "key"
    back.D |> Expect.equal "D" "enter:0"
    back.Ms |> Expect.equal "Ms" 123L
  }

  test "readAllLines reads all valid lines from a JSONL string" {
    let path = System.IO.Path.GetTempFileName()
    use tmp = new System.IO.StreamWriter(path)
    tmp.WriteLine(Recording.serialize { T = "key"; D = "up,0"; Ms = 10L })
    tmp.WriteLine(Recording.serialize { T = "resize"; D = "80x24"; Ms = 20L })
    tmp.Flush()
    let lines = Recording.readAllLines path
    lines |> Expect.hasLength "2 lines" 2
    lines.[0].T |> Expect.equal "first tag" "key"
    lines.[1].T |> Expect.equal "second tag" "resize"
  }

  test "readAllLines returns empty list for nonexistent file" {
    Recording.readAllLines "nonexistent-file-xyz.jsonl" |> Expect.isEmpty "empty"
  }
]

// ── Recording backend ─────────────────────────────────────────────────────────

let recordingBackendTests = testList "Recording backend" [
  test "records key events to JSONL" {
    let path = System.IO.Path.GetTempFileName()
    let events = [ KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None); KeyPressed(Key.Escape, Modifiers.None) ]
    let (inner, _) = TestBackend.create 80 24 events
    let recording = Recording.wrapRecording Recording.InputOnly path inner

    // Consume both events
    recording.PollEvent 0 |> ignore
    recording.PollEvent 0 |> ignore

    let lines = Recording.readAllLines path
    lines |> Expect.hasLength "2 recorded lines" 2
    lines.[0].T |> Expect.equal "first is key" "key"
    lines.[1].T |> Expect.equal "second is key" "key"
  }

  test "records resize events to JSONL" {
    let path = System.IO.Path.GetTempFileName()
    let events = [ Resized(100, 30) ]
    let (inner, _) = TestBackend.create 80 24 events
    let recording = Recording.wrapRecording Recording.InputOnly path inner

    recording.PollEvent 0 |> ignore

    let lines = Recording.readAllLines path
    lines |> Expect.hasLength "1 recorded line" 1
    lines.[0].T |> Expect.equal "resize tag" "resize"
    Recording.decodeEvent lines.[0] |> Expect.equal "decoded" (Some (Resized(100, 30)))
  }

  test "returns None when no events, same as inner" {
    let path = System.IO.Path.GetTempFileName()
    let (inner, _) = TestBackend.create 80 24 []
    let recording = Recording.wrapRecording Recording.InputOnly path inner

    recording.PollEvent 0 |> Expect.isNone "no events"
  }

  test "forwards all events back to caller" {
    let path = System.IO.Path.GetTempFileName()
    let events = [ KeyPressed(Key.Enter, Modifiers.None); Resized(100, 30) ]
    let (inner, _) = TestBackend.create 80 24 events
    let recording = Recording.wrapRecording Recording.InputOnly path inner

    let e1 = recording.PollEvent 0
    let e2 = recording.PollEvent 0
    e1 |> Expect.equal "first event returned" (Some (KeyPressed(Key.Enter, Modifiers.None)))
    e2 |> Expect.equal "second event returned" (Some (Resized(100, 30)))
  }
]

// ── Replay backend ────────────────────────────────────────────────────────────

let replayBackendTests = testList "Replay backend" [
  test "replays events in order" {
    let path = System.IO.Path.GetTempFileName()
    let events = [ KeyPressed(Key.Up, Modifiers.None); KeyPressed(Key.Down, Modifiers.None); Resized(80, 24) ]
    let (inner, _) = TestBackend.create 80 24 events
    let recording = Recording.wrapRecording Recording.InputOnly path inner
    for _ in 1 .. events.Length do recording.PollEvent 0 |> ignore

    let (replayInner, _) = TestBackend.create 80 24 []
    let replay = Recording.replayBackend Recording.Instant path replayInner

    let e1 = replay.PollEvent 0
    let e2 = replay.PollEvent 0
    let e3 = replay.PollEvent 0
    e1 |> Expect.equal "up"     (Some (KeyPressed(Key.Up, Modifiers.None)))
    e2 |> Expect.equal "down"   (Some (KeyPressed(Key.Down, Modifiers.None)))
    e3 |> Expect.equal "resize" (Some (Resized(80, 24)))
  }

  test "replay returns None when all events exhausted" {
    let path = System.IO.Path.GetTempFileName()
    let events = [ KeyPressed(Key.Enter, Modifiers.None) ]
    let (inner, _) = TestBackend.create 80 24 events
    let recording = Recording.wrapRecording Recording.InputOnly path inner
    recording.PollEvent 0 |> ignore

    let (replayInner, _) = TestBackend.create 80 24 []
    let replay = Recording.replayBackend Recording.Instant path replayInner
    replay.PollEvent 0 |> ignore
    replay.PollEvent 0 |> Expect.isNone "exhausted"
  }
]

// ── Integration: record → replay → same output ────────────────────────────────

let integrationTests = testList "Integration" [
  test "record and replay produce identical outputs" {
    // A trivial counter program that shows a number and quits on 'q'
    let program : Program<int, TerminalEvent> =
      { Init    = fun () -> 0, NoCmd
        Update  = fun msg model ->
          match msg with
          | KeyPressed(KeyChar 'q', _) -> model, Quit 0
          | KeyPressed(Key.Up, _)        -> model + 1, NoCmd
          | KeyPressed(Key.Down, _)      -> model - 1, NoCmd
          | _                            -> model, NoCmd
        View    = fun model -> El.text (sprintf "Count: %d" model)
        Subscribe = fun _ -> [KeySub (fun (k, m) -> Some (KeyPressed(k, m)))]
        OnError = None }

    let inputEvents = [
      KeyPressed(Key.Up,      Modifiers.None)
      KeyPressed(Key.Up,      Modifiers.None)
      KeyPressed(Key.Down,    Modifiers.None)
      KeyPressed(Key.Char (System.Text.Rune 'q'), Modifiers.None)
    ]

    // Record
    let recordPath = System.IO.Path.GetTempFileName() + ".jsonl"
    let (recordInner, getRecordOutput) = TestBackend.create 40 5 inputEvents
    let recordingBackend = Recording.wrapRecording Recording.InputOnly recordPath recordInner
    App.runWith AppConfig.defaults recordingBackend program

    // Replay
    let (replayInner, getReplayOutput) = TestBackend.create 40 5 []
    let replayBackend = Recording.replayBackend Recording.Instant recordPath replayInner
    App.runWith AppConfig.defaults replayBackend program

    // Both should have produced the same terminal output
    let recordOut = getRecordOutput ()
    let replayOut = getReplayOutput ()
    recordOut |> Expect.equal "outputs match" replayOut
  }
]

[<Tests>]
let allRecordingTests = testList "Recording" [
  keyRoundtripTests
  eventRoundtripTests
  serializationTests
  recordingBackendTests
  replayBackendTests
  integrationTests
]
