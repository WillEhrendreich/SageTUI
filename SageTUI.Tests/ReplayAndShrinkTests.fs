module ReplayAndShrinkTests

open Expecto
open Expecto.Flip
open SageTUI

// ── Test program for replay/shrink ───────────────────────────────────────────

type CounterMsg = Inc | Dec | Reset

type CounterModel = { Count: int }

let counterProgram : Program<CounterModel, CounterMsg> =
  { Init    = fun () -> { Count = 0 }, Cmd.none
    Update  = fun msg model ->
      match msg with
      | Inc   -> { model with Count = model.Count + 1 }, Cmd.none
      | Dec   -> { model with Count = model.Count - 1 }, Cmd.none
      | Reset -> { Count = 0 }, Cmd.none
    View    = fun model -> El.text (sprintf "Count: %d" model.Count)
    Subscribe = fun _ -> [] }

// ── Testing.replaySession ────────────────────────────────────────────────────

let replaySessionTests = testList "Testing.replaySession" [
  test "empty sequence leaves model at init state" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.replaySession [] app
    result.Model.Count |> Expect.equal "count stays 0" 0
  }
  test "replaying [Inc;Inc;Inc] yields count 3" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.replaySession [Inc; Inc; Inc] app
    result.Model.Count |> Expect.equal "count is 3" 3
  }
  test "replaying [Inc;Dec] yields count 0" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.replaySession [Inc; Dec] app
    result.Model.Count |> Expect.equal "count is 0" 0
  }
  test "replaying [Inc;Inc;Reset;Inc] yields count 1" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.replaySession [Inc; Inc; Reset; Inc] app
    result.Model.Count |> Expect.equal "count is 1" 1
  }
  test "stops at first Quit message" {
    let quitProgram : Program<CounterModel, CounterMsg> =
      { counterProgram with
          Update = fun msg model ->
            match msg with
            | Inc -> { model with Count = model.Count + 1 }, Cmd.quitWith 0
            | _   -> model, Cmd.none }
    let app = TestHarness.init 80 24 quitProgram
    let result = Testing.replaySession [Inc; Inc; Inc] app
    result.HasQuit |> Expect.isTrue "has quit"
    result.Model.Count |> Expect.equal "stopped after first Inc" 1
  }
]

// ── Testing.shrinkReplay ─────────────────────────────────────────────────────

let shrinkReplayTests = testList "Testing.shrinkReplay" [
  test "returns None when predicate never fails" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.shrinkReplay [Inc; Inc; Inc] app (fun a -> a.Model.Count > 100)
    result |> Expect.isNone "predicate never triggered"
  }
  test "returns Some when full sequence fails" {
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.shrinkReplay [Inc; Inc; Inc] app (fun a -> a.Model.Count >= 3)
    result |> Expect.isSome "predicate triggered"
  }
  test "shrinks [Inc×10] → [Inc] when predicate is count > 0" {
    let app = TestHarness.init 80 24 counterProgram
    let msgs = List.replicate 10 Inc
    let result = Testing.shrinkReplay msgs app (fun a -> a.Model.Count > 0)
    match result with
    | None -> failtest "should have found minimal"
    | Some shrunk ->
      shrunk |> Expect.hasLength "single Inc" 1
      shrunk |> Expect.equal "exactly [Inc]" [Inc]
  }
  test "shrinks [Inc;Dec;Inc;Inc;Inc] to reproduce count >= 2" {
    let app = TestHarness.init 80 24 counterProgram
    let msgs = [Inc; Dec; Inc; Inc; Inc]
    let result = Testing.shrinkReplay msgs app (fun a -> a.Model.Count >= 2)
    match result with
    | None -> failtest "should have found minimal"
    | Some shrunk ->
      // Minimal should be at most 2 Incs
      (shrunk.Length, 2) |> Expect.isLessThanOrEqual "at most 2 messages"
  }
  test "shrinks to empty list when all messages needed" {
    // predicate fails when count is negative (need Dec to happen)
    let app = TestHarness.init 80 24 counterProgram
    let result = Testing.shrinkReplay [Dec] app (fun a -> a.Model.Count < 0)
    match result with
    | None -> failtest "should detect Dec causes negative"
    | Some shrunk ->
      shrunk |> Expect.hasLength "single Dec" 1
  }
  test "prefix shrink removes trailing irrelevant messages" {
    // [Inc;Inc;Dec;Inc;Inc;Inc;Inc] — predicate is count >= 2
    // Shortest prefix: [Inc;Inc] (length 2)
    let app = TestHarness.init 80 24 counterProgram
    let msgs = [Inc; Inc; Dec; Inc; Inc; Inc; Inc]
    let result = Testing.shrinkReplay msgs app (fun a -> a.Model.Count >= 2)
    match result with
    | None -> failtest "should find minimal"
    | Some shrunk ->
      (shrunk.Length, 3) |> Expect.isLessThanOrEqual "at most 3 messages"
  }
]

// ── Testing.replayFromLines / Testing.sessionToLines ─────────────────────────

let serializationTests = testList "Testing.replayFromLines" [
  test "sessionToLines / replayFromLines round-trips" {
    let msgs = [Inc; Dec; Reset; Inc]
    let serialize msg =
      match msg with
      | Inc -> "Inc" | Dec -> "Dec" | Reset -> "Reset"
    let deserialize s =
      match s with
      | "Inc" -> Some Inc | "Dec" -> Some Dec | "Reset" -> Some Reset
      | _ -> None
    let lines = Testing.sessionToLines serialize msgs
    lines |> Expect.hasLength "4 lines" 4
    let restored = Testing.replayFromLines deserialize lines
    restored |> Expect.equal "round-trips" msgs
  }
  test "replayFromLines skips unrecognized lines" {
    let deserialize s =
      match s with
      | "Inc" -> Some Inc | _ -> None
    let lines = [| "Inc"; "bad"; "Inc"; "garbage" |]
    let msgs = Testing.replayFromLines deserialize lines
    msgs |> Expect.hasLength "2 valid messages" 2
    msgs |> Expect.equal "only Incs" [Inc; Inc]
  }
]



// ── Testing.routeTerminalEvent ────────────────────────────────────────────────

// A key-driven counter for testing routeTerminalEvent
type KeyCounterMsg = KeyInc | KeyDec | KeyReset | KeyQuit

let keyCounterProgram : Program<CounterModel, KeyCounterMsg> =
  let bindings = Keys.bind [
    Key.Char '+', KeyInc
    Key.Char '-', KeyDec
    Key.Char 'r', KeyReset
    Key.Char 'q', KeyQuit ]
  { Init    = fun () -> { Count = 0 }, Cmd.none
    Update  = fun msg model ->
      match msg with
      | KeyInc   -> { model with Count = model.Count + 1 }, Cmd.none
      | KeyDec   -> { model with Count = model.Count - 1 }, Cmd.none
      | KeyReset -> { Count = 0 }, Cmd.none
      | KeyQuit  -> model, Cmd.quitWith 0
    View    = fun model -> El.text (sprintf "Count: %d" model.Count)
    Subscribe = fun _ -> [ bindings ] }

let routeTerminalEventTests = testList "Testing.routeTerminalEvent" [
  test "KeyPressed routes to KeySub handler" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent (KeyPressed(Key.Char '+', Modifiers.None)) app
    app'.Model.Count |> Expect.equal "count incremented" 1
  }
  test "Resized routes to resize" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent (Resized(120, 40)) app
    app'.Width  |> Expect.equal "width updated"  120
    app'.Height |> Expect.equal "height updated" 40
  }
  test "FocusGained is a no-op" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent FocusGained app
    app'.Model |> Expect.equal "model unchanged" app.Model
  }
  test "FocusLost is a no-op" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent FocusLost app
    app'.Model |> Expect.equal "model unchanged" app.Model
  }
  test "Pasted is a no-op for keyCounterProgram" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent (Pasted "hello") app
    app'.Model |> Expect.equal "model unchanged" app.Model
  }
  test "multiple events can be folded" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let events = [
      KeyPressed(Key.Char '+', Modifiers.None)
      KeyPressed(Key.Char '+', Modifiers.None)
      KeyPressed(Key.Char '+', Modifiers.None)
      KeyPressed(Key.Char '-', Modifiers.None) ]
    let app' = events |> List.fold (fun a e -> Testing.routeTerminalEvent e a) app
    app'.Model.Count |> Expect.equal "count is 2" 2
  }
]

// ── Testing.replayRecording (file-based) ──────────────────────────────────────

let replayRecordingTests = testList "Testing.replayRecording" [
  test "replayRecording returns init state for missing file" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.replayRecording "nonexistent-path-does-not-exist.jsonl" app
    app'.Model.Count |> Expect.equal "count unchanged for missing file" 0
  }
]

// ── TextInput cursor clamping ────────────────────────────────────────────────

let textInputClampTests = testList "TextInput.clamp" [
  test "clamp keeps valid cursor unchanged" {
    let m = { Text = "hello"; Cursor = 3 ; SelectionAnchor = None }
    let m' = TextInput.clamp m
    m'.Cursor |> Expect.equal "cursor unchanged" 3
  }
  test "clamp clamps cursor above text length to text length" {
    let m = { Text = "hi"; Cursor = 99 ; SelectionAnchor = None }
    let m' = TextInput.clamp m
    m'.Cursor |> Expect.equal "clamped to 2" 2
  }
  test "clamp clamps negative cursor to 0" {
    let m = { Text = "hi"; Cursor = -5 ; SelectionAnchor = None }
    let m' = TextInput.clamp m
    m'.Cursor |> Expect.equal "clamped to 0" 0
  }
  test "clamp handles empty text" {
    let m = { Text = ""; Cursor = 5 ; SelectionAnchor = None }
    let m' = TextInput.clamp m
    m'.Cursor |> Expect.equal "clamped to 0 for empty" 0
  }
  test "setText preserves cursor within new text" {
    let m = { Text = "hello world"; Cursor = 5 ; SelectionAnchor = None }
    let m' = TextInput.setText "hi" m
    m'.Cursor |> Expect.equal "cursor clamped to 2" 2
    m'.Text   |> Expect.equal "text updated" "hi"
  }
  test "setText keeps cursor at original position if within new text" {
    let m = { Text = "hi"; Cursor = 2 ; SelectionAnchor = None }
    let m' = TextInput.setText "hello world" m
    m'.Cursor |> Expect.equal "cursor stays at 2" 2
    m'.Text   |> Expect.equal "text updated" "hello world"
  }
  test "setText with cursor at 0 stays at 0" {
    let m = { Text = "abc"; Cursor = 0 ; SelectionAnchor = None }
    let m' = TextInput.setText "xyz" m
    m'.Cursor |> Expect.equal "cursor stays at 0" 0
  }
]
[<Tests>]
let allReplayAndShrinkTests = testList "Testing.replayAndShrink" [
  replaySessionTests
  shrinkReplayTests
  serializationTests
  routeTerminalEventTests
  replayRecordingTests
  textInputClampTests
]
