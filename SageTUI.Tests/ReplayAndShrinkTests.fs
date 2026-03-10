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
    Subscribe = fun _ -> []
    OnError = CrashOnError }

// ── Testing.replaySession────────────────────────────────────────────────────

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
    Key.Char (System.Text.Rune '+'), KeyInc
    Key.Char (System.Text.Rune '-'), KeyDec
    Key.Char (System.Text.Rune 'r'), KeyReset
    Key.Char (System.Text.Rune 'q'), KeyQuit ]
  { Init    = fun () -> { Count = 0 }, Cmd.none
    Update  = fun msg model ->
      match msg with
      | KeyInc   -> { model with Count = model.Count + 1 }, Cmd.none
      | KeyDec   -> { model with Count = model.Count - 1 }, Cmd.none
      | KeyReset -> { Count = 0 }, Cmd.none
      | KeyQuit  -> model, Cmd.quitWith 0
    View    = fun model -> El.text (sprintf "Count: %d" model.Count)
    Subscribe = fun _ -> [ bindings ]
    OnError = CrashOnError }

let routeTerminalEventTests = testList "Testing.routeTerminalEvent" [
  test "KeyPressed routes to KeySub handler" {
    let app = TestHarness.init 80 24 keyCounterProgram
    let app' = Testing.routeTerminalEvent (KeyPressed(Key.Char (System.Text.Rune '+'), Modifiers.None)) app
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
      KeyPressed(Key.Char (System.Text.Rune '+'), Modifiers.None)
      KeyPressed(Key.Char (System.Text.Rune '+'), Modifiers.None)
      KeyPressed(Key.Char (System.Text.Rune '+'), Modifiers.None)
      KeyPressed(Key.Char (System.Text.Rune '-'), Modifiers.None) ]
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
// ── E2E mouse pipeline tests ─────────────────────────────────────────────────

// A mouse-aware program: tracks click position (ClickSub), drag position (DragSub),
// release position (MouseSub for Released phase), and click count.
type MouseMsg =
  | Clicked of int * int
  | Dragged of int * int
  | MouseReleased of int * int

type MouseModel = {
  ClickPos: (int * int) option
  DragPos: (int * int) option
  ReleasePos: (int * int) option
  ClickCount: int
}

let mouseProgramInit () =
  { ClickPos = None; DragPos = None; ReleasePos = None; ClickCount = 0 }, Cmd.none

let mouseProgramUpdate msg model =
  match msg with
  | Clicked(x, y)       -> { model with ClickPos = Some(x, y); ClickCount = model.ClickCount + 1 }, Cmd.none
  | Dragged(x, y)       -> { model with DragPos = Some(x, y) }, Cmd.none
  | MouseReleased(x, y) -> { model with ReleasePos = Some(x, y) }, Cmd.none

let mouseProgram : Program<MouseModel, MouseMsg> =
  { Init = mouseProgramInit
    Update = mouseProgramUpdate
    View = fun _ -> El.text "mouse test"
    Subscribe = fun _ ->
      [ ClickSub  (fun (me, _key) -> Some (Clicked  (me.X, me.Y)))
        DragSub   (fun me         -> Some (Dragged   (me.X, me.Y)))
        MouseSub  (fun me -> match me.Phase with
                             | MousePhase.Released -> Some (MouseReleased (me.X, me.Y))
                             | _ -> None) ]
    OnError = CrashOnError }

let mousePipelineTests = testList "E2E: mouse pipeline (ClickSub / DragSub / MouseSub)" [
  test "clickAt fires ClickSub with correct coordinates" {
    let app = TestHarness.init 80 24 mouseProgram
    let app' = TestHarness.clickAt 10 5 app
    app'.Model.ClickPos  |> Expect.equal "click pos (10,5)" (Some(10,5))
    app'.Model.ClickCount |> Expect.equal "click count 1" 1
  }

  test "dragAt fires DragSub with correct coordinates" {
    let app = TestHarness.init 80 24 mouseProgram
    let app' = TestHarness.dragAt 15 8 LeftButton app
    app'.Model.DragPos |> Expect.equal "drag pos (15,8)" (Some(15,8))
  }

  test "releaseAt fires MouseSub.Released with correct coordinates" {
    let app = TestHarness.init 80 24 mouseProgram
    let app' = TestHarness.releaseAt 12 6 app
    app'.Model.ReleasePos |> Expect.equal "release pos (12,6)" (Some(12,6))
  }

  test "click does NOT fire DragSub" {
    let app = TestHarness.init 80 24 mouseProgram
    let app' = TestHarness.clickAt 10 5 app
    app'.Model.DragPos |> Expect.isNone "no drag from click"
  }

  test "drag does NOT fire ClickSub" {
    let app = TestHarness.init 80 24 mouseProgram
    let app' = TestHarness.dragAt 10 5 LeftButton app
    app'.Model.ClickPos |> Expect.isNone "no click from drag"
    app'.Model.ClickCount |> Expect.equal "click count stays 0" 0
  }

  test "routeTerminalEvent Pressed routes to clickAt" {
    let app = TestHarness.init 80 24 mouseProgram
    let event = MouseInput { Button = LeftButton; X = 20; Y = 10; Modifiers = Modifiers.None; Phase = Pressed }
    let app' = Testing.routeTerminalEvent event app
    app'.Model.ClickPos |> Expect.equal "clicked at (20,10)" (Some(20,10))
  }

  test "routeTerminalEvent Motion routes to dragAt (DragSub fires)" {
    let app = TestHarness.init 80 24 mouseProgram
    let event = MouseInput { Button = LeftButton; X = 25; Y = 12; Modifiers = Modifiers.None; Phase = Motion }
    let app' = Testing.routeTerminalEvent event app
    app'.Model.DragPos |> Expect.equal "dragged to (25,12)" (Some(25,12))
    app'.Model.ClickPos |> Expect.isNone "no click from motion event"
  }

  test "routeTerminalEvent Released routes to releaseAt (MouseSub.Released fires)" {
    let app = TestHarness.init 80 24 mouseProgram
    let event = MouseInput { Button = LeftButton; X = 30; Y = 15; Modifiers = Modifiers.None; Phase = MousePhase.Released }
    let app' = Testing.routeTerminalEvent event app
    app'.Model.ReleasePos |> Expect.equal "released at (30,15)" (Some(30,15))
    app'.Model.DragPos |> Expect.isNone "no drag from release event"
  }

  test "Recording.encodeEvent / decodeEvent round-trips MouseInput Pressed" {
    let event = MouseInput { Button = LeftButton; X = 10; Y = 5; Modifiers = Modifiers.None; Phase = Pressed }
    let encoded = Recording.encodeEvent 0L event
    let decoded = Recording.decodeEvent encoded
    decoded |> Expect.equal "round-trips" (Some event)
  }

  test "Recording.encodeEvent / decodeEvent round-trips MouseInput Motion" {
    let event = MouseInput { Button = LeftButton; X = 15; Y = 8; Modifiers = Modifiers.None; Phase = Motion }
    let encoded = Recording.encodeEvent 0L event
    let decoded = Recording.decodeEvent encoded
    decoded |> Expect.equal "round-trips" (Some event)
  }

  test "Recording.encodeEvent / decodeEvent round-trips MouseInput Released" {
    let event = MouseInput { Button = LeftButton; X = 12; Y = 6; Modifiers = Modifiers.None; Phase = MousePhase.Released }
    let encoded = Recording.encodeEvent 0L event
    let decoded = Recording.decodeEvent encoded
    decoded |> Expect.equal "round-trips" (Some event)
  }

  test "full press-drag-release sequence all fire correct handlers" {
    let app = TestHarness.init 80 24 mouseProgram
    let press   = MouseInput { Button = LeftButton; X = 10; Y = 5; Modifiers = Modifiers.None; Phase = Pressed }
    let motion1 = MouseInput { Button = LeftButton; X = 12; Y = 7; Modifiers = Modifiers.None; Phase = Motion }
    let motion2 = MouseInput { Button = LeftButton; X = 14; Y = 9; Modifiers = Modifiers.None; Phase = Motion }
    let release = MouseInput { Button = LeftButton; X = 14; Y = 9; Modifiers = Modifiers.None; Phase = MousePhase.Released }
    let app' =
      app
      |> Testing.routeTerminalEvent press
      |> Testing.routeTerminalEvent motion1
      |> Testing.routeTerminalEvent motion2
      |> Testing.routeTerminalEvent release
    app'.Model.ClickPos   |> Expect.equal "pressed at (10,5)" (Some(10,5))
    app'.Model.DragPos    |> Expect.equal "last drag at (14,9)" (Some(14,9))
    app'.Model.ReleasePos |> Expect.equal "released at (14,9)" (Some(14,9))
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
  mousePipelineTests
]
