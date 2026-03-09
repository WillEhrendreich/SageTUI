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

[<Tests>]
let allReplayAndShrinkTests = testList "Testing.replayAndShrink" [
  replaySessionTests
  shrinkReplayTests
  serializationTests
]
