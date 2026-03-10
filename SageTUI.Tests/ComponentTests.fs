module SageTUI.ComponentTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ─────────────────────────────────────────────────────────────────────────────
// Shared helpers
// ─────────────────────────────────────────────────────────────────────────────

type CounterMsg = Inc | Dec

let counterProgram : Program<int, CounterMsg> =
  { Init = fun () -> 0, Cmd.none
    Update = fun msg count ->
      match msg with
      | Inc -> count + 1, Cmd.none
      | Dec -> (max 0 (count - 1)), Cmd.none
    View = fun count -> El.text (string count)
    Subscribe = fun _ -> []
    OnError = None }

type OuterModel= { Counter: int; Name: string }

let counterLens : Lens<OuterModel, int> =
  { Get = fun m -> m.Counter
    Set = fun c m -> { m with Counter = c } }

type ModalModel = { Title: string; Count: int }

let modalProgram : Program<ModalModel, CounterMsg> =
  { Init = fun () -> { Title = "Modal"; Count = 0 }, Cmd.none
    Update = fun msg model ->
      match msg with
      | Inc -> { model with Count = model.Count + 1 }, Cmd.none
      | Dec -> { model with Count = max 0 (model.Count - 1) }, Cmd.none
    View = fun model -> El.text (sprintf "[%s:%d]" model.Title model.Count)
    Subscribe = fun _ -> []
    OnError = None }

type ModalOuter= { Modal: ModalModel option; Status: string }

let modalPrism : Prism<ModalOuter, ModalModel> =
  { TryGet = fun m -> m.Modal
    Set = fun md m -> { m with Modal = Some md } }

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 43: Lens laws
// ─────────────────────────────────────────────────────────────────────────────

let lensLawTests = testList "Lens laws" [

  testProperty "get-set: get (set i o) = i" <| fun (counter: int) (name: NonEmptyString) ->
    let outer = { Counter = 0; Name = name.Get }
    let outer' = counterLens.Set counter outer
    counterLens.Get outer' |> Expect.equal "get after set returns set value" counter

  testProperty "set-get: set (get o) o = o" <| fun (counter: PositiveInt) (name: NonEmptyString) ->
    let outer = { Counter = counter.Get; Name = name.Get }
    let outer' = counterLens.Set (counterLens.Get outer) outer
    outer' |> Expect.equal "set-get round-trip is no-op" outer

  testProperty "set-set: set i (set j o) = set i o" <| fun (i: int) (j: int) (name: NonEmptyString) ->
    let outer = { Counter = 0; Name = name.Get }
    let setTwice = counterLens.Set i (counterLens.Set j outer)
    let setOnce  = counterLens.Set i outer
    setTwice |> Expect.equal "set-set: last set wins" setOnce
]

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 43: Program.embed
// ─────────────────────────────────────────────────────────────────────────────

type ParentMsg = Counter of CounterMsg | Noop

let componentEmbedTests = testList "Program.embed" [

  testCase "embed.Update routes child msg and updates lens target" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 5; Name = "test" }
    let (outer', cmd) = embedded.Update Inc outer
    outer'.Counter |> Expect.equal "counter incremented" 6
    outer'.Name    |> Expect.equal "name unchanged" "test"
    (match cmd with NoCmd -> true | _ -> false) |> Expect.isTrue "cmd is none"

  testCase "embed.Update does not affect sibling fields" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 3; Name = "untouched" }
    let (outer', _) = embedded.Update Dec outer
    outer'.Counter |> Expect.equal "decremented" 2
    outer'.Name    |> Expect.equal "sibling unchanged" "untouched"

  testCase "embed.View renders from lens.Get" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 42; Name = "test" }
    let elem   = embedded.View outer
    let lines  = TestHarness.renderElementLines 10 1 elem
    lines.[0].TrimEnd() |> Expect.equal "shows counter value" "42"

  testCase "embed.Init sets child model via lens" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 99; Name = "original" }
    let (outer', cmd) = embedded.Init outer
    outer'.Counter |> Expect.equal "counter reset to child.Init value (0)" 0
    outer'.Name    |> Expect.equal "sibling preserved" "original"

  testCase "embed.Subscribe maps sub messages" <| fun () ->
    // Counter program has no subs, so result is []
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 0; Name = "x" }
    embedded.Subscribe outer |> Expect.isEmpty "no subs from counter program"

  testCase "embed.Update accumulates across multiple messages" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 0; Name = "x" }
    let (s1, _) = embedded.Update Inc outer
    let (s2, _) = embedded.Update Inc s1
    let (s3, _) = embedded.Update Dec s2
    s3.Counter |> Expect.equal "2 inc + 1 dec = 1" 1

  testCase "embed Dec at zero stays at zero" <| fun () ->
    let embedded = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 0; Name = "x" }
    let (outer', _) = embedded.Update Dec outer
    outer'.Counter |> Expect.equal "clamped at 0" 0
]

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 43: Program.embedOptional
// ─────────────────────────────────────────────────────────────────────────────

let componentEmbedOptionalTests = testList "Program.embedOptional" [

  testCase "embedOptional.View returns El.empty when prism yields None" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = None; Status = "idle" }
    let lines = TestHarness.renderElementLines 10 1 (embedded.View outer)
    lines.[0].Trim() |> Expect.equal "renders empty (blank line)" ""

  testCase "embedOptional.View renders inner when prism yields Some" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = Some { Title = "T"; Count = 7 }; Status = "open" }
    let lines = TestHarness.renderElementLines 20 1 (embedded.View outer)
    lines.[0].TrimEnd() |> Expect.equal "renders modal view" "[T:7]"

  testCase "embedOptional.Update is no-op when prism yields None" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = None; Status = "idle" }
    let (outer', cmd) = embedded.Update Inc outer
    outer' |> Expect.equal "model unchanged" outer
    (match cmd with NoCmd -> true | _ -> false) |> Expect.isTrue "cmd is none"

  testCase "embedOptional.Update applies when prism yields Some" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = Some { Title = "X"; Count = 3 }; Status = "open" }
    let (outer', _) = embedded.Update Inc outer
    outer'.Modal |> Expect.equal "modal count incremented" (Some { Title = "X"; Count = 4 })

  testCase "embedOptional.Subscribe returns [] when prism yields None" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = None; Status = "idle" }
    embedded.Subscribe outer |> Expect.isEmpty "no subs when absent"

  testCase "embedOptional.Init is no-op" <| fun () ->
    let embedded = Program.embedOptional modalPrism Counter modalProgram
    let outer = { Modal = Some { Title = "X"; Count = 0 }; Status = "x" }
    let (outer', cmd) = embedded.Init outer
    outer' |> Expect.equal "model unchanged by Init" outer
    (match cmd with NoCmd -> true | _ -> false) |> Expect.isTrue "cmd is none"
]

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 43: Program.map backward compat
// ─────────────────────────────────────────────────────────────────────────────

let programMapTests = testList "Program.map (backward compat)" [

  testCase "Program.map is equivalent to embed with manual lens" <| fun () ->
    let viaMap    = Program.map Counter (fun m -> m.Counter) (fun c m -> { m with Counter = c }) counterProgram
    let viaEmbed  = Program.embed counterLens Counter counterProgram
    let outer = { Counter = 5; Name = "test" }
    let (m1, _) = viaMap.Update Inc outer
    let (m2, _) = viaEmbed.Update Inc outer
    m1 |> Expect.equal "same result as embed" m2
]

[<Tests>]
let allComponentTests = testList "Component composition (Sprint 43)" [
  lensLawTests
  componentEmbedTests
  componentEmbedOptionalTests
  programMapTests
]
