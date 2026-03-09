module SageTUI.ArenaAllocationTests

open System
open Expecto
open Expecto.Flip
open SageTUI

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Measures managed bytes allocated by [f] after GC stabilization.
/// Runs [warmup] iterations of [f] first so the arena's bump-pointer is
/// exercised and any one-time .NET JIT or GC overhead is paid before we measure.
let measureAllocBytes (warmup: int) (measured: int) (f: unit -> unit) : int64 =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    for _ in 1 .. warmup do f ()
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    let before = GC.GetTotalAllocatedBytes(precise = true)
    for _ in 1 .. measured do f ()
    let after = GC.GetTotalAllocatedBytes(precise = true)
    after - before

// ── Tree fixtures ─────────────────────────────────────────────────────────────

let dashboardTree =
    El.column [
        El.row [
            El.text "Dashboard" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
        ]
        El.row [
            El.column [ El.text "Panel 1" |> El.bordered Light |> El.fill ] |> El.fill
            El.column [ El.text "Panel 2" |> El.bordered Light |> El.fill ] |> El.fill
        ] |> El.fill
        El.row [
            El.text "Status: OK" |> El.fg (Color.Named(Green, Bright))
            El.text " | " |> El.dim
            El.text "80x24"    |> El.dim
        ]
    ]

// ── Tests ─────────────────────────────────────────────────────────────────────

/// Maximum bytes the arena path may allocate per frame in steady state.
/// This is intentionally generous (1 KB) to be stable across GC versions
/// and debug vs. release JIT. The important invariant is "not proportional
/// to tree size" — i.e., allocations do not grow with each frame. In practice
/// the steady-state delta should be < 100 bytes (boxing of the FrameArena
/// struct fields and measurement overhead).
[<Literal>]
let MaxSteadyStateBytesPerFrame = 1024L

[<Tests>]
let arenaAllocationTests =
    // Allocation measurement uses GC.GetTotalAllocatedBytes (counts all threads).
    // Must run sequenced to avoid cross-thread contamination from the parallel test runner.
    testSequenced <| testList "Arena.steady-state allocation" [

        testCase "arena render allocates < 1KB per frame after warmup (80x24 dashboard)" <| fun () ->
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let area  = { X = 0; Y = 0; Width = 80; Height = 24 }

            let frame () =
                FrameArena.reset arena
                let root = Arena.lower arena dashboardTree
                ArenaRender.renderRoot arena root area buf

            let totalAllocated = measureAllocBytes 100 1000 frame
            let perFrame = totalAllocated / 1000L

            (perFrame, MaxSteadyStateBytesPerFrame)
            |> Expect.isLessThanOrEqual
                (sprintf "Arena per-frame allocation %d B exceeds %d B threshold; arena is allocating heap objects instead of using pre-allocated arrays"
                    perFrame MaxSteadyStateBytesPerFrame)

        testCase "arena NodeCount is stable across resets" <| fun () ->
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let area  = { X = 0; Y = 0; Width = 80; Height = 24 }

            // Two frames to confirm NodeCount stabilizes
            FrameArena.reset arena
            let root1 = Arena.lower arena dashboardTree
            ArenaRender.renderRoot arena root1 area buf
            let count1 = arena.NodeCount

            FrameArena.reset arena
            let root2 = Arena.lower arena dashboardTree
            ArenaRender.renderRoot arena root2 area buf
            let count2 = arena.NodeCount

            count2
            |> Expect.equal
                "NodeCount must be identical across frames for the same tree (arena is deterministic)"
                count1

        testCase "PeakNodes is set after reset (arena tracks high-water mark)" <| fun () ->
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let area  = { X = 0; Y = 0; Width = 80; Height = 24 }

            FrameArena.reset arena
            let root = Arena.lower arena dashboardTree
            ArenaRender.renderRoot arena root area buf
            let nodesBefore = arena.NodeCount

            // reset advances PeakNodes
            FrameArena.reset arena

            (arena.PeakNodes, 0)
            |> Expect.isGreaterThan
                "PeakNodes must be set after reset — arena must track high-water mark for diagnostics"
            arena.PeakNodes
            |> Expect.equal
                "PeakNodes must equal NodeCount of the previous frame"
                nodesBefore
    ]
