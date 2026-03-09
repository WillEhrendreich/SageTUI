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

/// Same structure but with El.keyed wrappers to exercise the HitMap path.
/// Keys are stable per-frame; strings are now materialised only in hitTest/keyAreas,
/// not during rendering.
let keyedDashboardTree =
    El.column [
        El.keyed "header" (
            El.row [
                El.text "Dashboard" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
            ]
        )
        El.keyed "main" (
            El.row [
                El.keyed "panel1" (El.column [ El.text "Panel 1" |> El.bordered Light |> El.fill ] |> El.fill)
                El.keyed "panel2" (El.column [ El.text "Panel 2" |> El.bordered Light |> El.fill ] |> El.fill)
            ] |> El.fill
        )
        El.keyed "statusbar" (
            El.row [
                El.text "Status: OK" |> El.fg (Color.Named(Green, Bright))
                El.text " | " |> El.dim
                El.text "80x24"    |> El.dim
            ]
        )
    ]

// ── Tests ─────────────────────────────────────────────────────────────────────

/// Maximum bytes the arena path may allocate per frame in steady state.
/// This is intentionally generous (1 KB) to be stable across GC versions
/// and debug vs. release JIT. The important invariant is "not proportional
/// to tree size" — i.e., allocations do not grow with each frame. In practice
/// the steady-state delta should be < 100 bytes (GC measurement overhead and
/// any residual boxing in the F# runtime under debug JIT).
/// Before Sprint 21: ~3,640 bytes/frame (Area records + StringBuilder + string-per-rune).
/// After Sprint 21: < 1,024 bytes/frame (struct Area, Span<char> text path, zero-alloc layout).
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

        testCase "keyed tree render allocates < 1KB per frame after warmup (HitMap path)" <| fun () ->
            // This test covers the HitMap path: El.keyed nodes record (KeyStart, KeyLen) pairs
            // in HitMap — no string materialisation during render, only during hitTest/keyAreas calls.
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let area  = { X = 0; Y = 0; Width = 80; Height = 24 }

            let frame () =
                FrameArena.reset arena
                let root = Arena.lower arena keyedDashboardTree
                ArenaRender.renderRoot arena root area buf

            let totalAllocated = measureAllocBytes 100 1000 frame
            let perFrame = totalAllocated / 1000L

            (perFrame, MaxSteadyStateBytesPerFrame)
            |> Expect.isLessThanOrEqual
                (sprintf "Keyed-tree arena per-frame allocation %d B exceeds %d B threshold; HitEntry string materialisation must be deferred to hitTest/keyAreas callers"
                    perFrame MaxSteadyStateBytesPerFrame)

        testCase "PeakLayout is set after reset (LayoutScratch high-water mark)" <| fun () ->
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let area  = { X = 0; Y = 0; Width = 80; Height = 24 }

            FrameArena.reset arena
            let root = Arena.lower arena dashboardTree
            ArenaRender.renderRoot arena root area buf
            let layoutPosBefore = arena.LayoutPos

            // After reset, PeakLayout should reflect the layout scratch consumed
            FrameArena.reset arena

            (arena.PeakLayout, 0)
            |> Expect.isGreaterThan
                "PeakLayout must be set after reset — arena must track layout scratch high-water mark for sizing diagnostics"
            arena.PeakLayout
            |> Expect.equal
                "PeakLayout must equal LayoutPos of the previous frame"
                layoutPosBefore

        testCase "LayoutScratch overflow raises a diagnostic error" <| fun () ->
            // Create an arena with a tiny LayoutScratch that cannot fit even a small row.
            // This verifies the guard fires with an actionable message rather than silently
            // overwriting array memory or throwing an IndexOutOfRangeException.
            let tinyArena = FrameArena.create 4096 65536 1 // 1-slot scratch: cannot hold a 2-child row
            let buf  = Buffer.create 80 24
            let area = { X = 0; Y = 0; Width = 80; Height = 24 }
            let twoChildRow =
                El.row [ El.text "Left"; El.text "Right" ]

            FrameArena.reset tinyArena
            let root = Arena.lower tinyArena twoChildRow
            let throws =
                try
                    ArenaRender.renderRoot tinyArena root area buf
                    false
                with ex ->
                    ex.Message.Contains("LayoutScratch overflow")

            throws
            |> Expect.isTrue
                "LayoutScratch overflow must throw with a diagnostic message containing 'LayoutScratch overflow', not IndexOutOfRangeException"

        testCase "keyAreas always reflects current-frame position (regression: stale prevKeyAreas for staying elements)" <| fun () ->
            // Regression test for Sprint 24 fix. Scenario: a keyed element is present in
            // both frame 1 and frame 2 ('staying'), but repositions between frames.
            // Verifies that ArenaRender.keyAreas always returns the CURRENT frame's area —
            // i.e., the map is unconditionally rebuilt when HitMap.Count > 0, not cached.
            //
            // In App.run, prevKeyAreas <- currentKeyAreas each frame. If the staying element
            // later exits, the exit transition must use the new (frame 2) position, not the
            // original (frame 1) entry position. Without Sprint 24's fix, prevKeyAreas would
            // be stale because the map was only rebuilt when entering/exiting were non-empty.
            let arena = FrameArena.create 4096 65536 4096
            let buf   = Buffer.create 80 24
            let fullArea = { X = 0; Y = 0; Width = 80; Height = 24 }

            // Frame 1: "panel" occupies left half (X=0, Width=40)
            // El.width must be OUTSIDE El.keyed to be visible to the Row layout pass
            FrameArena.reset arena
            let frame1 = El.row [ El.keyed "panel" (El.text "P") |> El.width 40; El.fill (El.text "") ]
            let root1 = Arena.lower arena frame1
            ArenaRender.renderRoot arena root1 fullArea buf
            let areas1 = ArenaRender.keyAreas arena

            // Frame 2: same key "panel", now occupies right half (X=40, Width=40)
            FrameArena.reset arena
            let frame2 = El.row [ El.fill (El.text ""); El.keyed "panel" (El.text "P") |> El.width 40 ]
            let root2 = Arena.lower arena frame2
            ArenaRender.renderRoot arena root2 fullArea buf
            let areas2 = ArenaRender.keyAreas arena

            areas1.["panel"].X |> Expect.equal "frame1: panel starts at left edge (X=0)" 0
            areas2.["panel"].X |> Expect.equal "frame2: panel repositioned to right half (X=40)" 40
            (areas2.["panel"].X, areas1.["panel"].X)
            |> Expect.isGreaterThan
                "keyAreas must reflect the current frame — if panel repositions, the map must show the new position"

        testCase "Dissolve per-frame apply allocates < 64 bytes after warmup (DissolveOrder cached)" <| fun () ->
            // Allocates the DissolveOrder array ONCE at construction, not per frame.
            // Verifies the per-frame apply path allocates near zero when using a cached order.
            // Before Sprint 24: fisherYatesShuffle allocated a fresh int array per frame
            // (~7.68KB for 40x12 area). After: the order is pre-computed; apply is allocation-free.
            let area = { X = 0; Y = 0; Width = 40; Height = 12 }
            let buf  = Buffer.create 40 12
            let order = TransitionFx.fisherYatesShuffle 42 (area.Width * area.Height)
            let snapshot = Array.zeroCreate (area.Width * area.Height)
            let at =
                { Key = "panel"
                  Transition = Dissolve 300<ms>
                  StartMs = 0L
                  DurationMs = 300
                  Easing = Ease.cubicInOut
                  SnapshotBefore = snapshot
                  Area = area
                  DissolveOrder = Some order }

            let frame () =
                let t = ActiveTransition.progress 150L at
                match at.DissolveOrder with
                | Some o ->
                    TransitionFx.applyDissolve t o at.SnapshotBefore buf.Cells at.Area.Y at.Area.Width at.Area.Height buf
                | None -> ()

            let totalAllocated = measureAllocBytes 100 1000 frame
            let perFrame = totalAllocated / 1000L

            (perFrame, 64L)
            |> Expect.isLessThanOrEqual
                (sprintf "Dissolve per-frame apply allocated %d bytes (expected < 64). DissolveOrder must be pre-computed and passed as a cached array, not re-allocated per frame." perFrame)
    ]
