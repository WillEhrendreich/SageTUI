// Scratch script for interactive SageFs validation.
// Run via sagefs-load_fsharp_script or `dotnet fsi scratch.fsx`

#r @"C:\Code\Repos\SageTUI\bin\Release\net8.0\SageTUI.Library.dll"

open SageTUI

// ── Arena auto-grow smoke tests ──────────────────────────────────────────────
printfn "=== Arena Auto-Grow Validation ==="

// Test 1: allocNode beyond initial capacity does not throw
let testAllocNodeGrow () =
  let arena = FrameArena.create 4 64 32
  for _ in 1..100 do
    FrameArena.allocNode arena |> ignore
  let stats = FrameArena.stats arena
  printfn "allocNode x100: nodeCount=%d capacity=%d  %s"
    stats.NodeCount stats.NodeCapacity
    (if stats.NodeCount = 100 && stats.NodeCapacity >= 100 then "✓" else "✗ FAIL")

testAllocNodeGrow ()

// Test 2: allocText beyond initial text capacity does not throw
let testAllocTextGrow () =
  let arena = FrameArena.create 16 8 32
  let mutable last = (0, 0)
  for _ in 1..20 do
    last <- FrameArena.allocText "hello" arena
  printfn "allocText x20 (beyond initial 8 chars): last=%A  %s"
    last (if fst last > 0 then "✓" else "✗ FAIL")

testAllocTextGrow ()

// Test 3: Arena.lower on large tree (exercises LayoutScratch auto-grow)
let testLowerLargeTree () =
  let many = El.column [ for i in 1..200 -> El.text (string i) ]
  try
    let arena = FrameArena.create 16 512 32
    Arena.lower arena many |> ignore
    printfn "Arena.lower 200-child column: ✓"
  with ex ->
    printfn "Arena.lower 200-child column: ✗ FAIL — %s" ex.Message

testLowerLargeTree ()

printfn "=== Done ==="
