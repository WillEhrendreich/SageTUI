open SageTUI
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 3, iterationCount = 10)>]
type BufferDiffBenchmarks() =
  let mutable front = Buffer.create 80 24
  let mutable back = Buffer.create 80 24
  let mutable backChanged = Buffer.create 80 24

  [<GlobalSetup>]
  member _.Setup() =
    front <- Buffer.create 80 24
    back <- Buffer.create 80 24
    backChanged <- Buffer.create 80 24
    let cell = PackedCell.create (int 'A') 0xC8C8C8 0x000000 0us
    for i in 0 .. 80 * 24 - 1 do
      front.Cells[i] <- cell
      back.Cells[i] <- cell
      backChanged.Cells[i] <- cell
    let changed = PackedCell.create (int 'B') 0xFF0000 0x000000 0us
    for i in 0 .. 10 .. 80 * 24 - 1 do
      backChanged.Cells[i] <- changed

  [<Benchmark(Description = "Buffer.diff identical (80x24)")>]
  member _.DiffIdentical() =
    Buffer.diff front back |> ignore

  [<Benchmark(Description = "Buffer.diff 10% changed (80x24)")>]
  member _.DiffChanged() =
    Buffer.diff front backChanged |> ignore

[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 3, iterationCount = 10)>]
type RenderBenchmarks() =
  let area80x24 = { X = 0; Y = 0; Width = 80; Height = 24 }
  let mutable buf = Buffer.create 80 24

  let dashboardTree =
    El.column [
      El.row [
        El.text "Dashboard" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
      ]
      El.row [
        El.column [
          El.text "Panel 1" |> El.bordered Light |> El.fill
        ] |> El.fill
        El.column [
          El.text "Panel 2" |> El.bordered Light |> El.fill
        ] |> El.fill
      ] |> El.fill
      El.row [
        El.text "Status: OK" |> El.fg (Color.Named(Green, Bright))
        El.text " | " |> El.dim
        El.text "80x24" |> El.dim
      ]
    ]

  [<IterationSetup>]
  member _.Reset() =
    buf <- Buffer.create 80 24

  [<Benchmark(Description = "Render dashboard tree (80x24)")>]
  member _.RenderDashboard() =
    Render.render area80x24 Style.empty buf dashboardTree

  [<Benchmark(Description = "Arena render dashboard (80x24)")>]
  member _.ArenaRenderDashboard() =
    let arena = FrameArena.create 4096 65536 4096
    let root = Arena.lower arena dashboardTree
    ArenaRender.renderRoot arena root area80x24 buf

[<MemoryDiagnoser>]
[<SimpleJob(warmupCount = 3, iterationCount = 10)>]
type LayoutBenchmarks() =
  let area80x50 = { X = 0; Y = 0; Width = 80; Height = 50 }
  let area80x100 = { X = 0; Y = 0; Width = 80; Height = 100 }

  let list50 =
    El.column [
      for i in 1 .. 50 do
        El.text (sprintf "Item %d" i) |> El.fill
    ]

  let inner =
    El.row [
      El.text "A" |> El.fill
      El.text "B" |> El.width 10
      El.text "C" |> El.percentage 30
    ]
  let nested =
    El.column [
      for _ in 1 .. 10 do
        inner |> El.bordered Light |> El.padAll 1
    ]

  [<Benchmark(Description = "Measure+layout 50 items column")>]
  member _.Layout50Items() =
    let buf = Buffer.create 80 50
    Render.render area80x50 Style.empty buf list50

  [<Benchmark(Description = "Measure+layout nested 3-level")>]
  member _.LayoutNested() =
    let buf = Buffer.create 80 100
    Render.render area80x100 Style.empty buf nested

[<EntryPoint>]
let main _argv =
  BenchmarkRunner.Run<BufferDiffBenchmarks>() |> ignore
  BenchmarkRunner.Run<RenderBenchmarks>() |> ignore
  BenchmarkRunner.Run<LayoutBenchmarks>() |> ignore
  0
