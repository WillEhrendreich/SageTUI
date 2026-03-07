module HitTestTests

open Expecto
open Expecto.Flip
open SageTUI

let hitMapTests = testList "HitMap" [
  test "keyed element registers in hit map" {
    let arena = FrameArena.create 256 1024 256
    let elem = El.text "Click me" |> El.keyed "btn-save" |> El.width 10
    let rootHandle = Arena.lower arena elem
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    ArenaRender.renderRoot arena rootHandle area buf
    (arena.HitMap.Count, 0) |> Expect.isGreaterThan "has entries"
  }

  test "hitTest finds keyed element at coordinates" {
    let arena = FrameArena.create 256 1024 256
    let elem =
      El.column [
        El.text "Header" |> El.keyed "header" |> El.width 80
        El.text "Content" |> El.keyed "content" |> El.width 80
      ]
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    ArenaRender.hitTest arena 10 0 |> Expect.equal "hits header" (Some "header")
  }

  test "hitTest returns None for empty area" {
    let arena = FrameArena.create 256 1024 256
    let elem = El.text "Hello"
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    ArenaRender.hitTest arena 10 10 |> Expect.isNone "no hit"
  }

  test "hitTest returns topmost element (Z-order)" {
    let arena = FrameArena.create 256 1024 256
    let elem =
      El.overlay [
        El.text "Background" |> El.keyed "bg" |> El.fill
        El.text "Foreground" |> El.keyed "fg" |> El.width 20
      ]
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    ArenaRender.hitTest arena 5 0 |> Expect.equal "hits fg (topmost)" (Some "fg")
  }

  test "hitTest misses outside all keyed areas" {
    let arena = FrameArena.create 256 1024 256
    let elem =
      El.row [
        El.text "Left" |> El.keyed "left" |> El.width 10
      ]
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    ArenaRender.hitTest arena 50 0 |> Expect.isNone "miss"
  }

  test "hit map resets between frames" {
    let arena = FrameArena.create 256 1024 256
    let elem = El.text "A" |> El.keyed "a"
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    let count1 = arena.HitMap.Count
    FrameArena.reset arena
    arena.HitMap.Count |> Expect.equal "reset clears" 0
    (count1, 0) |> Expect.isGreaterThan "had entries before reset"
  }

  test "nested keyed elements both register" {
    let arena = FrameArena.create 256 1024 256
    let elem =
      El.column [
        El.text "A" |> El.keyed "inner"
      ] |> El.keyed "outer"
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    (arena.HitMap.Count, 2) |> Expect.isGreaterThanOrEqual "both registered"
  }

  test "row with multiple keyed children" {
    let arena = FrameArena.create 256 1024 256
    let elem =
      El.row [
        El.text "Btn1" |> El.keyed "btn1" |> El.width 10
        El.text "Btn2" |> El.keyed "btn2" |> El.width 10
        El.text "Btn3" |> El.keyed "btn3" |> El.width 10
      ]
    let area = { X = 0; Y = 0; Width = 80; Height = 24 }
    let buf = Buffer.create 80 24
    let rootHandle = Arena.lower arena elem
    ArenaRender.renderRoot arena rootHandle area buf
    ArenaRender.hitTest arena 5 0 |> Expect.equal "btn1" (Some "btn1")
    ArenaRender.hitTest arena 15 0 |> Expect.equal "btn2" (Some "btn2")
    ArenaRender.hitTest arena 25 0 |> Expect.equal "btn3" (Some "btn3")
  }
]

[<Tests>]
let allHitTestTests = testList "HitTest" [ hitMapTests ]
