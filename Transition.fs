namespace SageTUI

type ActiveTransition = {
  Key: string
  Transition: Transition
  StartMs: int64
  DurationMs: int
  Easing: Easing
  SnapshotBefore: PackedCell array
  Area: Area
}

module ActiveTransition =
  let progress (nowMs: int64) (at: ActiveTransition) =
    let t = Anim.progress at.StartMs nowMs at.DurationMs
    at.Easing t

  let isDone (nowMs: int64) (at: ActiveTransition) =
    Anim.isDone at.StartMs nowMs at.DurationMs

module TransitionFx =
  let lerpPackedColor (t: float) (a: int32) (b: int32) : int32 =
    let tagA = a &&& 0x3
    let tagB = b &&& 0x3
    match tagA, tagB with
    | 3, 3 ->
      let rA = (a >>> 8) &&& 0xFF
      let gA = (a >>> 16) &&& 0xFF
      let bA = (a >>> 24) &&& 0xFF
      let rB = (b >>> 8) &&& 0xFF
      let gB = (b >>> 16) &&& 0xFF
      let bB = (b >>> 24) &&& 0xFF
      let r = Anim.lerpInt rA rB t
      let g = Anim.lerpInt gA gB t
      let bl = Anim.lerpInt bA bB t
      3 ||| (r <<< 8) ||| (g <<< 16) ||| (bl <<< 24)
    | _ ->
      match t >= 0.5 with
      | true -> b
      | false -> a

  let lerpCell (t: float) (old: PackedCell) (new': PackedCell) : PackedCell =
    { Rune = (match t >= 0.5 with true -> new'.Rune | false -> old.Rune)
      Fg = lerpPackedColor t old.Fg new'.Fg
      Bg = lerpPackedColor t old.Bg new'.Bg
      Attrs = (match t >= 0.5 with true -> new'.Attrs | false -> old.Attrs)
      _pad = 0us }

  let applyColorMorph (t: float) (snapshot: PackedCell array) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        match idx < snapshot.Length && idx < current.Length with
        | true ->
          let cell = lerpCell t snapshot.[idx] current.[idx]
          buf.Cells.[(offset + row) * buf.Width + col] <- cell
        | false -> ()

  let applyFade (t: float) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    let defaultFg = PackedColor.pack Default
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        match idx < current.Length with
        | true ->
          let cell = current.[idx]
          let faded = { cell with
                          Fg = lerpPackedColor t defaultFg cell.Fg
                          Bg = lerpPackedColor t 0 cell.Bg }
          buf.Cells.[(offset + row) * buf.Width + col] <- faded
        | false -> ()

  let applyWipe (t: float) (dir: Direction) (snapshot: PackedCell array) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        let revealed =
          match dir with
          | Direction.Right -> float (col + 1) / float (max 1 width) <= t
          | Direction.Left -> float (width - col) / float (max 1 width) <= t
          | Direction.Down -> float (row + 1) / float (max 1 height) <= t
          | Direction.Up -> float (height - row) / float (max 1 height) <= t
        match idx < snapshot.Length && idx < current.Length with
        | true ->
          let cell = match revealed with true -> current.[idx] | false -> snapshot.[idx]
          buf.Cells.[(offset + row) * buf.Width + col] <- cell
        | false -> ()

  let fisherYatesShuffle (seed: int) (n: int) =
    let rng = System.Random(seed)
    let arr: int array = Array.init n id
    for i in n - 1 .. -1 .. 1 do
      let j = rng.Next(i + 1)
      let tmp = arr.[i]
      arr.[i] <- arr.[j]
      arr.[j] <- tmp
    arr

  let applyDissolve (t: float) (shuffleOrder: int array) (snapshot: PackedCell array) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    let revealCount = int (float shuffleOrder.Length * t)
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        match idx < snapshot.Length && idx < current.Length && idx < shuffleOrder.Length with
        | true ->
          let revealed = shuffleOrder.[idx] < revealCount
          let cell = match revealed with true -> current.[idx] | false -> snapshot.[idx]
          buf.Cells.[(offset + row) * buf.Width + col] <- cell
        | false -> ()

module TransitionDuration =
  let rec get (t: Transition) =
    match t with
    | Fade d | ColorMorph d | Dissolve d | Grow d -> int d
    | Wipe(_, d) | SlideIn(_, d) -> int d
    | Sequence ts -> ts |> List.sumBy get
    | Custom _ -> 200

module Reconcile =
  let findKeyedElements (el: Element) : Map<string, Element> =
    let rec walk (acc: Map<string, Element>) (e: Element) =
      match e with
      | Keyed(key, _, _, child) when key <> "" ->
        walk (Map.add key e acc) child
      | Row children | Column children ->
        children |> List.fold walk acc
      | Styled(_, child) | Padded(_, child) | Bordered(_, child)
      | Constrained(_, child) ->
        walk acc child
      | Overlay layers ->
        layers |> List.fold walk acc
      | _ -> acc
    walk Map.empty el

  let reconcile (oldKeys: Map<string, Element>) (newKeys: Map<string, Element>) =
    let entering =
      newKeys
      |> Map.filter (fun k _ -> not (Map.containsKey k oldKeys))
      |> Map.toList
    let exiting =
      oldKeys
      |> Map.filter (fun k _ -> not (Map.containsKey k newKeys))
      |> Map.toList
    let staying =
      newKeys
      |> Map.filter (fun k _ -> Map.containsKey k oldKeys)
      |> Map.toList
    (entering, exiting, staying)
