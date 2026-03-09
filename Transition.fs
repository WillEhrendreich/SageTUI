namespace SageTUI

/// Kind-specific runtime payload for an active transition.
/// DissolvePayload carries the pre-computed Fisher-Yates shuffle order (computed
/// once at transition start) so applyDissolve never allocates per-frame.
/// NoPayload is used for all other transition kinds that need no extra data.
type TransitionPayload =
  | DissolvePayload of order: int array
  | NoPayload

type ActiveTransition = {
  Key: string
  Transition: Transition
  StartMs: int64
  DurationMs: int
  Easing: Easing
  SnapshotBefore: PackedCell array
  Area: Area
  /// Kind-specific payload. For Dissolve transitions: DissolvePayload with the
  /// pre-computed shuffle order (avoids per-frame O(N) array allocation).
  /// For all other kinds: NoPayload.
  Payload: TransitionPayload
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

  /// SlideIn: new content enters from the given direction, sliding to its final position.
  /// The new content is shifted and clips into the area as t advances from 0 to 1.
  /// At t=0 the area shows the snapshot; at t=1 it shows the new content fully in place.
  let applySlideIn (t: float) (dir: Direction) (snapshot: PackedCell array) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        match idx < snapshot.Length && idx < current.Length with
        | true ->
          let cell =
            match dir with
            | Direction.Right ->
              // Content enters from the right; at t, the left portion (shift cols) still shows snapshot.
              let shift = int (float width * (1.0 - t))
              match col < shift with
              | true  -> snapshot.[idx]
              | false ->
                let srcIdx = row * width + (col - shift)
                match srcIdx < current.Length with
                | true  -> current.[srcIdx]
                | false -> snapshot.[idx]
            | Direction.Left ->
              // Content enters from the left; at t, right portion shows snapshot.
              let visible = int (float width * t)
              let shift   = width - visible
              match col >= visible with
              | true  -> snapshot.[idx]
              | false ->
                let srcIdx = row * width + (col + shift)
                match srcIdx < current.Length with
                | true  -> current.[srcIdx]
                | false -> snapshot.[idx]
            | Direction.Down ->
              // Content enters from below; top rows still show snapshot.
              let shift = int (float height * (1.0 - t))
              match row < shift with
              | true  -> snapshot.[idx]
              | false ->
                let srcIdx = (row - shift) * width + col
                match srcIdx < current.Length with
                | true  -> current.[srcIdx]
                | false -> snapshot.[idx]
            | Direction.Up ->
              // Content enters from above; bottom rows still show snapshot.
              let visible = int (float height * t)
              let shift   = height - visible
              match row >= visible with
              | true  -> snapshot.[idx]
              | false ->
                let srcIdx = (row + shift) * width + col
                match srcIdx < current.Length with
                | true  -> current.[srcIdx]
                | false -> snapshot.[idx]
          buf.Cells.[(offset + row) * buf.Width + col] <- cell
        | false -> ()

  /// Grow: new content expands outward from the center using Chebyshev distance.
  /// At t=0 only the center is visible; at t=1 the full area is revealed.
  /// For even-dimensioned areas no cell is at distance 0, so at t=0 the whole area shows snapshot.
  /// For odd-dimensioned areas the single center cell has distance 0 and IS revealed at t=0.
  let applyGrow (t: float) (snapshot: PackedCell array) (current: PackedCell array) (offset: int) (width: int) (height: int) (buf: Buffer) =
    let cx = float width  * 0.5
    let cy = float height * 0.5
    // maxR = max(cx,cy) + 0.5: guarantees the farthest corner is covered at t=1.
    // The farthest corner's Chebyshev dist from the float center = max(cx-0.5, cy-0.5) < max(cx,cy)+0.5.
    let maxR = max cx cy + 0.5
    let r = maxR * t
    for row in 0 .. height - 1 do
      for col in 0 .. width - 1 do
        let idx = row * width + col
        match idx < snapshot.Length && idx < current.Length with
        | true ->
          // Chebyshev distance from cell centre to area centre
          let dx = abs (float col + 0.5 - cx)
          let dy = abs (float row + 0.5 - cy)
          let cell =
            match max dx dy <= r with
            | true  -> current.[idx]
            | false -> snapshot.[idx]
          buf.Cells.[(offset + row) * buf.Width + col] <- cell
        | false -> ()

module TransitionDuration =
  let rec get (t: Transition) =
    match t with
    | Fade d | ColorMorph d | Dissolve d | Grow d -> int d
    | Wipe(_, d) | SlideIn(_, d) -> int d
    | Sequence ts -> ts |> List.sumBy get  // Duration is pre-computed for when Sequence is implemented; returns 0 for empty list
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
    (entering, exiting)
