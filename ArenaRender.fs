namespace SageTUI

open System.Text

module ArenaRender =
  let unpackConstraint (kind: byte) (value: int16) : Constraint =
    match kind with
    | 0uy -> Fixed (int value)
    | 1uy -> Min (int value)
    | 2uy -> Max (int value)
    | 3uy -> Percentage (int value)
    | 4uy -> Fill
    | 5uy -> Ratio(int value >>> 8, int value &&& 0xFF)
    | _ -> Fill

  let unpackBorderStyle (kind: byte) : BorderStyle =
    match kind with
    | 0uy -> Light
    | 1uy -> Heavy
    | 2uy -> Double
    | 3uy -> Rounded
    | _ -> BorderStyle.Ascii

  let unpackPadding (ds: int) (dl: int) : Padding =
    { Top = ds >>> 16; Right = ds &&& 0xFFFF
      Bottom = dl >>> 16; Left = dl &&& 0xFFFF }

  let countChildren (arena: FrameArena) (firstChild: int) =
    let mutable count = 0
    let mutable idx = firstChild
    while idx >= 0 do
      count <- count + 1
      idx <- arena.Nodes.[idx].NextSibling
    count

  let rec render (arena: FrameArena) (nodeIdx: int) (area: Area) (inheritedFg: int) (inheritedBg: int) (inheritedAttrs: uint16) (buf: Buffer) =
    match area.Width <= 0 || area.Height <= 0 with
    | true -> ()
    | false ->
      let node = arena.Nodes.[nodeIdx]
      match node.Kind with
      | 0uy -> () // Empty

      | 1uy -> // Text
        let fg =
          match Arena.unpackStyleFg node.StylePacked with
          | 0 -> inheritedFg
          | v -> v
        let bg =
          match Arena.unpackStyleBg node.StylePacked with
          | 0 -> inheritedBg
          | v -> v
        let attrs =
          match node.AttrsPacked with
          | 0us -> inheritedAttrs
          | v -> v ||| inheritedAttrs
        let text = System.String(arena.TextBuf, node.DataStart, node.DataLen)
        let mutable col = 0
        let sb = StringBuilder()
        for rune in text.EnumerateRunes() do
          let w = RuneWidth.getColumnWidth rune
          match col + w <= area.Width with
          | true ->
            sb.Append(rune.ToString()) |> ignore
            col <- col + w
          | false -> ()
        Buffer.writeString area.X area.Y fg bg attrs (sb.ToString()) buf

      | 2uy -> // Row
        let n = countChildren arena node.FirstChild
        let constraints = Array.zeroCreate<Constraint> n
        let childNodes = Array.zeroCreate<int> n
        let mutable idx = node.FirstChild
        let mutable i = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy ->
            constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
            childNodes.[i] <- cn.FirstChild
          | _ ->
            constraints.[i] <- Fill
            childNodes.[i] <- idx
          idx <- cn.NextSibling
          i <- i + 1
        let areas = Layout.splitH (Array.toList constraints) area
        List.iteri (fun j childArea ->
          render arena childNodes.[j] childArea inheritedFg inheritedBg inheritedAttrs buf) areas

      | 3uy -> // Column
        let n = countChildren arena node.FirstChild
        let constraints = Array.zeroCreate<Constraint> n
        let childNodes = Array.zeroCreate<int> n
        let mutable idx = node.FirstChild
        let mutable i = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy ->
            constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
            childNodes.[i] <- cn.FirstChild
          | _ ->
            constraints.[i] <- Fill
            childNodes.[i] <- idx
          idx <- cn.NextSibling
          i <- i + 1
        let areas = Layout.splitV (Array.toList constraints) area
        List.iteri (fun j childArea ->
          render arena childNodes.[j] childArea inheritedFg inheritedBg inheritedAttrs buf) areas

      | 4uy -> // Overlay
        let mutable idx = node.FirstChild
        while idx >= 0 do
          render arena idx area inheritedFg inheritedBg inheritedAttrs buf
          idx <- arena.Nodes.[idx].NextSibling

      | 5uy -> // Styled
        let fg =
          match Arena.unpackStyleFg node.StylePacked with
          | 0 -> inheritedFg
          | v -> v
        let bg =
          match Arena.unpackStyleBg node.StylePacked with
          | 0 -> inheritedBg
          | v -> v
        let attrs = node.AttrsPacked ||| inheritedAttrs
        render arena node.FirstChild area fg bg attrs buf

      | 6uy -> // Constrained
        let c = unpackConstraint node.ConstraintKind node.ConstraintVal
        let constrained = Layout.applyConstraint c area
        render arena node.FirstChild constrained inheritedFg inheritedBg inheritedAttrs buf

      | 7uy -> // Bordered
        let bs = unpackBorderStyle node.ConstraintKind
        BorderRender.renderBorder bs Style.empty area buf
        let inner = Layout.shrinkForBorder area
        render arena node.FirstChild inner inheritedFg inheritedBg inheritedAttrs buf

      | 8uy -> // Padded
        let pad = unpackPadding node.DataStart node.DataLen
        let inner =
          { X = area.X + pad.Left; Y = area.Y + pad.Top
            Width = max 0 (area.Width - pad.Left - pad.Right)
            Height = max 0 (area.Height - pad.Top - pad.Bottom) }
        render arena node.FirstChild inner inheritedFg inheritedBg inheritedAttrs buf

      | 9uy -> // Keyed (pass through to child)
        render arena node.FirstChild area inheritedFg inheritedBg inheritedAttrs buf

      | 10uy -> eprintfn "Canvas rendering not yet implemented"
      | _ -> ()

  let renderRoot (arena: FrameArena) (rootHandle: NodeHandle) (area: Area) (buf: Buffer) =
    render arena (NodeHandle.value rootHandle) area 0 0 0us buf
