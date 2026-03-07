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

  /// Measure intrinsic width of an arena node.
  let rec measureWidth (arena: FrameArena) (nodeIdx: int) : int =
    let node = arena.Nodes.[nodeIdx]
    match node.Kind with
    | 0uy -> 0 // Empty
    | 1uy -> // Text
      let mutable w = 0
      let text = System.String(arena.TextBuf, node.DataStart, node.DataLen)
      for rune in text.EnumerateRunes() do
        w <- w + RuneWidth.getColumnWidth rune
      w
    | 2uy -> // Row — sum of children widths
      let mutable total = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        total <- total + measureWidth arena idx
        idx <- arena.Nodes.[idx].NextSibling
      total
    | 3uy -> // Column — max of children widths
      let mutable maxW = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        let w = measureWidth arena idx
        maxW <- max maxW w
        idx <- arena.Nodes.[idx].NextSibling
      maxW
    | 4uy -> // Overlay — max of children widths
      let mutable maxW = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        let w = measureWidth arena idx
        maxW <- max maxW w
        idx <- arena.Nodes.[idx].NextSibling
      maxW
    | 5uy -> // Styled
      measureWidth arena node.FirstChild
    | 6uy -> // Constrained
      match node.ConstraintKind with
      | 0uy -> int node.ConstraintVal // Fixed → explicit width
      | 1uy -> max (int node.ConstraintVal) (measureWidth arena node.FirstChild) // Min
      | 2uy -> min (int node.ConstraintVal) (measureWidth arena node.FirstChild) // Max
      | _ -> measureWidth arena node.FirstChild
    | 7uy -> // Bordered
      measureWidth arena node.FirstChild + 2
    | 8uy -> // Padded
      let pad = unpackPadding node.DataStart node.DataLen
      measureWidth arena node.FirstChild + pad.Left + pad.Right
    | 9uy -> // Keyed
      measureWidth arena node.FirstChild
    | 11uy -> // Aligned — passthrough
      measureWidth arena node.FirstChild
    | 12uy -> // Gapped — passthrough
      measureWidth arena node.FirstChild
    | _ -> 0

  /// Measure intrinsic height of an arena node.
  let rec measureHeight (arena: FrameArena) (nodeIdx: int) : int =
    let node = arena.Nodes.[nodeIdx]
    match node.Kind with
    | 0uy -> 0 // Empty
    | 1uy -> 1 // Text — always 1 row
    | 2uy -> // Row — max of children heights
      let mutable maxH = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        let h = measureHeight arena idx
        maxH <- max maxH h
        idx <- arena.Nodes.[idx].NextSibling
      maxH
    | 3uy -> // Column — sum of children heights
      let mutable total = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        total <- total + measureHeight arena idx
        idx <- arena.Nodes.[idx].NextSibling
      total
    | 4uy -> // Overlay — max of children heights
      let mutable maxH = 0
      let mutable idx = node.FirstChild
      while idx >= 0 do
        let h = measureHeight arena idx
        maxH <- max maxH h
        idx <- arena.Nodes.[idx].NextSibling
      maxH
    | 5uy -> // Styled
      measureHeight arena node.FirstChild
    | 6uy -> // Constrained
      measureHeight arena node.FirstChild
    | 7uy -> // Bordered
      measureHeight arena node.FirstChild + 2
    | 8uy -> // Padded
      let pad = unpackPadding node.DataStart node.DataLen
      measureHeight arena node.FirstChild + pad.Top + pad.Bottom
    | 9uy -> // Keyed
      measureHeight arena node.FirstChild
    | 11uy -> // Aligned — passthrough
      measureHeight arena node.FirstChild
    | 12uy -> // Gapped — passthrough
      measureHeight arena node.FirstChild
    | _ -> 0

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
        let contentWidths = Array.zeroCreate<int> n
        let mutable idx = node.FirstChild
        let mutable i = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy ->
            constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
            childNodes.[i] <- cn.FirstChild
            contentWidths.[i] <- measureWidth arena cn.FirstChild
          | _ ->
            constraints.[i] <- Fill
            childNodes.[i] <- idx
            contentWidths.[i] <- measureWidth arena idx
          idx <- cn.NextSibling
          i <- i + 1
        let areas = Layout.splitHWithContent (Array.toList constraints) (Array.toList contentWidths) area
        List.iteri (fun j childArea ->
          render arena childNodes.[j] (Layout.intersectArea area childArea) inheritedFg inheritedBg inheritedAttrs buf) areas

      | 3uy -> // Column
        let n = countChildren arena node.FirstChild
        let constraints = Array.zeroCreate<Constraint> n
        let childNodes = Array.zeroCreate<int> n
        let contentHeights = Array.zeroCreate<int> n
        let mutable idx = node.FirstChild
        let mutable i = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy ->
            constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
            childNodes.[i] <- cn.FirstChild
            contentHeights.[i] <- measureHeight arena cn.FirstChild
          | _ ->
            constraints.[i] <- Fill
            childNodes.[i] <- idx
            contentHeights.[i] <- measureHeight arena idx
          idx <- cn.NextSibling
          i <- i + 1
        let areas = Layout.splitVWithContent (Array.toList constraints) (Array.toList contentHeights) area
        List.iteri (fun j childArea ->
          render arena childNodes.[j] (Layout.intersectArea area childArea) inheritedFg inheritedBg inheritedAttrs buf) areas

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

      | 9uy -> // Keyed (pass through to child + record hit area)
        match node.DataLen > 0 with
        | true ->
          let key = System.String(arena.TextBuf, node.DataStart, node.DataLen)
          arena.HitMap.Add({ X = area.X; Y = area.Y; Width = area.Width; Height = area.Height; Key = key })
        | false -> ()
        render arena node.FirstChild area inheritedFg inheritedBg inheritedAttrs buf

      | 10uy -> // Canvas
        let drawIdx = node.DataStart
        match drawIdx >= 0 && drawIdx < arena.CanvasDraws.Count with
        | true ->
          let config = arena.CanvasDraws.[drawIdx]
          CanvasRender.renderToBuffer config area buf
        | false -> ()

      | 11uy -> // Aligned
        let hAlign = match node.ConstraintKind with | 1uy -> HAlign.HCenter | 2uy -> HAlign.Right | _ -> HAlign.Left
        let vAlign = match int node.ConstraintVal with | 1 -> VAlign.VCenter | 2 -> VAlign.Bottom | _ -> VAlign.Top
        let cw = measureWidth arena node.FirstChild
        let ch = measureHeight arena node.FirstChild
        let aligned = Layout.alignArea hAlign vAlign cw ch area
        render arena node.FirstChild aligned inheritedFg inheritedBg inheritedAttrs buf

      | 12uy -> // Gapped
        let gap = int node.ConstraintVal
        let childNode = arena.Nodes.[node.FirstChild]
        match childNode.Kind with
        | 2uy -> // Row inside Gapped
          let n = countChildren arena childNode.FirstChild
          let constraints = Array.zeroCreate<Constraint> n
          let childNodes = Array.zeroCreate<int> n
          let contentWidths = Array.zeroCreate<int> n
          let mutable idx = childNode.FirstChild
          let mutable i = 0
          while idx >= 0 do
            let cn = arena.Nodes.[idx]
            match cn.Kind with
            | 6uy ->
              constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
              childNodes.[i] <- cn.FirstChild
              contentWidths.[i] <- measureWidth arena cn.FirstChild
            | _ ->
              constraints.[i] <- Fill
              childNodes.[i] <- idx
              contentWidths.[i] <- measureWidth arena idx
            idx <- cn.NextSibling
            i <- i + 1
          let areas = Layout.splitHWithGap gap (Array.toList constraints) (Array.toList contentWidths) area
          List.iteri (fun j childArea ->
            render arena childNodes.[j] (Layout.intersectArea area childArea) inheritedFg inheritedBg inheritedAttrs buf) areas
        | 3uy -> // Column inside Gapped
          let n = countChildren arena childNode.FirstChild
          let constraints = Array.zeroCreate<Constraint> n
          let childNodes = Array.zeroCreate<int> n
          let contentHeights = Array.zeroCreate<int> n
          let mutable idx = childNode.FirstChild
          let mutable i = 0
          while idx >= 0 do
            let cn = arena.Nodes.[idx]
            match cn.Kind with
            | 6uy ->
              constraints.[i] <- unpackConstraint cn.ConstraintKind cn.ConstraintVal
              childNodes.[i] <- cn.FirstChild
              contentHeights.[i] <- measureHeight arena cn.FirstChild
            | _ ->
              constraints.[i] <- Fill
              childNodes.[i] <- idx
              contentHeights.[i] <- measureHeight arena idx
            idx <- cn.NextSibling
            i <- i + 1
          let areas = Layout.splitVWithGap gap (Array.toList constraints) (Array.toList contentHeights) area
          List.iteri (fun j childArea ->
            render arena childNodes.[j] (Layout.intersectArea area childArea) inheritedFg inheritedBg inheritedAttrs buf) areas
        | _ ->
          render arena node.FirstChild area inheritedFg inheritedBg inheritedAttrs buf

      | _ -> ()

  let renderRoot (arena: FrameArena) (rootHandle: NodeHandle) (area: Area) (buf: Buffer) =
    render arena (NodeHandle.value rootHandle) area 0 0 0us buf

  let hitTest (arena: FrameArena) (x: int) (y: int) : string option =
    let mutable result = None
    for i in arena.HitMap.Count - 1 .. -1 .. 0 do
      match result with
      | Some _ -> ()
      | None ->
        let entry = arena.HitMap.[i]
        match x >= entry.X && x < entry.X + entry.Width && y >= entry.Y && y < entry.Y + entry.Height with
        | true -> result <- Some entry.Key
        | false -> ()
    result
