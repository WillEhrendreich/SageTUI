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

  /// Distribute remaining space to Fill children encoded as negative sizes.
  /// Fill children use ls.[szBase+i] = -(content+1). After this call, all entries are >= 0.
  let distributeFill (ls: int array) (szBase: int) (n: int) (fillCount: int) (fillContentTotal: int) (remaining: int) =
    if fillCount > 0 then
      if fillContentTotal <= remaining then
        let excess = remaining - fillContentTotal
        let perExtra = excess / fillCount
        let mutable extraRem = excess % fillCount
        for j in 0 .. n - 1 do
          if ls.[szBase + j] < 0 then
            let cw = -(ls.[szBase + j] + 1)
            let bonus = if extraRem > 0 then extraRem <- extraRem - 1; 1 else 0
            ls.[szBase + j] <- cw + perExtra + bonus
      else
        // Shrink path (rare: content exceeds available space)
        let pool = max 0 remaining
        let mutable used = 0
        for j in 0 .. n - 1 do
          if ls.[szBase + j] < 0 then
            let cw = -(ls.[szBase + j] + 1)
            let sz = if fillContentTotal > 0 then cw * pool / fillContentTotal else 0
            ls.[szBase + j] <- -(sz + 1)  // keep negative for bonus pass
            used <- used + sz
        let mutable bonusRem = pool - used
        for j in 0 .. n - 1 do
          if ls.[szBase + j] < 0 then
            let sz = -(ls.[szBase + j] + 1)
            let bonus = if bonusRem > 0 then bonusRem <- bonusRem - 1; 1 else 0
            ls.[szBase + j] <- sz + bonus

  /// Measure intrinsic width of an arena node.
  let rec measureWidth (arena: FrameArena) (nodeIdx: int) : int =
    let node = arena.Nodes.[nodeIdx]
    match node.Kind with
    | 0uy -> 0 // Empty
    | 1uy -> // Text — zero allocation: iterate rune widths directly from TextBuf span
      let mutable w = 0
      let span = System.ReadOnlySpan<char>(arena.TextBuf, node.DataStart, node.DataLen)
      for rune in System.MemoryExtensions.EnumerateRunes(span) do
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
    | 13uy -> // Responsive — measure first child (smallest breakpoint)
      match node.FirstChild with
      | -1 -> 0
      | wi -> measureWidth arena arena.Nodes.[wi].FirstChild
    | 15uy -> // ResponsiveH — measure first child (smallest breakpoint)
      match node.FirstChild with
      | -1 -> 0
      | wi -> measureWidth arena arena.Nodes.[wi].FirstChild
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
    | 13uy -> // Responsive — measure first child (smallest breakpoint)
      match node.FirstChild with
      | -1 -> 0
      | wi -> measureHeight arena arena.Nodes.[wi].FirstChild
    | 15uy -> // ResponsiveH — measure first child (smallest breakpoint)
      match node.FirstChild with
      | -1 -> 0
      | wi -> measureHeight arena arena.Nodes.[wi].FirstChild
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
        Buffer.writeCharSpan area.X area.Y fg bg attrs arena.TextBuf node.DataStart node.DataLen area.Width buf

      | 2uy -> // Row — uses LayoutScratch, zero managed allocation for common case
        let n = countChildren arena node.FirstChild
        let ls = arena.LayoutScratch
        let lp = arena.LayoutPos
        let cnBase = lp        // n slots: child node indices
        let szBase = lp + n   // n slots: sizes (negative = Fill with encoded content)
        arena.LayoutPos <- lp + n + n
        let mutable idx = node.FirstChild
        let mutable i = 0
        let mutable remaining = area.Width
        let mutable fillCount = 0
        let mutable fillContentTotal = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy -> // Constrained wrapper
            ls.[cnBase + i] <- cn.FirstChild
            match cn.ConstraintKind with
            | 0uy ->
              ls.[szBase + i] <- min (int cn.ConstraintVal) remaining
              remaining <- remaining - ls.[szBase + i]
            | 1uy ->
              ls.[szBase + i] <- int cn.ConstraintVal
              remaining <- remaining - ls.[szBase + i]
            | 2uy ->
              ls.[szBase + i] <- min (int cn.ConstraintVal) remaining
              remaining <- remaining - ls.[szBase + i]
            | 3uy ->
              ls.[szBase + i] <- area.Width * int cn.ConstraintVal / 100
              remaining <- remaining - ls.[szBase + i]
            | 4uy ->
              let cw = measureWidth arena cn.FirstChild
              ls.[szBase + i] <- -(cw + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + cw
            | 5uy ->
              let num = int cn.ConstraintVal >>> 8
              let den = int cn.ConstraintVal &&& 0xFF
              ls.[szBase + i] <- if den > 0 then area.Width * num / den else 0
              remaining <- remaining - ls.[szBase + i]
            | _ ->
              let cw = measureWidth arena cn.FirstChild
              ls.[szBase + i] <- -(cw + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + cw
          | _ -> // bare node → implicit Fill
            ls.[cnBase + i] <- idx
            let cw = measureWidth arena idx
            ls.[szBase + i] <- -(cw + 1)
            fillCount <- fillCount + 1
            fillContentTotal <- fillContentTotal + cw
          idx <- cn.NextSibling
          i <- i + 1
        distributeFill ls szBase n fillCount fillContentTotal remaining
        let mutable offset = 0
        for j in 0 .. n - 1 do
          let sz = ls.[szBase + j]
          let childArea = Layout.intersectArea area { X = area.X + offset; Y = area.Y; Width = sz; Height = area.Height }
          render arena ls.[cnBase + j] childArea inheritedFg inheritedBg inheritedAttrs buf
          offset <- offset + sz

      | 3uy -> // Column — uses LayoutScratch, zero managed allocation for common case
        let n = countChildren arena node.FirstChild
        let ls = arena.LayoutScratch
        let lp = arena.LayoutPos
        let cnBase = lp
        let szBase = lp + n
        arena.LayoutPos <- lp + n + n
        let mutable idx = node.FirstChild
        let mutable i = 0
        let mutable remaining = area.Height
        let mutable fillCount = 0
        let mutable fillContentTotal = 0
        while idx >= 0 do
          let cn = arena.Nodes.[idx]
          match cn.Kind with
          | 6uy ->
            ls.[cnBase + i] <- cn.FirstChild
            match cn.ConstraintKind with
            | 0uy ->
              ls.[szBase + i] <- min (int cn.ConstraintVal) remaining
              remaining <- remaining - ls.[szBase + i]
            | 1uy ->
              ls.[szBase + i] <- int cn.ConstraintVal
              remaining <- remaining - ls.[szBase + i]
            | 2uy ->
              ls.[szBase + i] <- min (int cn.ConstraintVal) remaining
              remaining <- remaining - ls.[szBase + i]
            | 3uy ->
              ls.[szBase + i] <- area.Height * int cn.ConstraintVal / 100
              remaining <- remaining - ls.[szBase + i]
            | 4uy ->
              let ch = measureHeight arena cn.FirstChild
              ls.[szBase + i] <- -(ch + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + ch
            | 5uy ->
              let num = int cn.ConstraintVal >>> 8
              let den = int cn.ConstraintVal &&& 0xFF
              ls.[szBase + i] <- if den > 0 then area.Height * num / den else 0
              remaining <- remaining - ls.[szBase + i]
            | _ ->
              let ch = measureHeight arena cn.FirstChild
              ls.[szBase + i] <- -(ch + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + ch
          | _ ->
            ls.[cnBase + i] <- idx
            let ch = measureHeight arena idx
            ls.[szBase + i] <- -(ch + 1)
            fillCount <- fillCount + 1
            fillContentTotal <- fillContentTotal + ch
          idx <- cn.NextSibling
          i <- i + 1
        distributeFill ls szBase n fillCount fillContentTotal remaining
        let mutable offset = 0
        for j in 0 .. n - 1 do
          let sz = ls.[szBase + j]
          let childArea = Layout.intersectArea area { X = area.X; Y = area.Y + offset; Width = area.Width; Height = sz }
          render arena ls.[cnBase + j] childArea inheritedFg inheritedBg inheritedAttrs buf
          offset <- offset + sz

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
        // Store (KeyStart, KeyLen) pairs — no string allocation per frame.
        // Materialise strings only when hitTest/keyAreas is called.
        match node.DataLen > 0 with
        | true ->
          arena.HitMap.Add({ X = area.X; Y = area.Y; Width = area.Width; Height = area.Height; KeyStart = node.DataStart; KeyLen = node.DataLen })
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
        | 2uy -> // Row inside Gapped — uses LayoutScratch
          let n = countChildren arena childNode.FirstChild
          let ls = arena.LayoutScratch
          let lp = arena.LayoutPos
          let cnBase = lp
          let szBase = lp + n
          arena.LayoutPos <- lp + n + n
          let usable = if n > 1 then max 0 (area.Width - gap * (n - 1)) else area.Width
          let mutable idx = childNode.FirstChild
          let mutable i = 0
          let mutable remaining = usable
          let mutable fillCount = 0
          let mutable fillContentTotal = 0
          while idx >= 0 do
            let cn = arena.Nodes.[idx]
            match cn.Kind with
            | 6uy ->
              ls.[cnBase + i] <- cn.FirstChild
              match cn.ConstraintKind with
              | 0uy -> ls.[szBase + i] <- min (int cn.ConstraintVal) remaining; remaining <- remaining - ls.[szBase + i]
              | 1uy -> ls.[szBase + i] <- int cn.ConstraintVal; remaining <- remaining - ls.[szBase + i]
              | 2uy -> ls.[szBase + i] <- min (int cn.ConstraintVal) remaining; remaining <- remaining - ls.[szBase + i]
              | 3uy -> ls.[szBase + i] <- usable * int cn.ConstraintVal / 100; remaining <- remaining - ls.[szBase + i]
              | 4uy ->
                let cw = measureWidth arena cn.FirstChild
                ls.[szBase + i] <- -(cw + 1)
                fillCount <- fillCount + 1
                fillContentTotal <- fillContentTotal + cw
              | 5uy ->
                let num = int cn.ConstraintVal >>> 8
                let den = int cn.ConstraintVal &&& 0xFF
                ls.[szBase + i] <- if den > 0 then usable * num / den else 0
                remaining <- remaining - ls.[szBase + i]
              | _ ->
                let cw = measureWidth arena cn.FirstChild
                ls.[szBase + i] <- -(cw + 1)
                fillCount <- fillCount + 1
                fillContentTotal <- fillContentTotal + cw
            | _ ->
              ls.[cnBase + i] <- idx
              let cw = measureWidth arena idx
              ls.[szBase + i] <- -(cw + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + cw
            idx <- cn.NextSibling
            i <- i + 1
          distributeFill ls szBase n fillCount fillContentTotal remaining
          let mutable offset = 0
          for j in 0 .. n - 1 do
            let sz = ls.[szBase + j]
            let gappedOffset = offset + j * gap
            let childArea = Layout.intersectArea area { X = area.X + gappedOffset; Y = area.Y; Width = sz; Height = area.Height }
            render arena ls.[cnBase + j] childArea inheritedFg inheritedBg inheritedAttrs buf
            offset <- offset + sz
        | 3uy -> // Column inside Gapped — uses LayoutScratch
          let n = countChildren arena childNode.FirstChild
          let ls = arena.LayoutScratch
          let lp = arena.LayoutPos
          let cnBase = lp
          let szBase = lp + n
          arena.LayoutPos <- lp + n + n
          let usable = if n > 1 then max 0 (area.Height - gap * (n - 1)) else area.Height
          let mutable idx = childNode.FirstChild
          let mutable i = 0
          let mutable remaining = usable
          let mutable fillCount = 0
          let mutable fillContentTotal = 0
          while idx >= 0 do
            let cn = arena.Nodes.[idx]
            match cn.Kind with
            | 6uy ->
              ls.[cnBase + i] <- cn.FirstChild
              match cn.ConstraintKind with
              | 0uy -> ls.[szBase + i] <- min (int cn.ConstraintVal) remaining; remaining <- remaining - ls.[szBase + i]
              | 1uy -> ls.[szBase + i] <- int cn.ConstraintVal; remaining <- remaining - ls.[szBase + i]
              | 2uy -> ls.[szBase + i] <- min (int cn.ConstraintVal) remaining; remaining <- remaining - ls.[szBase + i]
              | 3uy -> ls.[szBase + i] <- usable * int cn.ConstraintVal / 100; remaining <- remaining - ls.[szBase + i]
              | 4uy ->
                let ch = measureHeight arena cn.FirstChild
                ls.[szBase + i] <- -(ch + 1)
                fillCount <- fillCount + 1
                fillContentTotal <- fillContentTotal + ch
              | 5uy ->
                let num = int cn.ConstraintVal >>> 8
                let den = int cn.ConstraintVal &&& 0xFF
                ls.[szBase + i] <- if den > 0 then usable * num / den else 0
                remaining <- remaining - ls.[szBase + i]
              | _ ->
                let ch = measureHeight arena cn.FirstChild
                ls.[szBase + i] <- -(ch + 1)
                fillCount <- fillCount + 1
                fillContentTotal <- fillContentTotal + ch
            | _ ->
              ls.[cnBase + i] <- idx
              let ch = measureHeight arena idx
              ls.[szBase + i] <- -(ch + 1)
              fillCount <- fillCount + 1
              fillContentTotal <- fillContentTotal + ch
            idx <- cn.NextSibling
            i <- i + 1
          distributeFill ls szBase n fillCount fillContentTotal remaining
          let mutable offset = 0
          for j in 0 .. n - 1 do
            let sz = ls.[szBase + j]
            let gappedOffset = offset + j * gap
            let childArea = Layout.intersectArea area { X = area.X; Y = area.Y + gappedOffset; Width = area.Width; Height = sz }
            render arena ls.[cnBase + j] childArea inheritedFg inheritedBg inheritedAttrs buf
            offset <- offset + sz
        | _ ->
          render arena node.FirstChild area inheritedFg inheritedBg inheritedAttrs buf

      | 13uy -> // Responsive — pick last breakpoint where minWidth ≤ area.Width
        let mutable selected = -1
        let mutable wIdx = node.FirstChild
        while wIdx >= 0 do
          let wrapper = arena.Nodes.[wIdx]
          match wrapper.Kind = 14uy && area.Width >= int wrapper.ConstraintVal with
          | true  -> selected <- wrapper.FirstChild
          | false -> ()
          wIdx <- wrapper.NextSibling
        // Fallback to first breakpoint if nothing matched
        let target =
          match selected with
          | -1 ->
            match node.FirstChild with
            | -1 -> -1
            | wi -> arena.Nodes.[wi].FirstChild
          | idx -> idx
        match target with
        | -1 -> ()
        | idx -> render arena idx area inheritedFg inheritedBg inheritedAttrs buf

      | 15uy -> // ResponsiveH — pick last breakpoint where minHeight ≤ area.Height
        let mutable selected = -1
        let mutable wIdx = node.FirstChild
        while wIdx >= 0 do
          let wrapper = arena.Nodes.[wIdx]
          match wrapper.Kind = 16uy && area.Height >= int wrapper.ConstraintVal with
          | true  -> selected <- wrapper.FirstChild
          | false -> ()
          wIdx <- wrapper.NextSibling
        let target =
          match selected with
          | -1 ->
            match node.FirstChild with
            | -1 -> -1
            | wi -> arena.Nodes.[wi].FirstChild
          | idx -> idx
        match target with
        | -1 -> ()
        | idx -> render arena idx area inheritedFg inheritedBg inheritedAttrs buf

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
        | true -> result <- Some (System.String(arena.TextBuf, entry.KeyStart, entry.KeyLen))
        | false -> ()
    result

  let keyAreas (arena: FrameArena) : Map<string, Area> =
    let mutable areas = Map.empty
    for i in 0 .. arena.HitMap.Count - 1 do
      let entry = arena.HitMap.[i]
      let key = System.String(arena.TextBuf, entry.KeyStart, entry.KeyLen)
      areas <-
        Map.add
          key
          { X = entry.X
            Y = entry.Y
            Width = entry.Width
            Height = entry.Height }
          areas
    areas
