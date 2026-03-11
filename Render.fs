namespace SageTUI

open System.Text

module Render =
  let extractConstraint (elem: Element) =
    match elem with
    | Constrained(c, _) -> c
    | _ -> Fill 1

  let unwrapConstrained (elem: Element) =
    match elem with
    | Constrained(_, child) -> child
    | other -> other

  let rec render (area: Area) (inheritedStyle: Style) (buf: Buffer) (elem: Element) =
    match area.Width <= 0 || area.Height <= 0 with
    | true -> ()
    | false ->
      match elem with
      | Empty -> ()

      | Text(text, localStyle) ->
        let resolved = Style.merge inheritedStyle localStyle
        let fg = resolved.Fg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let bg = resolved.Bg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let attrs = resolved.Attrs.Value
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

      | Row children ->
        let constraints = children |> List.map extractConstraint
        let unwrapped = children |> List.map unwrapConstrained
        let contentWidths = Measure.childWidths children
        let areas = Layout.splitHWithContent constraints contentWidths area
        List.iter2 (fun childArea child ->
          render (Layout.intersectArea area childArea) inheritedStyle buf child) areas unwrapped

      | Column children ->
        let constraints = children |> List.map extractConstraint
        let unwrapped = children |> List.map unwrapConstrained
        let contentHeights = Measure.childHeights children
        let areas = Layout.splitVWithContent constraints contentHeights area
        List.iter2 (fun childArea child ->
          render (Layout.intersectArea area childArea) inheritedStyle buf child) areas unwrapped

      | Overlay layers ->
        layers |> List.iter (render area inheritedStyle buf)

      | Styled(style, child) ->
        render area (Style.merge inheritedStyle style) buf child

      | Constrained(c, child) ->
        let constrained = Layout.applyConstraint c area
        render constrained inheritedStyle buf child

      | Bordered(borderStyle, title, child) ->
        BorderRender.renderBorder borderStyle title inheritedStyle area buf
        let inner = Layout.shrinkForBorder area
        render inner inheritedStyle buf child

      | Padded(padding, child) ->
        let inner =
          { X = area.X + padding.Left
            Y = area.Y + padding.Top
            Width = max 0 (area.Width - padding.Left - padding.Right)
            Height = max 0 (area.Height - padding.Top - padding.Bottom) }
        render inner inheritedStyle buf child

      | Keyed(_, _, _, child) ->
        render area inheritedStyle buf child

      | Canvas config ->
        CanvasRender.renderToBuffer config area buf

      | Aligned(hAlign, vAlign, child) ->
        let cw = Measure.measureWidth child
        let ch = Measure.measureHeight child
        let aligned = Layout.alignArea hAlign vAlign cw ch area
        render aligned inheritedStyle buf child

      | Gapped(gap, child) ->
        match child with
        | Row children ->
          let constraints = children |> List.map extractConstraint
          let unwrapped = children |> List.map unwrapConstrained
          let contentWidths = Measure.childWidths children
          let areas = Layout.splitHWithGap gap constraints contentWidths area
          List.iter2 (fun childArea c ->
            render (Layout.intersectArea area childArea) inheritedStyle buf c) areas unwrapped
        | Column children ->
          let constraints = children |> List.map extractConstraint
          let unwrapped = children |> List.map unwrapConstrained
          let contentHeights = Measure.childHeights children
          let areas = Layout.splitVWithGap gap constraints contentHeights area
          List.iter2 (fun childArea c ->
            render (Layout.intersectArea area childArea) inheritedStyle buf c) areas unwrapped
        | other ->
          render area inheritedStyle buf other

      | Responsive breakpoints ->
        let selected =
          breakpoints
          |> List.filter (fun (minW, _) -> area.Width >= minW)
          |> List.tryLast
          |> Option.map snd
        match selected with
        | Some child -> render area inheritedStyle buf child
        | None ->
          match List.tryHead breakpoints with
          | Some (_, child) -> render area inheritedStyle buf child
          | None -> ()
      | ResponsiveH breakpoints ->
        let selected =
          breakpoints
          |> List.filter (fun (minH, _) -> area.Height >= minH)
          |> List.tryLast
          |> Option.map snd
        match selected with
        | Some child -> render area inheritedStyle buf child
        | None ->
          match List.tryHead breakpoints with
          | Some (_, child) -> render area inheritedStyle buf child
          | None -> ()

      | Scroll(offset, child) ->
        let naturalH = max area.Height (Measure.measureHeight child)
        let vBuf = Buffer.create area.Width naturalH
        render { X = 0; Y = 0; Width = area.Width; Height = naturalH } inheritedStyle vBuf child
        let safeOffset = max 0 (min offset (naturalH - area.Height))
        for row = 0 to area.Height - 1 do
          let srcRow = safeOffset + row
          for col = 0 to area.Width - 1 do
            Buffer.set (area.X + col) (area.Y + row) (Buffer.get col srcRow vBuf) buf

      | Filled localStyle ->
        let resolved = Style.merge inheritedStyle localStyle
        let fg = resolved.Fg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let bg = resolved.Bg |> Option.map PackedColor.pack |> Option.defaultValue 0
        let attrs = resolved.Attrs.Value
        let spaceRune = int (System.Text.Rune ' ').Value
        for row = area.Y to area.Y + area.Height - 1 do
          for col = area.X to area.X + area.Width - 1 do
            Buffer.set col row { Rune = spaceRune; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us } buf

  /// Collect debug nodes from the element tree by performing a layout pass without rendering.
  /// Returns a list of DebugNode records capturing the resolved bounds of every element,
  /// which can be fed to Buffer.applyDebugOverlay to draw a non-perturbing debug frame.
  let collectDebugNodes (width: int) (height: int) (elem: Element) : DebugNode list =
    match width <= 0 || height <= 0 with
    | true -> []
    | false ->
      let nodes = System.Collections.Generic.List<DebugNode>()
      let area0 = { X = 0; Y = 0; Width = width; Height = height }

      let computeTag e =
        match e with
        | Empty              -> "·"
        | Text(s, _)         -> sprintf "T\"%s\"" (if s.Length > 8 then s.[..7] + "…" else s)
        | Row _              -> "Row"
        | Column _           -> "Col"
        | Overlay _          -> "Ov"
        | Styled _           -> "Sty"
        | Constrained _      -> "Con"
        | Bordered _         -> "Brd"
        | Padded _           -> "Pad"
        | Keyed(k, _, _, _)  -> sprintf "Key:%s" (if k.Length > 6 then k.[..5] + "…" else k)
        | Canvas _           -> "Canvas"
        | Aligned _          -> "Align"
        | Gapped _           -> "Gap"
        | Responsive _       -> "Rsp"
        | ResponsiveH _      -> "RspH"
        | Scroll _           -> "Scrl"
        | Filled _           -> "Filled"

      // cFromParent: the Constraint already resolved by the parent's Row/Column split
      // When Some, the passed area is already constraint-resolved; do NOT re-apply.
      // When None, check if elem itself is Constrained and apply it.
      let rec collect (area: Area) (depth: int) (cFromParent: Constraint option) (elem: Element) =
        match area.Width <= 0 || area.Height <= 0 with
        | true -> ()
        | false ->
          let actualElem = unwrapConstrained elem
          let constr =
            match cFromParent with
            | Some _ -> cFromParent
            | None ->
              match elem with
              | Constrained(c, _) -> Some c
              | _ -> None
          let resolvedArea =
            match cFromParent with
            | Some _ -> area  // parent already resolved the area
            | None ->
              match elem with
              | Constrained(c, _) -> Layout.applyConstraint c area
              | _ -> area
          let tag = computeTag actualElem
          nodes.Add { Tag = tag; Bounds = resolvedArea; Constraint = constr; Depth = depth }

          match actualElem with
          | Empty | Text _ | Canvas _ | Filled _ -> ()

          | Row children ->
            let contentWidths = Measure.childWidths children
            let areas = Layout.splitHWithContent (children |> List.map extractConstraint) contentWidths resolvedArea
            List.iter2 (fun childArea child ->
              let cc = match child with Constrained(c2, _) -> Some c2 | _ -> None
              collect (Layout.intersectArea resolvedArea childArea) (depth + 1) cc (unwrapConstrained child)
            ) areas children

          | Column children ->
            let contentHeights = Measure.childHeights children
            let areas = Layout.splitVWithContent (children |> List.map extractConstraint) contentHeights resolvedArea
            List.iter2 (fun childArea child ->
              let cc = match child with Constrained(c2, _) -> Some c2 | _ -> None
              collect (Layout.intersectArea resolvedArea childArea) (depth + 1) cc (unwrapConstrained child)
            ) areas children

          | Overlay layers ->
            layers |> List.iter (fun l ->
              let cc = match l with Constrained(c2, _) -> Some c2 | _ -> None
              collect resolvedArea (depth + 1) cc (unwrapConstrained l))

          | Styled(_, child) ->
            collect resolvedArea (depth + 1) None (unwrapConstrained child)

          | Constrained _ -> ()  // unreachable after unwrapConstrained

          | Bordered(_, _, child) ->
            let inner = Layout.shrinkForBorder resolvedArea
            collect inner (depth + 1) None (unwrapConstrained child)

          | Padded(padding, child) ->
            let inner = {
              X = resolvedArea.X + padding.Left
              Y = resolvedArea.Y + padding.Top
              Width  = max 0 (resolvedArea.Width  - padding.Left - padding.Right)
              Height = max 0 (resolvedArea.Height - padding.Top  - padding.Bottom)
            }
            collect inner (depth + 1) None (unwrapConstrained child)

          | Keyed(_, _, _, child) ->
            collect resolvedArea (depth + 1) None (unwrapConstrained child)

          | Aligned(hAlign, vAlign, child) ->
            let cw = Measure.measureWidth child
            let ch = Measure.measureHeight child
            let aligned = Layout.alignArea hAlign vAlign cw ch resolvedArea
            collect aligned (depth + 1) None (unwrapConstrained child)

          | Gapped(gap, child) ->
            match child with
            | Row children ->
              let contentWidths = Measure.childWidths children
              let areas = Layout.splitHWithGap gap (children |> List.map extractConstraint) contentWidths resolvedArea
              List.iter2 (fun childArea c ->
                let cc = match c with Constrained(c2, _) -> Some c2 | _ -> None
                collect (Layout.intersectArea resolvedArea childArea) (depth + 1) cc (unwrapConstrained c)
              ) areas children
            | Column children ->
              let contentHeights = Measure.childHeights children
              let areas = Layout.splitVWithGap gap (children |> List.map extractConstraint) contentHeights resolvedArea
              List.iter2 (fun childArea c ->
                let cc = match c with Constrained(c2, _) -> Some c2 | _ -> None
                collect (Layout.intersectArea resolvedArea childArea) (depth + 1) cc (unwrapConstrained c)
              ) areas children
            | other ->
              collect resolvedArea (depth + 1) None (unwrapConstrained other)

          | Responsive breakpoints ->
            let selected =
              breakpoints
              |> List.filter (fun (minW, _) -> resolvedArea.Width >= minW)
              |> List.tryLast |> Option.map snd
            match selected with
            | Some child -> collect resolvedArea (depth + 1) None (unwrapConstrained child)
            | None ->
              match List.tryHead breakpoints with
              | Some(_, child) -> collect resolvedArea (depth + 1) None (unwrapConstrained child)
              | None -> ()

          | ResponsiveH breakpoints ->
            let selected =
              breakpoints
              |> List.filter (fun (minH, _) -> resolvedArea.Height >= minH)
              |> List.tryLast |> Option.map snd
            match selected with
            | Some child -> collect resolvedArea (depth + 1) None (unwrapConstrained child)
            | None ->
              match List.tryHead breakpoints with
              | Some(_, child) -> collect resolvedArea (depth + 1) None (unwrapConstrained child)
              | None -> ()

          | Scroll(_, child) ->
            let naturalH = max resolvedArea.Height (Measure.measureHeight child)
            let vArea = { resolvedArea with Height = naturalH }
            collect vArea (depth + 1) None (unwrapConstrained child)

      collect area0 0 None elem
      nodes |> Seq.toList

  /// Draw a non-perturbing debug overlay over a rendered buffer.
  /// Each node produces a colored box border at its resolved bounds.
  /// Colors cycle by depth: Cyan→Magenta→Yellow→Green→Red→Blue (all Bright).
  /// Outer nodes (smaller depth) draw first; inner nodes draw on top.
  /// Call after Render.render to overlay without perturbing the layout pass.
  let applyDebugOverlay (nodes: DebugNode list) (buf: Buffer) : unit =
    let debugColors = [|
      PackedColor.pack (Named(Cyan,    Bright))
      PackedColor.pack (Named(Magenta, Bright))
      PackedColor.pack (Named(Yellow,  Bright))
      PackedColor.pack (Named(Green,   Bright))
      PackedColor.pack (Named(Red,     Bright))
      PackedColor.pack (Named(Blue,    Bright))
    |]
    let boxH  = 0x2500  // ─
    let boxV  = 0x2502  // │
    let boxTL = 0x256D  // ╭
    let boxTR = 0x256E  // ╮
    let boxBL = 0x2570  // ╰
    let boxBR = 0x256F  // ╯

    let setCell x y rune fg =
      if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height then
        let i = y * buf.Width + x
        buf.Cells[i] <- { buf.Cells[i] with Rune = rune; Fg = fg }

    let tintCell x y fg =
      if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height then
        let i = y * buf.Width + x
        buf.Cells[i] <- { buf.Cells[i] with Fg = fg }

    let sorted = nodes |> List.sortBy (fun n -> n.Depth)

    for node in sorted do
      let fg = debugColors[node.Depth % debugColors.Length]
      let b  = node.Bounds
      let x0 = b.X
      let y0 = b.Y
      let x1 = b.X + b.Width  - 1
      let y1 = b.Y + b.Height - 1

      match b.Width, b.Height with
      | w, _ when w <= 0 -> ()
      | _, h when h <= 0 -> ()
      | 1, 1 ->
        tintCell x0 y0 fg
      | 1, _ ->
        for y in y0 .. y1 do setCell x0 y boxV fg
      | _, 1 ->
        for x in x0 .. x1 do setCell x y0 boxH fg
      | _ ->
        setCell x0 y0 boxTL fg
        setCell x1 y0 boxTR fg
        setCell x0 y1 boxBL fg
        setCell x1 y1 boxBR fg
        for x in x0 + 1 .. x1 - 1 do
          setCell x y0 boxH fg
          setCell x y1 boxH fg
        for y in y0 + 1 .. y1 - 1 do
          setCell x0 y boxV fg
          setCell x1 y boxV fg
