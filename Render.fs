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
