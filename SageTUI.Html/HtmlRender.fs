namespace SageTUI.Html

open Falco.Markup
open SageTUI

/// Renders SageTUI Element trees to Falco.Markup XmlNode (HTML/CSS)
module HtmlRender =

  let colorToCss (c: Color) =
    match c with
    | Color.Default -> "inherit"
    | Color.Named (bc, intensity) ->
      let name =
        match bc with
        | BaseColor.Black -> "black"
        | BaseColor.Red -> "red"
        | BaseColor.Green -> "green"
        | BaseColor.Yellow -> "yellow"
        | BaseColor.Blue -> "blue"
        | BaseColor.Magenta -> "magenta"
        | BaseColor.Cyan -> "cyan"
        | BaseColor.White -> "white"
      match intensity with
      | Intensity.Normal -> name
      | Intensity.Bright -> sprintf "light%s" name
    | Color.Ansi256 idx -> sprintf "var(--ansi-%d)" idx
    | Color.Rgb (r, g, b) -> sprintf "rgb(%d,%d,%d)" r g b

  let styleToCss (s: Style) =
    [ match s.Fg with
      | Some c -> yield sprintf "color:%s" (colorToCss c)
      | None -> ()
      match s.Bg with
      | Some c -> yield sprintf "background-color:%s" (colorToCss c)
      | None -> ()
      if TextAttrs.has TextAttrs.bold s.Attrs then yield "font-weight:bold"
      if TextAttrs.has TextAttrs.dim s.Attrs then yield "opacity:0.5"
      if TextAttrs.has TextAttrs.italic s.Attrs then yield "font-style:italic"
      let decorations =
        [ if TextAttrs.has TextAttrs.underline s.Attrs then yield "underline"
          if TextAttrs.has TextAttrs.strikethrough s.Attrs then yield "line-through" ]
      match decorations with
      | [] -> ()
      | ds -> yield sprintf "text-decoration:%s" (String.concat " " ds)
      if TextAttrs.has TextAttrs.reverse s.Attrs then yield "filter:invert(1)" ]
    |> String.concat ";"

  let constraintToCss (c: Constraint) =
    match c with
    | Fixed n -> sprintf "width:%dch;flex:0 0 %dch" n n
    | Min n -> sprintf "min-width:%dch" n
    | Max n -> sprintf "max-width:%dch" n
    | Percentage p -> sprintf "width:%d%%" p
    | Fill -> "flex:1 1 auto"
    | Ratio (n, d) -> sprintf "flex:%d %d auto" n d

  let borderStyleToCss (bs: BorderStyle) =
    match bs with
    | Light -> "border:1px solid"
    | Heavy -> "border:2px solid"
    | Double -> "border:3px double"
    | Rounded -> "border:1px solid;border-radius:4px"
    | BorderStyle.Ascii -> "border:1px dashed"

  let paddingToCss (p: Padding) =
    sprintf "padding:%dch %dch %dch %dch" p.Top p.Right p.Bottom p.Left

  let rec render (el: Element) : XmlNode =
    match el with
    | Element.Empty ->
      Text.raw ""
    | Text (s, style) ->
      let css = styleToCss style
      match css with
      | "" -> Elem.span [] [ Text.raw s ]
      | css -> Elem.span [ Attr.style css ] [ Text.raw s ]
    | Row children ->
      Elem.div
        [ Attr.style "display:flex;flex-direction:row" ]
        (children |> List.map render)
    | Column children ->
      Elem.div
        [ Attr.style "display:flex;flex-direction:column" ]
        (children |> List.map render)
    | Overlay children ->
      Elem.div
        [ Attr.style "position:relative" ]
        (children |> List.mapi (fun i c ->
          match i with
          | 0 -> render c
          | _ -> Elem.div [ Attr.style "position:absolute;inset:0" ] [ render c ]))
    | Styled (style, child) ->
      let css = styleToCss style
      match css with
      | "" -> render child
      | css -> Elem.div [ Attr.style css ] [ render child ]
    | Constrained (con, child) ->
      Elem.div [ Attr.style (constraintToCss con) ] [ render child ]
    | Bordered (bs, _, child) ->
      Elem.div [ Attr.style (borderStyleToCss bs) ] [ render child ]
    | Padded (p, child) ->
      Elem.div [ Attr.style (paddingToCss p) ] [ render child ]
    | Keyed (key, _, _, child) ->
      Elem.div [ Attr.id key ] [ render child ]
    | Canvas _ ->
      Elem.div
        [ Attr.style "background:#333;color:#999" ]
        [ Text.raw "[canvas]" ]
    | Aligned (hAlign, vAlign, child) ->
      let hCss = match hAlign with HAlign.Left -> "flex-start" | HAlign.HCenter -> "center" | HAlign.Right -> "flex-end"
      let vCss = match vAlign with VAlign.Top -> "flex-start" | VAlign.VCenter -> "center" | VAlign.Bottom -> "flex-end"
      Elem.div
        [ Attr.style (sprintf "display:flex;justify-content:%s;align-items:%s" hCss vCss) ]
        [ render child ]
    | Gapped (gap, child) ->
      Elem.div
        [ Attr.style (sprintf "gap:%dch" gap) ]
        [ render child ]
    | Responsive breakpoints ->
      // In HTML, render all breakpoints with container queries via CSS classes;
      // simple fallback: render the first breakpoint
      let child =
        breakpoints |> List.tryHead |> Option.map snd |> Option.defaultValue Element.Empty
      Elem.div
        [ Attr.style "display:contents" ]
        [ render child ]
    | ResponsiveH breakpoints ->
      // Height breakpoints: simple fallback to first breakpoint in HTML context
      let child =
        breakpoints |> List.tryHead |> Option.map snd |> Option.defaultValue Element.Empty
      Elem.div
        [ Attr.style "display:contents" ]
        [ render child ]

  /// Render Element to HTML string
  let renderToString (el: Element) : string =
    renderNode (render el)
