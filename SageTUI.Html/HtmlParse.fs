namespace SageTUI.Html

open Falco.Markup
open SageTUI

/// Converts Falco.Markup XmlNode (HTML) to SageTUI Element trees for terminal rendering
module HtmlParse =

  let private tryParseColor (s: string) : Color option =
    match s.Trim().ToLowerInvariant() with
    | "inherit" | "" -> None
    | "black" -> Some (Color.Named(BaseColor.Black, Intensity.Normal))
    | "red" -> Some (Color.Named(BaseColor.Red, Intensity.Normal))
    | "green" -> Some (Color.Named(BaseColor.Green, Intensity.Normal))
    | "yellow" -> Some (Color.Named(BaseColor.Yellow, Intensity.Normal))
    | "blue" -> Some (Color.Named(BaseColor.Blue, Intensity.Normal))
    | "magenta" -> Some (Color.Named(BaseColor.Magenta, Intensity.Normal))
    | "cyan" -> Some (Color.Named(BaseColor.Cyan, Intensity.Normal))
    | "white" -> Some (Color.Named(BaseColor.White, Intensity.Normal))
    | "lightblack" -> Some (Color.Named(BaseColor.Black, Intensity.Bright))
    | "lightred" -> Some (Color.Named(BaseColor.Red, Intensity.Bright))
    | "lightgreen" -> Some (Color.Named(BaseColor.Green, Intensity.Bright))
    | "lightyellow" -> Some (Color.Named(BaseColor.Yellow, Intensity.Bright))
    | "lightblue" -> Some (Color.Named(BaseColor.Blue, Intensity.Bright))
    | "lightmagenta" -> Some (Color.Named(BaseColor.Magenta, Intensity.Bright))
    | "lightcyan" -> Some (Color.Named(BaseColor.Cyan, Intensity.Bright))
    | "lightwhite" -> Some (Color.Named(BaseColor.White, Intensity.Bright))
    | s when s.StartsWith("rgb(") && s.EndsWith(")") ->
      let parts = s.[4..s.Length-2].Split(',')
      match parts with
      | [| r; g; b |] ->
        match System.Byte.TryParse(r.Trim()), System.Byte.TryParse(g.Trim()), System.Byte.TryParse(b.Trim()) with
        | (true, rv), (true, gv), (true, bv) -> Some (Color.Rgb(rv, gv, bv))
        | _ -> None
      | _ -> None
    | s when s.StartsWith("oklch(") && s.EndsWith(")") ->
      let inner = s.[6..s.Length-2].Trim()
      let parts = inner.Split([|' '; ','|], System.StringSplitOptions.RemoveEmptyEntries)
      match parts with
      | [| lStr; cStr; hStr |] ->
        let lVal =
          match lStr.EndsWith("%") with
          | true -> System.Double.TryParse(lStr.[..lStr.Length-2]) |> fun (ok, v) -> match ok with true -> Some (v / 100.0) | false -> None
          | false -> System.Double.TryParse(lStr) |> fun (ok, v) -> match ok with true -> Some v | false -> None
        let cVal = System.Double.TryParse(cStr) |> fun (ok, v) -> match ok with true -> Some v | false -> None
        let hRaw = match hStr.EndsWith("deg") with true -> hStr.[..hStr.Length-4] | false -> hStr
        let hVal = System.Double.TryParse(hRaw) |> fun (ok, v) -> match ok with true -> Some v | false -> None
        match lVal, cVal, hVal with
        | Some l, Some c, Some h ->
          let (r, g, b) = Oklch.toRgb l c h
          Some (Color.Rgb(r, g, b))
        | _ -> None
      | _ -> None
    | _ -> None

  let private parseStylePairs (css: string) : (string * string) list =
    css.Split(';')
    |> Array.choose (fun pair ->
      match pair.IndexOf(':') with
      | -1 -> None
      | i -> Some (pair.[..i-1].Trim().ToLowerInvariant(), pair.[i+1..].Trim()))
    |> Array.toList

  let private buildStyle (pairs: (string * string) list) : Style =
    let mutable style = Style.empty
    for (k, v) in pairs do
      match k with
      | "color" ->
        match tryParseColor v with
        | Some c -> style <- { style with Fg = Some c }
        | None -> ()
      | "background-color" ->
        match tryParseColor v with
        | Some c -> style <- { style with Bg = Some c }
        | None -> ()
      | "font-weight" when v = "bold" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.bold style.Attrs }
      | "font-style" when v = "italic" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.italic style.Attrs }
      | "text-decoration" when v.Contains("underline") ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.underline style.Attrs }
      | "text-decoration" when v.Contains("line-through") ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.strikethrough style.Attrs }
      | "opacity" when v = "0.5" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.dim style.Attrs }
      | _ -> ()
    style

  let private getAttrValue (name: string) (attrs: XmlAttribute list) : string option =
    attrs |> List.tryPick (fun attr ->
      match attr with
      | KeyValueAttr (k, v) when k = name -> Some v
      | _ -> None)

  let private getStylePairs (attrs: XmlAttribute list) : (string * string) list =
    match getAttrValue "style" attrs with
    | Some css -> parseStylePairs css
    | None -> []

  let private hasStylePair (key: string) (value: string) (pairs: (string * string) list) =
    pairs |> List.exists (fun (k, v) -> k = key && v.Contains(value))

  let private tryParseConstraint (pairs: (string * string) list) : Constraint option =
    let tryWidth =
      pairs |> List.tryPick (fun (k, v) ->
        match k with
        | "width" when v.EndsWith("ch") ->
          match System.Int32.TryParse(v.[..v.Length-3]) with
          | true, n -> Some (Fixed n)
          | _ -> None
        | "width" when v.EndsWith("%") ->
          match System.Int32.TryParse(v.[..v.Length-2]) with
          | true, n -> Some (Percentage n)
          | _ -> None
        | "min-width" when v.EndsWith("ch") ->
          match System.Int32.TryParse(v.[..v.Length-3]) with
          | true, n -> Some (Min n)
          | _ -> None
        | "max-width" when v.EndsWith("ch") ->
          match System.Int32.TryParse(v.[..v.Length-3]) with
          | true, n -> Some (Max n)
          | _ -> None
        | "flex" when v = "1 1 auto" -> Some Fill
        | _ -> None)
    tryWidth

  let private tryParsePadding (pairs: (string * string) list) : Padding option =
    pairs |> List.tryPick (fun (k, v) ->
      match k with
      | "padding" ->
        let parts = v.Replace("ch", "").Split(' ')
        match parts with
        | [| t; r; b; l |] ->
          match System.Int32.TryParse(t), System.Int32.TryParse(r),
                System.Int32.TryParse(b), System.Int32.TryParse(l) with
          | (true, tv), (true, rv), (true, bv), (true, lv) ->
            Some { Top = tv; Right = rv; Bottom = bv; Left = lv }
          | _ -> None
        | _ -> None
      | _ -> None)

  let private tryParseBorder (pairs: (string * string) list) : BorderStyle option =
    pairs |> List.tryPick (fun (k, v) ->
      match k with
      | "border" ->
        match v with
        | "1px solid" -> Some Light
        | "2px solid" -> Some Heavy
        | "3px double" -> Some Double
        | "1px dashed" -> Some BorderStyle.Ascii
        | _ -> None
      | _ -> None)

  let rec parse (node: XmlNode) : Element =
    match node with
    | TextNode s -> El.text s
    | SelfClosingNode ((tag, attrs)) ->
      match tag with
      | "br" -> El.text "\n"
      | "hr" -> El.border (El.text (String.replicate 40 "─"))
      | _ -> Element.Empty
    | ParentNode ((tag, attrs), children) ->
      let pairs = getStylePairs attrs
      let childElements =
        children
        |> List.map parse
        |> List.filter (fun el ->
          match el with
          | Element.Empty -> false
          | Text (s, _) when s = "" -> false
          | _ -> true)

      // Detect layout from CSS
      let isFlexRow =
        hasStylePair "display" "flex" pairs &&
        hasStylePair "flex-direction" "row" pairs
      let isFlexCol =
        hasStylePair "display" "flex" pairs &&
        hasStylePair "flex-direction" "column" pairs
      let isOverlay =
        hasStylePair "position" "relative" pairs

      // Build the base element from structure
      let baseEl =
        match tag with
        | "span" ->
          let style = buildStyle pairs
          match childElements with
          | [ Text (s, _) ] -> Text (s, style)
          | [ single ] when style = Style.empty -> single
          | [ single ] -> Styled (style, single)
          | _ ->
            match style = Style.empty with
            | true -> Row childElements
            | false -> Styled (style, Row childElements)
        | "div" when isFlexRow -> Row childElements
        | "div" when isFlexCol -> Column childElements
        | "div" when isOverlay ->
          Overlay (childElements |> List.map (fun el ->
            match el with
            | Styled (_, inner) -> inner
            | other -> other))
        | _ ->
          match childElements with
          | [] -> Element.Empty
          | [ single ] -> single
          | many -> Column many

      // Apply constraint wrapper
      let withConstraint =
        match tryParseConstraint pairs with
        | Some con -> Constrained (con, baseEl)
        | None -> baseEl

      // Apply padding wrapper
      let withPadding =
        match tryParsePadding pairs with
        | Some p -> Padded (p, withConstraint)
        | None -> withConstraint

      // Apply border wrapper
      let withBorder =
        match tryParseBorder pairs with
        | Some bs -> Bordered (bs, None, withPadding)
        | None -> withPadding

      // Apply style from non-layout CSS (only for div elements that aren't flex containers)
      let withStyle =
        match tag with
        | "div" when not isFlexRow && not isFlexCol && not isOverlay ->
          let style = buildStyle pairs
          match style = Style.empty with
          | true -> withBorder
          | false -> Styled (style, withBorder)
        | _ -> withBorder

      // Apply keyed from id attribute
      match getAttrValue "id" attrs with
      | Some id -> Keyed (id, Fade 0<ms>, Fade 0<ms>, withStyle)
      | None -> withStyle
