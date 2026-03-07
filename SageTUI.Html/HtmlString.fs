namespace SageTUI.Html

open AngleSharp
open AngleSharp.Dom
open AngleSharp.Html.Parser
open SageTUI

/// Parses raw HTML strings into SageTUI Element trees for terminal rendering
module HtmlString =

  // === CSS → SageTUI type converters ===

  let private tryParseColor (s: string) : Color option =
    match s.Trim().ToLowerInvariant() with
    | "inherit" | "initial" | "unset" | "currentcolor" | "" -> None
    | "black" -> Some (Color.Named(BaseColor.Black, Intensity.Normal))
    | "red" -> Some (Color.Named(BaseColor.Red, Intensity.Normal))
    | "green" -> Some (Color.Named(BaseColor.Green, Intensity.Normal))
    | "yellow" -> Some (Color.Named(BaseColor.Yellow, Intensity.Normal))
    | "blue" -> Some (Color.Named(BaseColor.Blue, Intensity.Normal))
    | "magenta" | "fuchsia" -> Some (Color.Named(BaseColor.Magenta, Intensity.Normal))
    | "cyan" | "aqua" -> Some (Color.Named(BaseColor.Cyan, Intensity.Normal))
    | "white" -> Some (Color.Named(BaseColor.White, Intensity.Normal))
    | "gray" | "grey" -> Some (Color.Named(BaseColor.Black, Intensity.Bright))
    | "lightblack" | "darkgray" | "darkgrey" -> Some (Color.Named(BaseColor.Black, Intensity.Bright))
    | "lightred" -> Some (Color.Named(BaseColor.Red, Intensity.Bright))
    | "lightgreen" | "lime" -> Some (Color.Named(BaseColor.Green, Intensity.Bright))
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
    | s when s.StartsWith("#") && s.Length = 7 ->
      match System.Byte.TryParse(s.[1..2], System.Globalization.NumberStyles.HexNumber, null),
            System.Byte.TryParse(s.[3..4], System.Globalization.NumberStyles.HexNumber, null),
            System.Byte.TryParse(s.[5..6], System.Globalization.NumberStyles.HexNumber, null) with
      | (true, r), (true, g), (true, b) -> Some (Color.Rgb(r, g, b))
      | _ -> None
    | s when s.StartsWith("#") && s.Length = 4 ->
      match System.Byte.TryParse(sprintf "%c%c" s.[1] s.[1], System.Globalization.NumberStyles.HexNumber, null),
            System.Byte.TryParse(sprintf "%c%c" s.[2] s.[2], System.Globalization.NumberStyles.HexNumber, null),
            System.Byte.TryParse(sprintf "%c%c" s.[3] s.[3], System.Globalization.NumberStyles.HexNumber, null) with
      | (true, r), (true, g), (true, b) -> Some (Color.Rgb(r, g, b))
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
      | "background-color" | "background" ->
        match tryParseColor v with
        | Some c -> style <- { style with Bg = Some c }
        | None -> ()
      | "font-weight" when v = "bold" || v = "700" || v = "800" || v = "900" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.bold style.Attrs }
      | "font-style" when v = "italic" || v = "oblique" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.italic style.Attrs }
      | "text-decoration" when v.Contains("underline") ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.underline style.Attrs }
      | "text-decoration" when v.Contains("line-through") ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.strikethrough style.Attrs }
      | "opacity" when v = "0.5" ->
        style <- { style with Attrs = TextAttrs.combine TextAttrs.dim style.Attrs }
      | _ -> ()
    style

  let private getStyleFromElement (el: IElement) : (string * string) list =
    match el.GetAttribute("style") with
    | null | "" -> []
    | css -> parseStylePairs css

  let private hasStylePair (key: string) (value: string) (pairs: (string * string) list) =
    pairs |> List.exists (fun (k, v) -> k = key && v.Contains(value))

  let private tryParseConstraint (pairs: (string * string) list) : Constraint option =
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

  let private tryParsePadding (pairs: (string * string) list) : Padding option =
    pairs |> List.tryPick (fun (k, v) ->
      match k with
      | "padding" ->
        let parts = v.Replace("ch", "").Replace("px", "").Replace("em", "").Split(' ')
        match parts with
        | [| t; r; b; l |] ->
          match System.Int32.TryParse(t), System.Int32.TryParse(r),
                System.Int32.TryParse(b), System.Int32.TryParse(l) with
          | (true, tv), (true, rv), (true, bv), (true, lv) ->
            Some { Top = tv; Right = rv; Bottom = bv; Left = lv }
          | _ -> None
        | [| v1 |] ->
          match System.Int32.TryParse(v1) with
          | true, n -> Some { Top = n; Right = n; Bottom = n; Left = n }
          | _ -> None
        | [| tb; lr |] ->
          match System.Int32.TryParse(tb), System.Int32.TryParse(lr) with
          | (true, tbv), (true, lrv) ->
            Some { Top = tbv; Right = lrv; Bottom = tbv; Left = lrv }
          | _ -> None
        | _ -> None
      | _ -> None)

  let private tryParseBorder (pairs: (string * string) list) : BorderStyle option =
    pairs |> List.tryPick (fun (k, v) ->
      match k with
      | "border" ->
        match v with
        | s when s.Contains("double") -> Some Double
        | s when s.Contains("dashed") -> Some BorderStyle.Ascii
        | s when s.Contains("2px") -> Some Heavy
        | s when s.Contains("solid") -> Some Light
        | _ -> Some Light
      | "border-style" ->
        match v with
        | "solid" -> Some Light
        | "double" -> Some Double
        | "dashed" -> Some BorderStyle.Ascii
        | _ -> Some Light
      | _ -> None)

  // === AngleSharp DOM → Element conversion ===

  let rec private convertNode (node: INode) : Element option =
    match node.NodeType with
    | NodeType.Text ->
      let text = node.TextContent.Trim()
      match text with
      | "" -> None
      | s -> Some (El.text s)
    | NodeType.Element ->
      let el = node :?> IElement
      Some (convertElement el)
    | _ -> None

  and private convertElement (el: IElement) : Element =
    let pairs = getStyleFromElement el
    let children =
      el.ChildNodes
      |> Seq.choose convertNode
      |> Seq.toList

    let tag = el.LocalName.ToLowerInvariant()

    // Detect layout from CSS
    let isFlexRow =
      hasStylePair "display" "flex" pairs &&
      hasStylePair "flex-direction" "row" pairs
    let isFlexCol =
      hasStylePair "display" "flex" pairs &&
      hasStylePair "flex-direction" "column" pairs
    let isOverlay =
      hasStylePair "position" "relative" pairs

    // Build base element from tag semantics + CSS
    let baseEl =
      match tag with
      // Semantic block elements → Column
      | "div" when isFlexRow -> Row children
      | "div" when isFlexCol -> Column children
      | "div" when isOverlay ->
        Overlay (children |> List.map (fun el ->
          match el with
          | Styled (_, inner) -> inner
          | other -> other))
      | "div" | "section" | "article" | "main" | "aside" | "nav" | "header" | "footer" ->
        match children with
        | [] -> Element.Empty
        | [ single ] -> single
        | many -> Column many
      // Inline elements → Row or Text
      | "span" ->
        let style = buildStyle pairs
        match children with
        | [ Text (s, _) ] -> Text (s, style)
        | [ single ] when style = Style.empty -> single
        | [ single ] -> Styled (style, single)
        | [] -> Element.Empty
        | many when style = Style.empty -> Row many
        | many -> Styled (style, Row many)
      // Text formatting
      | "b" | "strong" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.bold }, inner)
      | "i" | "em" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.italic }, inner)
      | "u" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.underline }, inner)
      | "s" | "del" | "strike" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.strikethrough }, inner)
      | "code" | "pre" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Column many
        let style = { Style.empty with Fg = Some (Color.Named(BaseColor.Green, Intensity.Normal)) }
        Styled (style, inner)
      // Headings → bold text
      | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.bold }, inner)
      // Lists
      | "ul" | "ol" -> Column children
      | "li" ->
        let bullet = El.text "• "
        match children with
        | [] -> bullet
        | [ single ] -> Row [ bullet; single ]
        | many -> Row [ bullet; Column many ]
      // Links → underlined with color
      | "a" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        let style = {
          Fg = Some (Color.Named(BaseColor.Cyan, Intensity.Normal))
          Bg = None
          Attrs = TextAttrs.underline
        }
        Styled (style, inner)
      // Line break / horizontal rule
      | "br" -> El.text "\n"
      | "hr" -> El.border (El.text (String.replicate 40 "─"))
      // Table → Column of Rows
      | "table" -> Column children
      | "thead" | "tbody" | "tfoot" -> Column children
      | "tr" -> Row children
      | "th" ->
        let inner = match children with [] -> Element.Empty | [ c ] -> c | many -> Row many
        Styled ({ Style.empty with Attrs = TextAttrs.bold }, inner)
      | "td" ->
        match children with [] -> El.text " " | [ c ] -> c | many -> Row many
      // Images → alt text placeholder
      | "img" ->
        let alt = match el.GetAttribute("alt") with null -> "[img]" | s -> sprintf "[%s]" s
        El.text alt
      // Input elements → placeholder representation
      | "input" ->
        let typ = match el.GetAttribute("type") with null -> "text" | t -> t
        let placeholder = match el.GetAttribute("placeholder") with null -> "" | p -> p
        El.bordered Light (El.text (sprintf "[%s: %s]" typ placeholder))
      | "button" ->
        let inner = match children with [] -> El.text "button" | [ c ] -> c | many -> Row many
        El.bordered Light inner
      | "textarea" ->
        El.bordered Light (
          match children with
          | [ c ] -> c
          | _ -> El.text "[textarea]")
      // Fallback for unknown tags
      | _ ->
        match children with
        | [] -> Element.Empty
        | [ single ] -> single
        | many -> Column many

    // Apply CSS wrappers (constraint → padding → border → style → keyed)
    let withConstraint =
      match tryParseConstraint pairs with
      | Some con -> Constrained (con, baseEl)
      | None -> baseEl

    let withPadding =
      match tryParsePadding pairs with
      | Some p -> Padded (p, withConstraint)
      | None -> withConstraint

    let withBorder =
      match tryParseBorder pairs with
      | Some bs -> Bordered (bs, withPadding)
      | None ->
        match tag with
        | "div" when isFlexRow || isFlexCol || isOverlay -> withPadding
        | _ ->
          let hasBorderRadius =
            pairs |> List.exists (fun (k, _) -> k = "border-radius")
          match hasBorderRadius with
          | true -> Bordered (Rounded, withPadding)
          | false -> withPadding

    // Apply style (only for non-layout containers to avoid double-wrapping)
    let withStyle =
      match tag with
      | "span" | "b" | "strong" | "i" | "em" | "u" | "s" | "del" | "strike"
      | "a" | "code" | "pre" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "th" ->
        withBorder
      | _ ->
        let style = buildStyle pairs
        match style = Style.empty with
        | true -> withBorder
        | false -> Styled (style, withBorder)

    // Apply keyed from id
    match el.GetAttribute("id") with
    | null | "" -> withStyle
    | id -> Keyed (id, Fade 0<ms>, Fade 0<ms>, withStyle)

  // === Public API ===

  let private parser = HtmlParser()

  /// Parse an HTML string into a SageTUI Element tree
  let parse (html: string) : Element =
    let doc = parser.ParseDocument(html)
    let body = doc.Body
    match body with
    | null -> Element.Empty
    | body ->
      let children =
        body.ChildNodes
        |> Seq.choose convertNode
        |> Seq.toList
      match children with
      | [] -> Element.Empty
      | [ single ] -> single
      | many -> Column many

  /// Parse an HTML fragment (no <html>/<body> wrapper needed)
  let parseFragment (html: string) : Element =
    let doc = parser.ParseDocument(sprintf "<body>%s</body>" html)
    let body = doc.Body
    match body with
    | null -> Element.Empty
    | body ->
      let children =
        body.ChildNodes
        |> Seq.choose convertNode
        |> Seq.toList
      match children with
      | [] -> Element.Empty
      | [ single ] -> single
      | many -> Column many
