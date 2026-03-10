module SageTUI.HtmlTests

open Expecto
open Expecto.Flip
open Falco.Markup
open SageTUI
open SageTUI.Html

// === HtmlRender tests: Element → XmlNode → HTML string ===

let private toHtml el = HtmlRender.renderToString el

let htmlRenderEmptyTests = testList "HtmlRender.Empty" [
  testCase "Empty → empty string" <| fun () ->
    toHtml Element.Empty
    |> Expect.equal "empty renders to empty" ""
]

let htmlRenderTextTests = testList "HtmlRender.Text" [
  testCase "plain text → <span>" <| fun () ->
    toHtml (El.text "hello")
    |> Expect.equal "span" "<span>hello</span>"

  testCase "text with fg color → span with CSS color" <| fun () ->
    let html = toHtml (El.fg (Color.Named(BaseColor.Red, Intensity.Normal)) (El.text "hi"))
    html |> Expect.stringContains "has color" "color:red"

  testCase "text with bg color → span with background-color" <| fun () ->
    let html = toHtml (El.bg (Color.Named(BaseColor.Blue, Intensity.Bright)) (El.text "bg"))
    html |> Expect.stringContains "has bg" "background-color:lightblue"

  testCase "text with RGB color" <| fun () ->
    let html = toHtml (El.fg (Color.Rgb(255uy, 0uy, 128uy)) (El.text "rgb"))
    html |> Expect.stringContains "has rgb" "rgb(255,0,128)"

  testCase "bold text" <| fun () ->
    let style = { Style.empty with Attrs = TextAttrs.bold }
    let html = toHtml (Text("bold", style))
    html |> Expect.stringContains "has bold" "font-weight:bold"

  testCase "italic text" <| fun () ->
    let style = { Style.empty with Attrs = TextAttrs.italic }
    let html = toHtml (Text("italic", style))
    html |> Expect.stringContains "has italic" "font-style:italic"

  testCase "underline text" <| fun () ->
    let style = { Style.empty with Attrs = TextAttrs.underline }
    let html = toHtml (Text("underline", style))
    html |> Expect.stringContains "has underline" "text-decoration:underline"
]

let htmlRenderLayoutTests = testList "HtmlRender.Layout" [
  testCase "Row → flex-direction:row" <| fun () ->
    let html = toHtml (El.row [ El.text "a"; El.text "b" ])
    html |> Expect.stringContains "flex row" "flex-direction:row"

  testCase "Column → flex-direction:column" <| fun () ->
    let html = toHtml (El.column [ El.text "a"; El.text "b" ])
    html |> Expect.stringContains "flex col" "flex-direction:column"

  testCase "Row contains children" <| fun () ->
    let html = toHtml (El.row [ El.text "a"; El.text "b" ])
    html |> Expect.stringContains "has a" "<span>a</span>"
    html |> Expect.stringContains "has b" "<span>b</span>"

  testCase "Overlay → position:relative" <| fun () ->
    let html = toHtml (El.overlay [ El.text "base"; El.text "top" ])
    html |> Expect.stringContains "relative" "position:relative"
    html |> Expect.stringContains "absolute" "position:absolute"
]

let htmlRenderWrappersTests = testList "HtmlRender.Wrappers" [
  testCase "Bordered Light → border:1px solid" <| fun () ->
    let html = toHtml (El.bordered Light (El.text "box"))
    html |> Expect.stringContains "border" "border:1px solid"

  testCase "Bordered Heavy → border:2px solid" <| fun () ->
    let html = toHtml (El.bordered Heavy (El.text "box"))
    html |> Expect.stringContains "heavy border" "border:2px solid"

  testCase "Bordered Rounded → border-radius" <| fun () ->
    let html = toHtml (El.bordered Rounded (El.text "box"))
    html |> Expect.stringContains "rounded" "border-radius:4px"

  testCase "Padded → CSS padding" <| fun () ->
    let html = toHtml (El.padAll 2 (El.text "padded"))
    html |> Expect.stringContains "padding" "padding:2ch 2ch 2ch 2ch"

  testCase "Keyed → id attribute" <| fun () ->
    let el = Keyed("main", Fade 0<ms>, Fade 0<ms>, El.text "content")
    let html = toHtml el
    html |> Expect.stringContains "has id" "id=\"main\""

  testCase "Canvas → placeholder" <| fun () ->
    let buf : PixelBuffer = { Width = 0; Height = 0; Pixels = [||] }
    let el = Canvas { Mode = CanvasMode.HalfBlock; Draw = (fun _ _ -> buf); Fallback = None }
    let html = toHtml el
    html |> Expect.stringContains "canvas placeholder" "[canvas]"
]

let htmlRenderConstraintTests = testList "HtmlRender.Constraints" [
  testCase "Fixed → width in ch" <| fun () ->
    let html = toHtml (Constrained(Fixed 30, El.text "fixed"))
    html |> Expect.stringContains "fixed width" "width:30ch"

  testCase "Min → min-width" <| fun () ->
    let html = toHtml (Constrained(Min 10, El.text "min"))
    html |> Expect.stringContains "min-width" "min-width:10ch"

  testCase "Max → max-width" <| fun () ->
    let html = toHtml (Constrained(Max 50, El.text "max"))
    html |> Expect.stringContains "max-width" "max-width:50ch"

  testCase "Percentage → width percent" <| fun () ->
    let html = toHtml (Constrained(Percentage 75, El.text "pct"))
    html |> Expect.stringContains "pct width" "width:75%"

  testCase "Fill → flex auto" <| fun () ->
    let html = toHtml (Constrained(Fill, El.text "fill"))
    html |> Expect.stringContains "flex fill" "flex:1 1 auto"

  testCase "Ratio → flex-grow" <| fun () ->
    let html = toHtml (Constrained(Ratio(1, 3), El.text "ratio"))
    html |> Expect.stringContains "flex ratio" "flex:1 3 auto"
]

let htmlRenderNestedTests = testList "HtmlRender.Nested" [
  testCase "nested Row > Column > Bordered > styled text" <| fun () ->
    let el =
      El.row [
        El.column [
          El.bordered Light (
            El.fg (Color.Rgb(255uy, 0uy, 128uy)) (El.text "deep"))
        ]
      ]
    let html = toHtml el
    html |> Expect.stringContains "has row" "flex-direction:row"
    html |> Expect.stringContains "has col" "flex-direction:column"
    html |> Expect.stringContains "has border" "border:"
    html |> Expect.stringContains "has rgb" "rgb(255,0,128)"
]

// === HtmlParse tests: XmlNode → Element ===

let htmlParseTextTests = testList "HtmlParse.Text" [
  testCase "TextNode → Text element" <| fun () ->
    let el = HtmlParse.parse (TextNode "hello")
    match el with
    | Text (s, _) -> s |> Expect.equal "text content" "hello"
    | other -> failwithf "expected Text, got %A" other

  testCase "span with text → Text element" <| fun () ->
    let node = Elem.span [] [ Text.raw "world" ]
    let el = HtmlParse.parse node
    match el with
    | Text (s, _) -> s |> Expect.equal "span text" "world"
    | other -> failwithf "expected Text, got %A" other

  testCase "span with color → styled Text" <| fun () ->
    let node = Elem.span [ Attr.style "color:red" ] [ Text.raw "red" ]
    let el = HtmlParse.parse node
    match el with
    | Text (s, style) ->
      s |> Expect.equal "text" "red"
      style.Fg |> Expect.isSome "has fg"
    | other -> failwithf "expected Text, got %A" other
]

let htmlParseLayoutTests = testList "HtmlParse.Layout" [
  testCase "flex-direction:row → Row" <| fun () ->
    let node = Elem.div
                [ Attr.style "display:flex;flex-direction:row" ]
                [ Elem.span [] [ Text.raw "a" ]; Elem.span [] [ Text.raw "b" ] ]
    let el = HtmlParse.parse node
    match el with
    | Row children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Row, got %A" other

  testCase "flex-direction:column → Column" <| fun () ->
    let node = Elem.div
                [ Attr.style "display:flex;flex-direction:column" ]
                [ Elem.span [] [ Text.raw "a" ]; Elem.span [] [ Text.raw "b" ] ]
    let el = HtmlParse.parse node
    match el with
    | Column children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Column, got %A" other
]

let htmlParseWrapperTests = testList "HtmlParse.Wrappers" [
  testCase "border:1px solid → Bordered Light" <| fun () ->
    let node = Elem.div
                [ Attr.style "border:1px solid" ]
                [ Elem.span [] [ Text.raw "box" ] ]
    let el = HtmlParse.parse node
    match el with
    | Bordered (Light, _, _) -> ()
    | other -> failwithf "expected Bordered Light, got %A" other

  testCase "padding → Padded" <| fun () ->
    let node = Elem.div
                [ Attr.style "padding:2ch 2ch 2ch 2ch" ]
                [ Elem.span [] [ Text.raw "pad" ] ]
    let el = HtmlParse.parse node
    match el with
    | Padded ({ Top = 2; Right = 2; Bottom = 2; Left = 2 }, _) -> ()
    | other -> failwithf "expected Padded 2, got %A" other

  testCase "width:30ch → Constrained Fixed" <| fun () ->
    let node = Elem.div
                [ Attr.style "width:30ch;flex:0 0 30ch" ]
                [ Elem.span [] [ Text.raw "w" ] ]
    let el = HtmlParse.parse node
    match el with
    | Constrained (Fixed 30, _) -> ()
    | other -> failwithf "expected Constrained Fixed 30, got %A" other

  testCase "id attribute → Keyed" <| fun () ->
    let node = Elem.div
                [ Attr.id "main" ]
                [ Elem.span [] [ Text.raw "keyed" ] ]
    let el = HtmlParse.parse node
    match el with
    | Keyed ("main", _, _, _) -> ()
    | other -> failwithf "expected Keyed main, got %A" other
]

// === Roundtrip tests: Element → XmlNode → Element (structural equivalence) ===

let htmlRoundtripTests = testList "Html.Roundtrip" [
  testCase "plain text roundtrips" <| fun () ->
    let original = El.text "hello"
    let roundtripped = original |> HtmlRender.render |> HtmlParse.parse
    match roundtripped with
    | Text ("hello", _) -> ()
    | other -> failwithf "expected Text hello, got %A" other

  testCase "Row roundtrips" <| fun () ->
    let original = El.row [ El.text "a"; El.text "b" ]
    let roundtripped = original |> HtmlRender.render |> HtmlParse.parse
    match roundtripped with
    | Row children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Row, got %A" other

  testCase "Column roundtrips" <| fun () ->
    let original = El.column [ El.text "x"; El.text "y" ]
    let roundtripped = original |> HtmlRender.render |> HtmlParse.parse
    match roundtripped with
    | Column children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Column, got %A" other

  testCase "Bordered Light roundtrips" <| fun () ->
    let original = El.bordered Light (El.text "box")
    let roundtripped = original |> HtmlRender.render |> HtmlParse.parse
    match roundtripped with
    | Bordered (Light, _, _) -> ()
    | other -> failwithf "expected Bordered Light, got %A" other
]

// === HtmlString tests: raw HTML string → Element ===

let htmlStringBasicTests = testList "HtmlString.Basic" [
  testCase "plain text" <| fun () ->
    let el = HtmlString.parseFragment "hello world"
    match el with
    | Text ("hello world", _) -> ()
    | other -> failwithf "expected Text, got %A" other

  testCase "span with text" <| fun () ->
    let el = HtmlString.parseFragment "<span>hello</span>"
    match el with
    | Text ("hello", _) -> ()
    | other -> failwithf "expected Text hello, got %A" other

  testCase "div with text" <| fun () ->
    let el = HtmlString.parseFragment "<div>content</div>"
    match el with
    | Text ("content", _) -> ()
    | other -> failwithf "expected Text content, got %A" other

  testCase "empty string" <| fun () ->
    let el = HtmlString.parseFragment ""
    match el with
    | Element.Empty -> ()
    | other -> failwithf "expected Empty, got %A" other
]

let htmlStringSemanticTests = testList "HtmlString.Semantic" [
  testCase "bold tag" <| fun () ->
    let el = HtmlString.parseFragment "<b>bold</b>"
    match el with
    | Styled (s, Text ("bold", _)) ->
      TextAttrs.has TextAttrs.bold s.Attrs
      |> Expect.isTrue "has bold"
    | other -> failwithf "expected Styled bold, got %A" other

  testCase "strong tag" <| fun () ->
    let el = HtmlString.parseFragment "<strong>strong</strong>"
    match el with
    | Styled (s, Text ("strong", _)) ->
      TextAttrs.has TextAttrs.bold s.Attrs
      |> Expect.isTrue "has bold"
    | other -> failwithf "expected Styled bold, got %A" other

  testCase "italic tag" <| fun () ->
    let el = HtmlString.parseFragment "<em>italic</em>"
    match el with
    | Styled (s, Text ("italic", _)) ->
      TextAttrs.has TextAttrs.italic s.Attrs
      |> Expect.isTrue "has italic"
    | other -> failwithf "expected Styled italic, got %A" other

  testCase "underline tag" <| fun () ->
    let el = HtmlString.parseFragment "<u>underline</u>"
    match el with
    | Styled (s, Text ("underline", _)) ->
      TextAttrs.has TextAttrs.underline s.Attrs
      |> Expect.isTrue "has underline"
    | other -> failwithf "expected Styled underline, got %A" other

  testCase "link tag → cyan underline" <| fun () ->
    let el = HtmlString.parseFragment """<a href="#">link</a>"""
    match el with
    | Styled (s, Text ("link", _)) ->
      s.Fg |> Expect.isSome "has fg color"
      TextAttrs.has TextAttrs.underline s.Attrs
      |> Expect.isTrue "has underline"
    | other -> failwithf "expected Styled link, got %A" other

  testCase "heading → bold" <| fun () ->
    let el = HtmlString.parseFragment "<h1>Title</h1>"
    match el with
    | Styled (s, Text ("Title", _)) ->
      TextAttrs.has TextAttrs.bold s.Attrs
      |> Expect.isTrue "has bold"
    | other -> failwithf "expected Styled bold heading, got %A" other

  testCase "list items → bullets" <| fun () ->
    let el = HtmlString.parseFragment "<ul><li>one</li><li>two</li></ul>"
    match el with
    | Column items ->
      items |> List.length |> Expect.equal "2 items" 2
    | other -> failwithf "expected Column of items, got %A" other
]

let htmlStringCssTests = testList "HtmlString.CSS" [
  testCase "inline color" <| fun () ->
    let el = HtmlString.parseFragment """<span style="color:red">red text</span>"""
    match el with
    | Text (s, style) ->
      s |> Expect.equal "text" "red text"
      style.Fg |> Expect.isSome "has fg"
    | other -> failwithf "expected styled Text, got %A" other

  testCase "hex color" <| fun () ->
    let el = HtmlString.parseFragment """<span style="color:#ff0080">hex</span>"""
    match el with
    | Text (_, style) ->
      match style.Fg with
      | Some (Color.Rgb (255uy, 0uy, 128uy)) -> ()
      | other -> failwithf "expected Rgb(255,0,128), got %A" other
    | other -> failwithf "expected Text, got %A" other

  testCase "short hex color" <| fun () ->
    let el = HtmlString.parseFragment """<span style="color:#f00">short hex</span>"""
    match el with
    | Text (_, style) ->
      match style.Fg with
      | Some (Color.Rgb (255uy, 0uy, 0uy)) -> ()
      | other -> failwithf "expected Rgb(255,0,0), got %A" other
    | other -> failwithf "expected Text, got %A" other

  testCase "flex row" <| fun () ->
    let el = HtmlString.parseFragment """<div style="display:flex;flex-direction:row"><span>a</span><span>b</span></div>"""
    match el with
    | Row children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Row, got %A" other

  testCase "flex column" <| fun () ->
    let el = HtmlString.parseFragment """<div style="display:flex;flex-direction:column"><span>a</span><span>b</span></div>"""
    match el with
    | Column children ->
      children |> List.length |> Expect.equal "2 children" 2
    | other -> failwithf "expected Column, got %A" other

  testCase "border" <| fun () ->
    let el = HtmlString.parseFragment """<div style="border:1px solid"><span>box</span></div>"""
    match el with
    | Bordered (Light, _, _) -> ()
    | other -> failwithf "expected Bordered Light, got %A" other

  testCase "padding" <| fun () ->
    let el = HtmlString.parseFragment """<div style="padding:2ch 2ch 2ch 2ch"><span>pad</span></div>"""
    match el with
    | Padded ({ Top = 2; Right = 2; Bottom = 2; Left = 2 }, _) -> ()
    | other -> failwithf "expected Padded 2, got %A" other
]

let htmlStringComplexTests = testList "HtmlString.Complex" [
  testCase "nested structure" <| fun () ->
    let html = """
      <div style="display:flex;flex-direction:row">
        <div style="border:1px solid">
          <span style="color:red">hello</span>
        </div>
        <span>world</span>
      </div>"""
    let el = HtmlString.parseFragment html
    match el with
    | Row [ Bordered(Light, _, _); Text("world", _) ] -> ()
    | Row children ->
      children |> List.length |> Expect.equal "2 children in row" 2
    | other -> failwithf "expected Row with bordered + text, got %A" other

  testCase "full HTML document" <| fun () ->
    let html = """
      <html><body>
        <h1>Dashboard</h1>
        <div style="display:flex;flex-direction:row">
          <span>Left</span>
          <span>Right</span>
        </div>
      </body></html>"""
    let el = HtmlString.parse html
    match el with
    | Column _ -> ()
    | other -> failwithf "expected Column, got %A" other

  testCase "table renders as rows" <| fun () ->
    let html = """<table><tr><td>A</td><td>B</td></tr><tr><td>C</td><td>D</td></tr></table>"""
    let el = HtmlString.parseFragment html
    // AngleSharp inserts tbody, so table→Column[tbody→Column[rows]]
    // We just verify the structure flattens to contain Row elements with cells
    let rec containsRow = function
      | Row _ -> true
      | Column children -> children |> List.exists containsRow
      | Styled (_, inner) -> containsRow inner
      | _ -> false
    containsRow el |> Expect.isTrue "should contain rows"

  testCase "id attribute → Keyed" <| fun () ->
    let el = HtmlString.parseFragment """<div id="main"><span>content</span></div>"""
    match el with
    | Keyed ("main", _, _, _) -> ()
    | other -> failwithf "expected Keyed main, got %A" other

  testCase "img → alt text" <| fun () ->
    let el = HtmlString.parseFragment """<img alt="Logo" />"""
    match el with
    | Text (s, _) when s.Contains("Logo") -> ()
    | other -> failwithf "expected text with Logo, got %A" other
]

[<Tests>]
let allHtmlTests = testList "Html" [
  htmlRenderEmptyTests
  htmlRenderTextTests
  htmlRenderLayoutTests
  htmlRenderWrappersTests
  htmlRenderConstraintTests
  htmlRenderNestedTests
  htmlParseTextTests
  htmlParseLayoutTests
  htmlParseWrapperTests
  htmlRoundtripTests
  htmlStringBasicTests
  htmlStringSemanticTests
  htmlStringCssTests
  htmlStringComplexTests
]
