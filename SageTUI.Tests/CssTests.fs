module SageTUI.CssTests

open Expecto
open Expecto.Flip
open Expecto.ExpectoFsCheck
open FsCheck
open SageTUI
open SageTUI.Html

// ================================================================
// Element structure helpers (Element has NoEquality)
// ================================================================
let rec countLeaves (el: Element) =
  match el with
  | Element.Empty -> 0
  | Element.Text _ -> 1
  | Element.Row cs | Element.Column cs | Element.Overlay cs ->
    cs |> List.sumBy countLeaves
  | Element.Styled (_, c) | Element.Constrained (_, c)
  | Element.Bordered (_, _, c) | Element.Padded (_, c)
  | Element.Keyed (_, _, _, c)
  | Element.Aligned (_, _, c) | Element.Gapped (_, c) -> countLeaves c
  | Element.Canvas _ -> 1
  | Element.Responsive bps ->
    bps |> List.tryHead |> Option.map (snd >> countLeaves) |> Option.defaultValue 0
  | Element.ResponsiveH bps ->
    bps |> List.tryHead |> Option.map (snd >> countLeaves) |> Option.defaultValue 0

let isRow el = match el with Element.Row _ -> true | _ -> false
let isCol el = match el with Element.Column _ -> true | _ -> false
let isEmpty el = match el with Element.Empty -> true | _ -> false

// ================================================================
// MDN EXAMPLE-BASED TESTS
// ================================================================

[<Tests>]
let mdnDisplayTests = testList "MDN Display Defaults" [
  testList "Block elements" [
    for tag in
      [ "div"; "p"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"
        "section"; "article"; "main"; "aside"; "nav"
        "header"; "footer"; "ul"; "ol"; "pre"
        "blockquote"; "form"; "fieldset" ] do
      test (sprintf "%s" tag) {
        Defaults.display tag |> Expect.equal "" CssBlock
      }
  ]
  testList "Inline elements" [
    for tag in
      [ "span"; "a"; "strong"; "b"; "em"; "i"; "u"; "s"; "del"
        "code"; "small"; "sub"; "sup"; "abbr"; "cite"; "q"; "mark"
        "br"; "img"; "input"; "label" ] do
      test (sprintf "%s" tag) {
        Defaults.display tag |> Expect.equal "" CssInline
      }
  ]
  test "li" { Defaults.display "li" |> Expect.equal "" CssListItem }
  test "table" { Defaults.display "table" |> Expect.equal "" CssTable }
  test "tr" { Defaults.display "tr" |> Expect.equal "" CssTableRow }
  test "td" { Defaults.display "td" |> Expect.equal "" CssTableCell }
  test "th" { Defaults.display "th" |> Expect.equal "" CssTableCell }
]

[<Tests>]
let mdnSpecTests = testList "MDN Specificity Examples" [
  test "ID=1-0-0" {
    Spec.compute (IdSel "x") |> Expect.equal "" { Ids = 1; Cls = 0; Tags = 0 }
  }
  test "Class=0-1-0" {
    Spec.compute (ClassSel "x") |> Expect.equal "" { Ids = 0; Cls = 1; Tags = 0 }
  }
  test "Type=0-0-1" {
    Spec.compute (TagSel "d") |> Expect.equal "" { Ids = 0; Cls = 0; Tags = 1 }
  }
  test "*=0-0-0" {
    Spec.compute StarSel |> Expect.equal "" { Ids = 0; Cls = 0; Tags = 0 }
  }
  test "div.foo=0-1-1" {
    Spec.compute (CompSel [TagSel "d"; ClassSel "f"])
    |> Expect.equal "" { Ids = 0; Cls = 1; Tags = 1 }
  }
  test "#id .c div=1-1-1" {
    Spec.compute (DescSel (DescSel (IdSel "i", ClassSel "c"), TagSel "d"))
    |> Expect.equal "" { Ids = 1; Cls = 1; Tags = 1 }
  }
  test "ID>classes" {
    (Spec.cmp { Ids = 1; Cls = 0; Tags = 0 } { Ids = 0; Cls = 4; Tags = 0 } > 0)
    |> Expect.isTrue ""
  }
  test "Class>types" {
    (Spec.cmp { Ids = 0; Cls = 1; Tags = 0 } { Ids = 0; Cls = 0; Tags = 4 } > 0)
    |> Expect.isTrue ""
  }
  test "Equal=0" {
    Spec.cmp { Ids = 0; Cls = 1; Tags = 1 } { Ids = 0; Cls = 1; Tags = 1 }
    |> Expect.equal "" 0
  }
]

[<Tests>]
let mdnInheritTests = testList "MDN Inheritance Examples" [
  testList "Inherits" [
    for p in
      [ "color"; "font-weight"; "font-style"; "font-family"
        "font-size"; "line-height"; "text-align"; "visibility"
        "cursor"; "list-style" ] do
      test p { Defaults.inherits p |> Expect.isTrue "" }
  ]
  testList "Does not inherit" [
    for p in
      [ "background-color"; "border"; "padding"; "margin"; "display"
        "width"; "height"; "text-decoration"; "min-width"; "max-width" ] do
      test p { Defaults.inherits p |> Expect.isFalse "" }
  ]
]

[<Tests>]
let mdnFlowTests = testList "MDN Flow Layout Examples" [
  test "2 blocks → Column" {
    let r = CssLayout.flowLayout [(CssBlock, El.text "a"); (CssBlock, El.text "b")]
    r |> isCol |> Expect.isTrue "blocks stack vertically"
  }
  test "2 inlines → Row" {
    let r = CssLayout.flowLayout [(CssInline, El.text "a"); (CssInline, El.text "b")]
    r |> isRow |> Expect.isTrue "inlines flow horizontally"
  }
  test "mixed → Column with anonymous Row" {
    let r = CssLayout.flowLayout [(CssInline, El.text "i"); (CssBlock, El.text "b")]
    r |> isCol |> Expect.isTrue "mixed produces Column"
    match r with
    | Element.Column [Element.Row _; _] -> ()
    | other -> failwithf "expected Column[Row;block] got %A" other
  }
  test "consecutive inlines wrapped together" {
    let r =
      CssLayout.flowLayout [
        (CssBlock, El.text "b")
        (CssInline, El.text "a")
        (CssInline, El.text "b")
        (CssBlock, El.text "c")
      ]
    match r with
    | Element.Column [_; Element.Row items; _] ->
      items.Length |> Expect.equal "2 inlines in row" 2
    | other -> failwithf "expected Column[block;Row[2];block] got %A" other
  }
  test "empty → Empty" {
    CssLayout.flowLayout [] |> isEmpty |> Expect.isTrue ""
  }
  test "single → unwrapped" {
    match CssLayout.flowLayout [(CssBlock, El.text "only")] with
    | Element.Text ("only", _) -> ()
    | other -> failwithf "expected Text got %A" other
  }
]

// ================================================================
// PROPERTY-BASED TESTS (FsCheck)
// ================================================================

let fsCheckCfg = { FsCheckConfig.defaultConfig with maxTest = 200 }

[<Tests>]
let specPropertyTests = testList "Specificity Properties" [
  testPropertyWithConfig fsCheckCfg "reflexive" <|
    fun (i: byte) (c: byte) (t: byte) ->
      let s = { Ids = int i % 6; Cls = int c % 11; Tags = int t % 11 }
      Spec.cmp s s |> Expect.equal "a=a" 0

  testPropertyWithConfig fsCheckCfg "antisymmetric" <|
    fun (i1: byte) (c1: byte) (t1: byte) (i2: byte) (c2: byte) (t2: byte) ->
      let a = { Ids = int i1 % 6; Cls = int c1 % 11; Tags = int t1 % 11 }
      let b = { Ids = int i2 % 6; Cls = int c2 % 11; Tags = int t2 % 11 }
      (Spec.cmp a b + Spec.cmp b a) |> Expect.equal "anti" 0

  testPropertyWithConfig fsCheckCfg "transitive" <|
    fun (i1: byte) (c1: byte) (t1: byte)
        (i2: byte) (c2: byte) (t2: byte)
        (i3: byte) (c3: byte) (t3: byte) ->
      let a = { Ids = int i1 % 4; Cls = int c1 % 6; Tags = int t1 % 6 }
      let b = { Ids = int i2 % 4; Cls = int c2 % 6; Tags = int t2 % 6 }
      let c = { Ids = int i3 % 4; Cls = int c3 % 6; Tags = int t3 % 6 }
      match Spec.cmp a b > 0 && Spec.cmp b c > 0 with
      | true -> (Spec.cmp a c > 0) |> Expect.isTrue "a>b>c => a>c"
      | false -> ()

  testPropertyWithConfig fsCheckCfg "ID dominates" <|
    fun (c: byte) (t: byte) ->
      let hi = { Ids = 1; Cls = 0; Tags = 0 }
      let lo = { Ids = 0; Cls = int c % 100 + 1; Tags = int t % 100 }
      (Spec.cmp hi lo > 0) |> Expect.isTrue "ID>classes"

  testPropertyWithConfig fsCheckCfg "class dominates types" <|
    fun (t: byte) ->
      let hi = { Ids = 0; Cls = 1; Tags = 0 }
      let lo = { Ids = 0; Cls = 0; Tags = int t % 100 + 1 }
      (Spec.cmp hi lo > 0) |> Expect.isTrue "class>types"

  testPropertyWithConfig fsCheckCfg "universal=zero" <|
    fun () ->
      Spec.compute StarSel |> Expect.equal "" Spec.zero

  testPropertyWithConfig fsCheckCfg "compound=sum of parts" <|
    fun () ->
      let parts = [TagSel "d"; ClassSel "f"; IdSel "b"]
      let compound = Spec.compute (CompSel parts)
      let manual =
        parts
        |> List.map Spec.compute
        |> List.fold
          (fun a s ->
            { Ids = a.Ids + s.Ids; Cls = a.Cls + s.Cls; Tags = a.Tags + s.Tags })
          Spec.zero
      compound |> Expect.equal "" manual
]

[<Tests>]
let flowPropertyTests = testList "Flow Layout Properties" [
  testPropertyWithConfig fsCheckCfg "pure blocks → Column" <|
    fun (n: byte) ->
      let count = int n % 5 + 2
      let cs = List.init count (fun i -> (CssBlock, El.text (sprintf "b%d" i)))
      CssLayout.flowLayout cs |> isCol |> Expect.isTrue "blocks→col"

  testPropertyWithConfig fsCheckCfg "pure inlines → Row" <|
    fun (n: byte) ->
      let count = int n % 5 + 2
      let cs = List.init count (fun i -> (CssInline, El.text (sprintf "i%d" i)))
      CssLayout.flowLayout cs |> isRow |> Expect.isTrue "inlines→row"

  testPropertyWithConfig fsCheckCfg "empty → Empty" <|
    fun () ->
      CssLayout.flowLayout [] |> isEmpty |> Expect.isTrue "empty"

  testPropertyWithConfig fsCheckCfg "single unwrapped" <|
    fun (s: string) ->
      match s with
      | null | "" -> ()
      | _ ->
        match CssLayout.flowLayout [(CssBlock, El.text s)] with
        | Element.Text (t, _) -> t |> Expect.equal "" s
        | _ -> failwith "single not unwrapped"

  testPropertyWithConfig fsCheckCfg "preserves leaf count" <|
    fun (nb: byte) (ni: byte) ->
      let nbl = int nb % 4 + 1
      let nil = int ni % 4 + 1
      let bs = List.init nbl (fun i -> (CssBlock, El.text (sprintf "b%d" i)))
      let ins = List.init nil (fun i -> (CssInline, El.text (sprintf "i%d" i)))
      let all = bs @ ins
      let inp = all |> List.sumBy (fun (_, el) -> countLeaves el)
      let outp = countLeaves (CssLayout.flowLayout all)
      outp |> Expect.equal "leaf count" inp

  testPropertyWithConfig fsCheckCfg "mixed → Column top" <|
    fun (nb: byte) (ni: byte) ->
      let nbl = int nb % 3 + 1
      let nil = int ni % 3 + 1
      let bs = List.init nbl (fun i -> (CssBlock, El.text (sprintf "b%d" i)))
      let ins = List.init nil (fun i -> (CssInline, El.text (sprintf "i%d" i)))
      let mixed = bs @ ins
      match mixed.Length with
      | 1 -> ()
      | _ ->
        CssLayout.flowLayout mixed |> isCol |> Expect.isTrue "mixed→col"

  testPropertyWithConfig fsCheckCfg "none is not block-level" <|
    fun () ->
      CssLayout.isBlockLevel CssNone |> Expect.isFalse "none"

  testPropertyWithConfig fsCheckCfg "flex is block-level" <|
    fun () ->
      CssLayout.isBlockLevel (CssFlex FdRow) |> Expect.isTrue "flex=block"
      CssLayout.isBlockLevel (CssFlex FdColumn) |> Expect.isTrue "flex=block"
]

[<Tests>]
let inheritPropertyTests = testList "Inheritance Properties" [
  testPropertyWithConfig fsCheckCfg "known inherited" <|
    fun () ->
      [ "color"; "font-weight"; "font-style"; "font-family"
        "font-size"; "line-height"; "text-align"; "visibility"
        "cursor"; "list-style"; "list-style-type" ]
      |> List.iter (fun p ->
        Defaults.inherits p |> Expect.isTrue p)

  testPropertyWithConfig fsCheckCfg "layout never inherits" <|
    fun () ->
      [ "background-color"; "border"; "padding"; "margin"; "display"
        "width"; "height"; "text-decoration"; "min-width"; "max-width"
        "overflow"; "position"; "flex-direction"; "flex-grow"
        "flex-shrink"; "gap"; "z-index" ]
      |> List.iter (fun p ->
        Defaults.inherits p |> Expect.isFalse p)

  testPropertyWithConfig fsCheckCfg "arbitrary string default=no" <|
    fun (s: string) ->
      match s with
      | null | "" -> ()
      | s when s.Contains("color") || s.Contains("font-")
            || s.Contains("text-align") || s.Contains("line-height")
            || s.Contains("visibility") || s.Contains("cursor")
            || s.Contains("list-style") -> ()
      | _ -> Defaults.inherits s |> Expect.isFalse ""
]

[<Tests>]
let displayPropertyTests = testList "Display Mapping Properties" [
  testPropertyWithConfig fsCheckCfg "all tags have display" <|
    fun () ->
      [ "div"; "p"; "span"; "a"; "strong"; "b"; "em"; "i"; "u"; "s"
        "del"; "code"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "pre"
        "blockquote"; "section"; "article"; "main"; "aside"; "nav"
        "header"; "footer"; "ul"; "ol"; "li"; "table"; "tr"; "td"
        "th"; "form"; "input"; "label"; "button"; "textarea"; "select"
        "small"; "sub"; "sup"; "abbr"; "br"; "img"; "hr"; "figure"
        "figcaption" ]
      |> List.iter (fun t ->
        match Defaults.display t with
        | CssNone -> failwith t
        | _ -> ())

  testPropertyWithConfig fsCheckCfg "unknown → block" <|
    fun (s: string) ->
      match s with
      | null | "" -> ()
      | s when s.Length > 20 -> ()
      | _ ->
        let known =
          set [ "div"; "p"; "span"; "a"; "strong"; "b"; "em"; "i"; "u"
                "s"; "del"; "strike"; "code"; "small"; "sub"; "sup"
                "abbr"; "cite"; "q"; "mark"; "time"; "data"; "var"
                "samp"; "kbd"; "dfn"; "bdo"; "bdi"; "label"; "output"
                "br"; "img"; "input"; "h1"; "h2"; "h3"; "h4"; "h5"
                "h6"; "pre"; "blockquote"; "section"; "article"; "main"
                "aside"; "nav"; "header"; "footer"; "ul"; "ol"; "li"
                "table"; "tr"; "td"; "th"; "thead"; "tbody"; "tfoot"
                "form"; "fieldset"; "figure"; "figcaption"; "details"
                "summary"; "address"; "dl"; "dd"; "dt"; "hr"; "button"
                "textarea"; "select" ]
        match known.Contains s with
        | true -> ()
        | false ->
          Defaults.display s |> Expect.equal "" CssBlock

  testPropertyWithConfig fsCheckCfg "block tags are block-level" <|
    fun () ->
      [ "div"; "p"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "section"
        "article"; "main"; "aside"; "nav"; "header"; "footer"; "ul"
        "ol"; "pre"; "blockquote"; "form"; "fieldset" ]
      |> List.iter (fun t ->
        Defaults.display t |> CssLayout.isBlockLevel |> Expect.isTrue t)

  testPropertyWithConfig fsCheckCfg "inline tags not block-level" <|
    fun () ->
      [ "span"; "a"; "strong"; "b"; "em"; "i"; "u"; "s"; "del"
        "code"; "small"; "sub"; "sup"; "abbr"; "cite"; "q"; "mark"
        "br"; "img"; "input"; "label" ]
      |> List.iter (fun t ->
        Defaults.display t |> CssLayout.isBlockLevel |> Expect.isFalse t)
]
