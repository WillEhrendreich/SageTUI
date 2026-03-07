namespace SageTUI.Html

open SageTUI

type FlexDir = FdRow | FdColumn

type CssDisplay =
  | CssBlock
  | CssInline
  | CssFlex of FlexDir
  | CssInlineBlock
  | CssListItem
  | CssTable
  | CssTableRow
  | CssTableCell
  | CssNone

[<Struct>]
type CssSpec = { Ids: int; Cls: int; Tags: int }

type CssSel =
  | TagSel of string
  | ClassSel of string
  | IdSel of string
  | StarSel
  | CompSel of CssSel list
  | DescSel of CssSel * CssSel
  | ChildSel of CssSel * CssSel

type CssRule = { Sel: CssSel; Props: (string * string) list }

type ComputedStyle = {
  Display: CssDisplay
  Color: Color option
  BgColor: Color option
  Bold: bool
  Italic: bool
  Deco: TextAttrs
  Width: int option
  MinW: int option
  MaxW: int option
  Pad: Padding
  Bdr: BorderStyle option
  FlexGrow: int
  FlexShrink: int
  Gap: int option
}

module ComputedStyle =
  let empty = {
    Display = CssBlock; Color = None; BgColor = None
    Bold = false; Italic = false; Deco = TextAttrs.none
    Width = None; MinW = None; MaxW = None
    Pad = Padding.zero; Bdr = None
    FlexGrow = 0; FlexShrink = 1; Gap = None
  }

module Defaults =
  let display (tag: string) : CssDisplay =
    match tag with
    | "div" | "p" | "section" | "article" | "main" | "aside" | "nav"
    | "header" | "footer" | "form" | "fieldset" | "blockquote"
    | "figure" | "figcaption" | "details" | "summary"
    | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"
    | "pre" | "address" | "dl" | "dd" | "dt"
    | "ul" | "ol" -> CssBlock
    | "span" | "a" | "strong" | "b" | "em" | "i" | "u" | "s" | "del" | "strike"
    | "code" | "small" | "sub" | "sup" | "abbr" | "cite" | "q" | "mark"
    | "time" | "data" | "var" | "samp" | "kbd" | "dfn" | "bdo" | "bdi"
    | "label" | "output" | "br" | "img" | "input" -> CssInline
    | "li" -> CssListItem
    | "table" -> CssTable
    | "thead" | "tbody" | "tfoot" -> CssTable
    | "tr" -> CssTableRow
    | "td" | "th" -> CssTableCell
    | "hr" | "button" | "textarea" | "select" -> CssBlock
    | _ -> CssBlock

  let inherits (prop: string) : bool =
    match prop with
    | "color" | "font-weight" | "font-style" | "font-family"
    | "font-size" | "line-height" | "text-align" | "visibility"
    | "cursor" | "list-style" | "list-style-type" -> true
    | _ -> false

module Spec =
  let zero : CssSpec = { Ids = 0; Cls = 0; Tags = 0 }

  let rec compute (sel: CssSel) : CssSpec =
    match sel with
    | IdSel _ -> { Ids = 1; Cls = 0; Tags = 0 }
    | ClassSel _ -> { Ids = 0; Cls = 1; Tags = 0 }
    | TagSel _ -> { Ids = 0; Cls = 0; Tags = 1 }
    | StarSel -> zero
    | CompSel parts ->
      parts
      |> List.map compute
      |> List.fold
        (fun a s -> { Ids = a.Ids + s.Ids; Cls = a.Cls + s.Cls; Tags = a.Tags + s.Tags })
        zero
    | DescSel (a, b) | ChildSel (a, b) ->
      let sa = compute a
      let sb = compute b
      { Ids = sa.Ids + sb.Ids; Cls = sa.Cls + sb.Cls; Tags = sa.Tags + sb.Tags }

  let cmp (a: CssSpec) (b: CssSpec) : int =
    match a.Ids - b.Ids with
    | 0 ->
      match a.Cls - b.Cls with
      | 0 -> a.Tags - b.Tags
      | d -> d
    | d -> d
