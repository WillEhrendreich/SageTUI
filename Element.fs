namespace SageTUI

type CanvasMode = Braille | HalfBlock | PixelProtocol

type PixelBuffer = {
  Width: int
  Height: int
  Pixels: Color array
}

type CanvasConfig = {
  Draw: int -> int -> PixelBuffer
  Mode: CanvasMode
  Fallback: CanvasMode option
}

[<Measure>] type ms

type Direction = Left | Right | Up | Down

type Easing = float -> float

[<NoEquality; NoComparison>]
type Transition =
  | Fade of duration: int<ms>
  | ColorMorph of duration: int<ms>
  | Wipe of direction: Direction * duration: int<ms>
  | SlideIn of direction: Direction * duration: int<ms>
  | Dissolve of duration: int<ms>
  | Grow of duration: int<ms>
  | Sequence of Transition list
  | Custom of (float -> int -> int -> int)

[<NoEquality; NoComparison>]
type Element =
  | Empty
  | Text of string * Style
  | Row of Element list
  | Column of Element list
  | Overlay of Element list
  | Styled of Style * Element
  | Constrained of Constraint * Element
  | Bordered of BorderStyle * Element
  | Padded of Padding * Element
  | Keyed of key: string * enter: Transition * exit: Transition * Element
  | Canvas of CanvasConfig
  | Aligned of HAlign * VAlign * Element
  | Gapped of int * Element

module El =
  let empty = Empty
  let text s = Text(s, Style.empty)
  let styledText style s = Text(s, style)
  let row children = Row children
  let column children = Column children
  let overlay layers = Overlay layers
  let styled style elem = Styled(style, elem)

  let fg color elem =
    Styled({ Style.empty with Fg = Some color }, elem)

  let bg color elem =
    Styled({ Style.empty with Bg = Some color }, elem)

  let bold elem =
    Styled({ Style.empty with Attrs = TextAttrs.bold }, elem)

  let dim elem =
    Styled({ Style.empty with Attrs = TextAttrs.dim }, elem)

  let italic elem =
    Styled({ Style.empty with Attrs = TextAttrs.italic }, elem)

  let underline elem =
    Styled({ Style.empty with Attrs = TextAttrs.underline }, elem)

  let reverse elem =
    Styled({ Style.empty with Attrs = TextAttrs.reverse }, elem)

  let strikethrough elem =
    Styled({ Style.empty with Attrs = TextAttrs.strikethrough }, elem)

  let width n elem = Constrained(Fixed n, elem)
  let minWidth n elem = Constrained(Min n, elem)
  let maxWidth n elem = Constrained(Max n, elem)
  let height n elem = Column [Constrained(Fixed n, elem)]
  let minHeight n elem = Column [Constrained(Min n, elem)]
  let maxHeight n elem = Column [Constrained(Max n, elem)]
  let fill elem = Constrained(Fill, elem)
  let percentage pct elem = Constrained(Percentage pct, elem)
  let ratio num den elem = Constrained(Ratio(num, den), elem)
  let bordered style elem = Bordered(style, elem)
  let border elem = Bordered(Light, elem)
  let padded p elem = Padded(p, elem)

  let padAll n elem =
    Padded({ Top = n; Right = n; Bottom = n; Left = n }, elem)

  let padHV h v elem =
    Padded({ Top = v; Right = h; Bottom = v; Left = h }, elem)

  let keyed key elem =
    Keyed(key, Fade 0<ms>, Fade 0<ms>, elem)

  let onEnter t elem =
    match elem with
    | Keyed(k, _, exit, child) -> Keyed(k, t, exit, child)
    | other -> Keyed("", t, Fade 0<ms>, other)

  let onExit t elem =
    match elem with
    | Keyed(k, enter, _, child) -> Keyed(k, enter, t, child)
    | other -> Keyed("", Fade 0<ms>, t, other)

  let transition t elem =
    match elem with
    | Keyed(k, _, _, child) -> Keyed(k, t, t, child)
    | other -> Keyed("", t, t, other)

  let viewTransition key elem =
    Keyed(key, ColorMorph 200<ms>, Fade 0<ms>, elem)

  // Alignment helpers
  let align h v elem = Aligned(h, v, elem)
  let alignLeft elem = Aligned(HAlign.Left, VAlign.Top, elem)
  let alignCenter elem = Aligned(HAlign.HCenter, VAlign.Top, elem)
  let alignRight elem = Aligned(HAlign.Right, VAlign.Top, elem)
  let alignTop elem = Aligned(HAlign.Left, VAlign.Top, elem)
  let alignMiddle elem = Aligned(HAlign.Left, VAlign.VCenter, elem)
  let alignBottom elem = Aligned(HAlign.Left, VAlign.Bottom, elem)
  let center elem = Aligned(HAlign.HCenter, VAlign.VCenter, elem)
  let alignBottomRight elem = Aligned(HAlign.Right, VAlign.Bottom, elem)

  // Gap helper
  let gap n elem = Gapped(n, elem)
