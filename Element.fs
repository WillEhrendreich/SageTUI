namespace SageTUI

/// Canvas pixel encoding mode.
type CanvasMode = Braille | HalfBlock | PixelProtocol

/// A buffer of RGBA pixels for canvas rendering.
type PixelBuffer = {
  Width: int
  Height: int
  Pixels: Color array
}

/// Configuration for a Canvas element.
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
  /// Slides new content in from the given direction while the snapshot slides out.
  | SlideIn of direction: Direction * duration: int<ms>
  | Dissolve of duration: int<ms>
  /// Reveals new content expanding outward from the center using Chebyshev distance.
  /// Even-dimensioned areas: snapshot shown at t=0 (no cell at Chebyshev distance 0).
  /// Odd-dimensioned areas: single center cell revealed at t=0.
  | Grow of duration: int<ms>
  /// Plays sub-transitions sequentially on the same keyed element.
  /// <remarks>
  /// Sub-transitions share the original snapshot captured at the start of the Sequence —
  /// each sub-transition blends from that same snapshot to the current frame content.
  /// For pixel-perfect independent chaining, use multiple <c>El.keyed</c> elements
  /// with staggered enter/exit transitions instead.
  /// </remarks>
  | Sequence of Transition list
  /// Not yet implemented — has no visual effect.
  | Custom of (float -> int -> int -> int)

/// The core UI element type. All views are trees of Element values.
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
  /// Responsive layout breakpoints. Each tuple is (minWidth, element).
  /// The last breakpoint whose minWidth ≤ available width is selected.
  /// Breakpoints are evaluated from first to last; use ascending order.
  | Responsive of (int * Element) list
  /// Height-based responsive layout breakpoints. Each tuple is (minHeight, element).
  /// The last breakpoint whose minHeight ≤ available height is selected.
  /// Composes with Responsive for 2D breakpoints.
  | ResponsiveH of (int * Element) list

/// Element constructors and combinators. All functions return Element values.
module El =
  /// An empty element that renders nothing.
  let empty = Empty
  /// Create a plain text element.
  let text s = Text(s, Style.empty)
  /// Create a text element with explicit styling.
  let styledText style s = Text(s, style)
  /// Arrange children horizontally in a row.
  let row children = Row children
  /// Arrange children vertically in a column.
  let column children = Column children
  /// Layer elements on top of each other (last = frontmost).
  let overlay layers = Overlay layers
  /// Apply a Style to an element.
  let styled style elem = Styled(style, elem)

  /// Set foreground color.
  let fg color elem =
    Styled({ Style.empty with Fg = Some color }, elem)

  /// Conditionally apply a foreground color.
  /// Returns `elem` unchanged when `colorOpt = None`; wraps in a foreground `Styled` node when `Some`.
  let fgOpt (colorOpt: Color option) (elem: Element) : Element =
    colorOpt |> Option.fold (fun e c -> fg c e) elem

  /// Set background color.
  let bg color elem =
    Styled({ Style.empty with Bg = Some color }, elem)

  /// Conditionally apply a background color.
  /// Returns `elem` unchanged when `colorOpt = None`; wraps in a background `Styled` node when `Some`.
  let bgOpt (colorOpt: Color option) (elem: Element) : Element =
    colorOpt |> Option.fold (fun e c -> bg c e) elem

  /// Make text bold.
  let bold elem =
    Styled({ Style.empty with Attrs = TextAttrs.bold }, elem)

  /// Make text dim.
  let dim elem =
    Styled({ Style.empty with Attrs = TextAttrs.dim }, elem)

  /// Make text italic.
  let italic elem =
    Styled({ Style.empty with Attrs = TextAttrs.italic }, elem)

  /// Underline text.
  let underline elem =
    Styled({ Style.empty with Attrs = TextAttrs.underline }, elem)

  /// Reverse foreground and background colors.
  let reverse elem =
    Styled({ Style.empty with Attrs = TextAttrs.reverse }, elem)

  /// Strike through text.
  let strikethrough elem =
    Styled({ Style.empty with Attrs = TextAttrs.strikethrough }, elem)

  /// Set exact width in cells.
  let width n elem = Constrained(Fixed n, elem)
  /// Set minimum width in cells.
  let minWidth n elem = Constrained(Min n, elem)
  /// Set maximum width in cells.
  let maxWidth n elem = Constrained(Max n, elem)
  /// Set exact height in rows.
  let height n elem = Column [Constrained(Fixed n, elem)]
  /// Set minimum height in rows.
  let minHeight n elem = Column [Constrained(Min n, elem)]
  /// Set maximum height in rows.
  let maxHeight n elem = Column [Constrained(Max n, elem)]
  /// Fill all available space.
  let fill elem = Constrained(Fill, elem)
  /// Size as a percentage of available space.
  let percentage pct elem = Constrained(Percentage pct, elem)
  /// Size as a ratio (num/den) of available space.
  let ratio num den elem = Constrained(Ratio(num, den), elem)
  /// Wrap in a border with the given style.
  let bordered style elem = Bordered(style, elem)
  /// Wrap in a light border.
  let border elem = Bordered(Light, elem)
  /// Add padding around an element.
  let padded p elem = Padded(p, elem)

  /// Add equal padding on all sides.
  let padAll n elem =
    Padded({ Top = n; Right = n; Bottom = n; Left = n }, elem)

  /// Add horizontal and vertical padding.
  let padHV h v elem =
    Padded({ Top = v; Right = h; Bottom = v; Left = h }, elem)

  /// Tag an element with a key for transition tracking.
  ///
  /// <remarks>
  /// Layout modifiers such as <c>El.width</c>, <c>El.height</c>, and <c>El.fill</c>
  /// must be applied <b>after</b> <c>El.keyed</c>, not inside the wrapped element,
  /// if they need to influence the parent layout pass.
  ///
  /// The keyed wrapper is transparent to the parent layout — it passes through to
  /// its child but does not itself carry a size constraint. Only a Constrained wrapper
  /// on the outside of the keyed element is visible to Row/Column layout.
  ///
  /// <code>
  /// // ✅ Correct — width 40 is visible to the parent Row
  /// El.keyed "panel" content |> El.width 40
  ///
  /// // ⚠️ Width 40 is invisible to the parent Row; "panel" acts as implicit Fill
  /// El.keyed "panel" (content |> El.width 40)
  /// </code>
  /// </remarks>
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

  // Canvas helpers
  let canvas (draw: int -> int -> PixelBuffer) =
    Canvas { Draw = draw; Mode = HalfBlock; Fallback = None }

  let canvasBraille (draw: int -> int -> PixelBuffer) =
    Canvas { Draw = draw; Mode = Braille; Fallback = None }

  let canvasWithMode (mode: CanvasMode) (draw: int -> int -> PixelBuffer) =
    Canvas { Draw = draw; Mode = mode; Fallback = None }

  // Formatted text
  let textf fmt = Printf.ksprintf text fmt

  // Text wrapping (composition — no new DU case)
  let private wordWrap (maxWidth: int) (s: string) =
    match maxWidth <= 0 with
    | true -> [ "" ]
    | false ->
      let lines = System.Collections.Generic.List<string>()
      for rawLine in s.Split('\n') do
        let words = rawLine.Split(' ')
        let current = System.Text.StringBuilder()
        let mutable col = 0
        for word in words do
          let wordLen =
            let mutable w = 0
            for rune in word.EnumerateRunes() do
              w <- w + RuneWidth.getColumnWidth rune
            w
          match col with
          | 0 ->
            match wordLen > maxWidth with
            | true ->
              let mutable charCol = 0
              for rune in word.EnumerateRunes() do
                let rw = RuneWidth.getColumnWidth rune
                match charCol + rw > maxWidth with
                | true ->
                  lines.Add(current.ToString())
                  current.Clear() |> ignore
                  current.Append(rune.ToString()) |> ignore
                  charCol <- rw
                | false ->
                  current.Append(rune.ToString()) |> ignore
                  charCol <- charCol + rw
              col <- charCol
            | false ->
              current.Append(word) |> ignore
              col <- wordLen
          | _ ->
            match col + 1 + wordLen > maxWidth with
            | true ->
              lines.Add(current.ToString())
              current.Clear() |> ignore
              current.Append(word) |> ignore
              col <- wordLen
            | false ->
              current.Append(' ').Append(word) |> ignore
              col <- col + 1 + wordLen
        lines.Add(current.ToString())
      lines |> Seq.toList

  let paragraph (maxWidth: int) (s: string) =
    wordWrap maxWidth s
    |> List.map text
    |> column

  let paragraphStyled (style: Style) (maxWidth: int) (s: string) =
    wordWrap maxWidth s
    |> List.map (styledText style)
    |> column

  /// Responsive layout breakpoints. Provide a list of `(minWidth, element)` pairs in
  /// ascending order; the last pair whose `minWidth ≤ availableWidth` is rendered.
  /// This is the TUI equivalent of CSS media queries.
  ///
  /// Example:
  ///   El.responsive [
  ///     (0,  El.column [ El.text "Compact" ])
  ///     (60, El.row [ El.text "Normal" ])
  ///     (120, El.row [ El.text "Wide"; sidePanel ])
  ///   ]
  let responsive (breakpoints: (int * Element) list) : Element =
    Responsive breakpoints

  /// Height-based responsive breakpoints. Like `El.responsive` but selects by `area.Height`.
  /// Composes with `El.responsive` for 2D breakpoints:
  ///   El.responsive [ (0, El.responsiveH [(0, small); (20, large)]) ]
  let responsiveH (breakpoints: (int * Element) list) : Element =
    ResponsiveH breakpoints

  /// Memoize a view function: returns cached Element when input equals the previous input.
  /// Declare at module level to persist the cache across renders.
  /// Use at module level: `let lazyCounter = El.lazy' Counter.view`
  ///
  /// Requires `'a : equality` — if your model type contains functions or [<NoEquality>] types,
  /// you will get a compile error here. Use `El.lazy'With` with a custom comparer instead.
  let lazy' (viewFn: 'a -> Element) : ('a -> Element) when 'a : equality =
    let mutable prev: struct('a * Element) voption = ValueNone
    fun model ->
      match prev with
      | ValueSome struct(oldModel, oldElem) when oldModel = model -> oldElem
      | _ ->
        let elem = viewFn model
        prev <- ValueSome struct(model, elem)
        elem

  /// Memoize a 2-argument view function.
  let lazy2 (viewFn: 'a -> 'b -> Element) : ('a -> 'b -> Element) when 'a : equality and 'b : equality =
    let mutable prev: struct('a * 'b * Element) voption = ValueNone
    fun a b ->
      match prev with
      | ValueSome struct(oldA, oldB, oldElem) when oldA = a && oldB = b -> oldElem
      | _ ->
        let elem = viewFn a b
        prev <- ValueSome struct(a, b, elem)
        elem

  /// Memoize a view function with a custom equality comparer.
  /// Use when `'a` does not support structural equality (e.g., contains functions).
  let lazy'With (comparer: 'a -> 'a -> bool) (viewFn: 'a -> Element) : ('a -> Element) =
    let mutable prev: struct('a * Element) voption = ValueNone
    fun model ->
      match prev with
      | ValueSome struct(oldModel, oldElem) when comparer oldModel model -> oldElem
      | _ ->
        let elem = viewFn model
        prev <- ValueSome struct(model, elem)
        elem

  /// Recursively wraps each element with a colored border showing its type and constraint.
  /// Useful for understanding how the layout engine allocates space.
  let debugLayout (elem: Element) : Element =
    let colors = [|
      Color.Named(Cyan, Bright)
      Color.Named(Magenta, Bright)
      Color.Named(Yellow, Bright)
      Color.Named(Green, Bright)
      Color.Named(Red, Bright)
      Color.Named(Blue, Bright)
    |]
    let rec dbg depth elem =
      let color = colors[depth % colors.Length]
      let label tag inner =
        Bordered(Light,
          Column [
            Text(tag, { Fg = Some color; Bg = None; Attrs = TextAttrs.bold })
            inner
          ])
        |> fun e -> Styled({ Fg = Some color; Bg = None; Attrs = TextAttrs.none }, e)
      match elem with
      | Empty -> label "·" Empty
      | Text(s, st) -> label (sprintf "T\"%s\"" (match s.Length > 12 with | true -> s[..11] + "…" | false -> s)) (Text(s, st))
      | Row children ->
        label "Row" (Row(children |> List.map (dbg (depth + 1))))
      | Column children ->
        label "Col" (Column(children |> List.map (dbg (depth + 1))))
      | Overlay layers ->
        label "Ov" (Overlay(layers |> List.map (dbg (depth + 1))))
      | Styled(st, child) ->
        label "Sty" (Styled(st, dbg (depth + 1) child))
      | Constrained(c, child) ->
        let cStr =
          match c with
          | Fill -> "Fill"
          | Fixed n -> sprintf "W%d" n
          | Percentage p -> sprintf "%d%%" p
          | Min n -> sprintf "≥%d" n
          | Max n -> sprintf "≤%d" n
          | Ratio(n, d) -> sprintf "%d/%d" n d
        label cStr (dbg (depth + 1) child)
      | Bordered(bs, child) ->
        label (sprintf "Brd:%A" bs) (Bordered(bs, dbg (depth + 1) child))
      | Padded(p, child) ->
        label (sprintf "Pad%d,%d,%d,%d" p.Top p.Right p.Bottom p.Left) (Padded(p, dbg (depth + 1) child))
      | Keyed(k, ent, ext, child) ->
        label (sprintf "Key:%s" k) (Keyed(k, ent, ext, dbg (depth + 1) child))
      | Canvas _ -> label "Canvas" elem
      | Aligned(h, v, child) ->
        label (sprintf "Align:%A,%A" h v) (Aligned(h, v, dbg (depth + 1) child))
      | Gapped(g, child) ->
        label (sprintf "Gap%d" g) (Gapped(g, dbg (depth + 1) child))
      | Responsive breakpoints ->
        label "Resp" (Responsive(breakpoints |> List.map (fun (minW, child) -> (minW, dbg (depth + 1) child))))
      | ResponsiveH breakpoints ->
        label "RespH" (ResponsiveH(breakpoints |> List.map (fun (minH, child) -> (minH, dbg (depth + 1) child))))
    dbg 0 elem

/// Computation expression builders for declarative, imperative-style layout construction.
///
/// Instead of wrapping lists in `[` `]`:
///   `El.column [ El.text "A"; if cond then El.text "B" else El.empty ]`
///
/// Use CE syntax:
///   `col { "A"; if cond then "B" }`
///
/// `yield!` splices an `Element list` and `for` iterates sequences:
///   `col { yield! headerItems; for x in items do row { El.text x.Name } }`
module View =
  type ColumnBuilder() =
    member _.Yield(e: Element) = [e]
    member _.Yield(s: string) = [El.text s]
    member _.YieldFrom(es: Element list) = es
    member _.YieldFrom(e: Element) = [e]
    member _.Zero() = []
    member _.Combine(a: Element list, b: Element list) = a @ b
    member _.Delay(f: unit -> Element list) = f()
    member _.For(xs: #seq<'a>, f: 'a -> Element list) =
      xs |> Seq.toList |> List.collect f
    member _.Run(es: Element list) = El.column es

  type RowBuilder() =
    member _.Yield(e: Element) = [e]
    member _.Yield(s: string) = [El.text s]
    member _.YieldFrom(es: Element list) = es
    member _.YieldFrom(e: Element) = [e]
    member _.Zero() = []
    member _.Combine(a: Element list, b: Element list) = a @ b
    member _.Delay(f: unit -> Element list) = f()
    member _.For(xs: #seq<'a>, f: 'a -> Element list) =
      xs |> Seq.toList |> List.collect f
    member _.Run(es: Element list) = El.row es

  /// Build a `Column` using computation expression syntax.
  /// Supports `yield`, `yield!`, `if`, and `for`.
  let col = ColumnBuilder()

  /// Build a `Row` using computation expression syntax.
  /// Supports `yield`, `yield!`, `if`, and `for`.
  let row = RowBuilder()

  /// Alias for `col` — build a `Column` using CE syntax.
  let view = ColumnBuilder()
