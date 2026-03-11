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
  /// User-supplied cell-level transition. The apply function receives the interpolation
  /// progress (0.0–1.0), the cell column and row within the transition area, the
  /// snapshot cell (before state), and the current rendered cell (after state), and
  /// returns the blended cell to write. This enables effects that none of the
  /// built-in transitions can express (e.g. radial reveals, checkerboard, noise).
  | Custom of duration: int<ms> * apply: (float -> int -> int -> PackedCell -> PackedCell -> PackedCell)

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
  | Bordered of BorderStyle * string option * Element
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
  /// Vertically clips a child element to the current viewport, starting at the given row offset.
  /// The visible region is determined by the available height at render time.
  /// Use with El.height to set the viewport size; the child renders at its natural (full) height
  /// into an offscreen buffer, then the viewport slice is copied into the target buffer.
  | Scroll of offset: int * child: Element
  /// Fills the entire layout area with spaces using the given style (merged with inherited).
  /// Use for modal backdrops, colored panels, or any solid background fill.
  | Filled of Style

/// Column width distribution for El.grid.
type GridColumns =
  /// All columns share available width equally (equivalent to all Fill).
  | EqualWidth
  /// Each column gets a fixed pixel width. The last value repeats if the list is shorter than cols.
  | FixedWidths of int list
  /// Each column gets a proportional share of available width.
  /// The last value repeats if the list is shorter than cols.
  | WeightedWidths of int list

/// Inline styled text span for use with El.richText.
/// Spans compose recursively — e.g. Bold [ Fg(Red, [ Literal "error" ]) ] renders bold red text.
type Span =
  | Literal      of string
  | Bold         of Span list
  | Italic       of Span list
  | Underline    of Span list
  | Strikethrough of Span list
  | Fg           of Color * Span list
  | Bg           of Color * Span list
  /// OSC 8 hyperlink. Falls back to underlined text on terminals that do not support it.
  | Link         of href: string * Span list

/// Helper constructors for Span values.
module Span =
  let text s          = Literal s
  let bold children   = Bold children
  let italic children = Italic children
  let underline children = Underline children
  let strikethrough children = Strikethrough children
  let fg color children = Fg(color, children)
  let bg color children = Bg(color, children)
  let link href children = Link(href, children)

/// Lightweight markup parser for Span values.
/// Supported tags: [bold][/bold], [italic][/italic], [underline][/underline],
/// [strikethrough][/strikethrough], [fg:colorname][/fg], [bg:colorname][/bg].
/// Color names: black, red, green, yellow, blue, magenta, cyan, white
/// (append "bright" for bright variants, e.g. "brightred").
module Markup =
  open System
  open System.Text.RegularExpressions

  let private parseColor (name: string) : Color option =
    match name.Trim().ToLowerInvariant() with
    | "black"        -> Some (Named(Black,   Normal))
    | "red"          -> Some (Named(Red,     Normal))
    | "green"        -> Some (Named(Green,   Normal))
    | "yellow"       -> Some (Named(Yellow,  Normal))
    | "blue"         -> Some (Named(Blue,    Normal))
    | "magenta"      -> Some (Named(Magenta, Normal))
    | "cyan"         -> Some (Named(Cyan,    Normal))
    | "white"        -> Some (Named(White,   Normal))
    | "brightblack"  -> Some (Named(Black,   Bright))
    | "brightred"    -> Some (Named(Red,     Bright))
    | "brightgreen"  -> Some (Named(Green,   Bright))
    | "brightyellow" -> Some (Named(Yellow,  Bright))
    | "brightblue"   -> Some (Named(Blue,    Bright))
    | "brightmagenta"-> Some (Named(Magenta, Bright))
    | "brightcyan"   -> Some (Named(Cyan,    Bright))
    | "brightwhite"  -> Some (Named(White,   Bright))
    | s when s.StartsWith("#") ->
      let hex = s.TrimStart('#')
      match hex.Length with
      | 3 ->
        try
          let r = Convert.ToByte(sprintf "%c%c" hex.[0] hex.[0], 16)
          let g = Convert.ToByte(sprintf "%c%c" hex.[1] hex.[1], 16)
          let b = Convert.ToByte(sprintf "%c%c" hex.[2] hex.[2], 16)
          Some (Rgb(r, g, b))
        with _ -> None
      | 6 ->
        try
          let r = Convert.ToByte(hex.[0..1], 16)
          let g = Convert.ToByte(hex.[2..3], 16)
          let b = Convert.ToByte(hex.[4..5], 16)
          Some (Rgb(r, g, b))
        with _ -> None
      | _ -> None
    | _ -> None

  // Token discriminated union for the markup parser
  [<Struct>]
  type private Token = Text of string | Open of string | Close of string

  let private tokenize (input: string) : Token list =
    let tagPattern = Regex(@"\[(/?)([^\]]+)\]")
    let mutable pos = 0
    let tokens = System.Collections.Generic.List<Token>()
    for m in tagPattern.Matches(input) do
      if m.Index > pos then
        tokens.Add(Text input[pos..m.Index - 1])
      let closing = m.Groups[1].Value = "/"
      let tag = m.Groups[2].Value
      tokens.Add(if closing then Close tag else Open tag)
      pos <- m.Index + m.Length
    if pos < input.Length then
      tokens.Add(Text input[pos..])
    tokens |> Seq.toList

  let private buildSpans (tokens: Token list) : Result<Span list, string> =
    // Stack-based parser
    let stack = System.Collections.Generic.Stack<string * System.Collections.Generic.List<Span>>()
    let root  = System.Collections.Generic.List<Span>()
    let mutable current = root
    let mutable error: string option = None

    let push tag =
      stack.Push(tag, current)
      current <- System.Collections.Generic.List<Span>()

    let pop expected =
      if stack.Count = 0 then
        error <- Some (sprintf "unexpected closing tag [/%s]" expected)
      else
        let (openTag, parent) = stack.Pop()
        let children = current |> Seq.toList
        let span =
          match openTag.ToLowerInvariant() with
          | "bold"          -> Ok (Bold children)
          | "italic"        -> Ok (Italic children)
          | "underline"     -> Ok (Underline children)
          | "strikethrough" -> Ok (Strikethrough children)
          | s when s.StartsWith("fg:") ->
            match parseColor (s[3..]) with
            | Some c -> Ok (Fg(c, children))
            | None   -> Error (sprintf "unknown color '%s'" s[3..])
          | s when s.StartsWith("bg:") ->
            match parseColor (s[3..]) with
            | Some c -> Ok (Bg(c, children))
            | None   -> Error (sprintf "unknown color '%s'" s[3..])
          | s when s.StartsWith("link:") -> Ok (Link(s[5..], children))
          | s -> Error (sprintf "unknown tag '[%s]'" s)
        match span with
        | Ok s  -> parent.Add(s); current <- parent
        | Error e -> error <- Some e; current <- parent

    for token in tokens do
      match error with
      | Some _ -> ()
      | None ->
        match token with
        | Text s -> current.Add(Literal s)
        | Open tag -> push tag
        | Close tag -> pop tag

    match error with
    | Some e -> Error e
    | None ->
      match stack.Count with
      | 0 -> Ok (root |> Seq.toList)
      | _ -> Error (sprintf "unclosed tag '[%s]'" (fst (stack.Peek())))

  /// Parse a markup string into a Span list.
  /// Returns Error if any tag is malformed or unclosed.
  let parse (input: string) : Result<Span list, string> =
    tokenize input |> buildSpans

  /// Parse a markup string. Falls back to a single Literal span if parsing fails.
  let parseOrLiteral (input: string) : Span list =
    match parse input with
    | Ok spans -> spans
    | Error _  -> [ Literal input ]

  /// Compute the total display-column width of a span list (ignoring ANSI/markup overhead).
  let rec width (spans: Span list) : int =
    spans |> List.sumBy (fun span ->
      match span with
      | Literal s -> s |> Seq.sumBy (fun c -> RuneWidth.getColumnWidth (System.Text.Rune c))
      | Bold children | Italic children | Underline children
      | Strikethrough children | Fg(_, children) | Bg(_, children)
      | Link(_, children) -> width children)

// ── StatusSegment ─────────────────────────────────────────────────────────────
// Simplified segment type for use with El.statusBar / El.statusBarFull.

/// A simple segment for `El.statusBar` and `El.statusBarFull`.
[<RequireQualifiedAccess>]
type StatusSegment =
  /// Plain text segment.
  | Text    of string
  /// Text with an explicit style override.
  | Styled  of Style * string
  /// A thin separator glyph (│).
  | Sep
  /// Flexible spacer — expands to consume available horizontal space.
  | Fill
  /// A single icon or glyph string.
  | Icon    of string

// ── MenuBarItem ───────────────────────────────────────────────────────────────

/// A single item in a `El.menuBar` row.
type MenuBarItem = {
  /// Unique key identifying this item (used for active-item matching).
  Key:      string
  /// Display label shown in the bar.
  Label:    string
  /// Optional hint text (shortcut or accelerator) shown after the label.
  Shortcut: string option
}

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
  /// Fill all available space (weight 1).
  let fill elem = Constrained(Fill 1, elem)
  /// Fill available space with a given weight. El.fillN 2 gives twice as much space as El.fill.
  let fillN n elem = Constrained(Fill n, elem)
  /// Size as a percentage of available space.
  let percentage pct elem = Constrained(Percentage pct, elem)
  /// Size as a ratio (num/den) of available space.
  let ratio num den elem = Constrained(Ratio(num, den), elem)
  /// Wrap in a border with the given style.
  let bordered style elem = Bordered(style, None, elem)
  /// Wrap in a border with the given style and a title in the top edge.
  let borderedWithTitle title style elem = Bordered(style, Some title, elem)
  /// Wrap in a light border.
  let border elem = Bordered(Light, None, elem)
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

  /// Mark a region of the UI as clickable with the given key.
  /// Wraps `child` in a `Keyed` element with no transitions, so click events
  /// on this region will report `key` to ClickSub handlers.
  ///
  /// Pair with `Sub.clicks` in your Subscribe function:
  /// <code>
  /// // View:
  /// El.clickRegion "submit" submitButton
  ///
  /// // Subscribe (at module level — allocates once):
  /// let clickBindings = Sub.clicks [ "submit", Submit; "cancel", Cancel ]
  /// ...
  /// Subscribe = fun _ -> [ keyBindings; clickBindings ]
  /// </code>
  let clickRegion (key: string) (elem: Element) : Element =
    Keyed(key, Fade 0<ms>, Fade 0<ms>, elem)

  /// Semantic alias for `El.clickRegion`. Names a region of the UI so click events
  /// on it report `key` to `Sub.clicks` handlers. Prefer this name when the intent
  /// is purely routing — "zone" communicates named area, "clickRegion" communicates action.
  ///
  /// <code>
  /// El.zone "sidebar" sidebarContent
  /// El.zone "toolbar" toolbarRow
  /// </code>
  let zone (key: string) (elem: Element) : Element =
    Keyed(key, Fade 0<ms>, Fade 0<ms>, elem)

  /// Like `El.clickRegion` but also attaches enter/exit transitions.
  /// Use when you want both click routing AND animated transitions on the same element.
  ///
  /// <code>
  /// El.clickRegionWith "panel" (Fade 150&lt;ms&gt;) (Fade 100&lt;ms&gt;) panelContent
  /// </code>
  let clickRegionWith (key: string) (enter: Transition) (exit: Transition) (elem: Element) : Element =
    Keyed(key, enter, exit, elem)

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

  // Inline markup parsing — lightweight tag-based styled text.
  // Grammar: literal text interspersed with [tag] ... [/] or [/tag] close markers.
  //
  // Supported tags (case-insensitive):
  //   Attrs:  [bold] [dim] [italic] [under] [blink] [rev] [strike]
  //   Colors: [black] [red] [green] [yellow] [blue] [magenta] [cyan] [white]
  //           [bblack] [bred] [bgreen] [byellow] [bblue] [bmagenta] [bcyan] [bwhite]  (bright variants)
  //           [bg:red] [bg:cyan] ... (background color via bg: prefix)
  //           [rgb:r,g,b] [fg:rgb:r,g,b] [bg:rgb:r,g,b] (RGB values)
  //   Close:  [/] or [/bold] or [/red] etc. — pops the innermost open tag
  //
  // Returns a Row of styled Text nodes. Returns El.empty for empty input.
  // Unknown tags are treated as literal text (the tag characters are preserved).

  /// Private markup parser internals
  module private Markup =
    let private parseColor (tag: string) : Color option =
      let t = tag.ToLowerInvariant()
      match t with
      | "black"   -> Some (Color.Named(BaseColor.Black,   Intensity.Normal))
      | "red"     -> Some (Color.Named(BaseColor.Red,     Intensity.Normal))
      | "green"   -> Some (Color.Named(BaseColor.Green,   Intensity.Normal))
      | "yellow"  -> Some (Color.Named(BaseColor.Yellow,  Intensity.Normal))
      | "blue"    -> Some (Color.Named(BaseColor.Blue,    Intensity.Normal))
      | "magenta" -> Some (Color.Named(BaseColor.Magenta, Intensity.Normal))
      | "cyan"    -> Some (Color.Named(BaseColor.Cyan,    Intensity.Normal))
      | "white"   -> Some (Color.Named(BaseColor.White,   Intensity.Normal))
      | "bblack"   -> Some (Color.Named(BaseColor.Black,   Intensity.Bright))
      | "bred"     -> Some (Color.Named(BaseColor.Red,     Intensity.Bright))
      | "bgreen"   -> Some (Color.Named(BaseColor.Green,   Intensity.Bright))
      | "byellow"  -> Some (Color.Named(BaseColor.Yellow,  Intensity.Bright))
      | "bblue"    -> Some (Color.Named(BaseColor.Blue,    Intensity.Bright))
      | "bmagenta" -> Some (Color.Named(BaseColor.Magenta, Intensity.Bright))
      | "bcyan"    -> Some (Color.Named(BaseColor.Cyan,    Intensity.Bright))
      | "bwhite"   -> Some (Color.Named(BaseColor.White,   Intensity.Bright))
      | _ when t.StartsWith("rgb:") ->
        let parts = t[4..].Split(',')
        match parts.Length with
        | 3 ->
          match System.Byte.TryParse(parts.[0]), System.Byte.TryParse(parts.[1]), System.Byte.TryParse(parts.[2]) with
          | (true, r), (true, g), (true, b) -> Some (Color.Rgb(r, g, b))
          | _ -> None
        | _ -> None
      | _ -> None

    let private parseAttr (tag: string) : TextAttrs option =
      match tag.ToLowerInvariant() with
      | "bold"   -> Some TextAttrs.bold
      | "dim"    -> Some TextAttrs.dim
      | "italic" -> Some TextAttrs.italic
      | "under"  -> Some TextAttrs.underline
      | "blink"  -> Some TextAttrs.blink
      | "rev"    -> Some TextAttrs.reverse
      | "strike" -> Some TextAttrs.strikethrough
      | _ -> None

    type private StyleChange =
      | PushFg of Color
      | PushBg of Color
      | PushAttr of TextAttrs
      | Pop

    let private parseTag (tag: string) : StyleChange option =
      let t = tag.Trim()
      match t with
      | "" -> None
      | "/" -> Some Pop
      | _ when t.StartsWith("/") -> Some Pop
      | _ when t.ToLowerInvariant().StartsWith("bg:") ->
        let rest = t[3..]
        parseColor rest |> Option.map PushBg
      | _ when t.ToLowerInvariant().StartsWith("fg:") ->
        let rest = t[3..]
        parseColor rest |> Option.map PushFg
      | _ ->
        match parseAttr t with
        | Some a -> Some (PushAttr a)
        | None ->
          parseColor t |> Option.map PushFg

    // Split input into alternating (text, tag) segments.
    // Returns list of (literal: string, tag: string option) pairs.
    let private tokenize (input: string) : (string * string option) list =
      let result = System.Collections.Generic.List<string * string option>()
      let mutable i = 0
      let mutable literal = System.Text.StringBuilder()
      while i < input.Length do
        let c = input.[i]
        match c with
        | '[' ->
          let close = input.IndexOf(']', i + 1)
          match close with
          | -1 ->
            // No closing bracket: treat as literal
            literal.Append(c) |> ignore
            i <- i + 1
          | j ->
            let tag = input.[i+1..j-1]
            result.Add(literal.ToString(), Some tag)
            literal.Clear() |> ignore
            i <- j + 1
        | _ ->
          literal.Append(c) |> ignore
          i <- i + 1
      if literal.Length > 0 then
        result.Add(literal.ToString(), None)
      result |> Seq.toList

    // Merge a stack of StyleChanges into a single Style.
    let private stackToStyle (stack: StyleChange list) : Style =
      let mutable fg: Color option = None
      let mutable bg: Color option = None
      let mutable attrs = TextAttrs.none
      for change in stack do
        match change with
        | PushFg c  -> fg    <- Some c
        | PushBg c  -> bg    <- Some c
        | PushAttr a -> attrs <- TextAttrs.combine attrs a
        | Pop        -> ()
      { Fg = fg; Bg = bg; Attrs = attrs }

    /// Parse markup string into a list of (text, style) pairs.
    let parse (input: string) : (string * Style) list =
      let tokens = tokenize input
      let mutable stack: StyleChange list = []
      let results = ResizeArray<string * Style>()
      for (lit, tagOpt) in tokens do
        match lit with
        | "" -> ()
        | s ->
          let style = stackToStyle stack
          results.Add((s, style))
        match tagOpt with
        | None -> ()
        | Some tag ->
          match parseTag tag with
          | None -> ()
          | Some Pop ->
            // Pop last non-Pop entry from stack
            match stack with
            | [] -> ()
            | _ :: rest -> stack <- rest
          | Some change -> stack <- change :: stack
      results |> Seq.toList

  /// Parse inline markup into a `Row` of styled `Text` nodes.
  ///
  /// Supported tags: [bold] [dim] [italic] [under] [blink] [rev] [strike]
  /// for attributes; [red] [green] [blue] [yellow] [cyan] [magenta] [white] [black]
  /// and bright variants [bred] [bgreen] etc. for foreground colors;
  /// [bg:red] [bg:cyan] etc. for background; [rgb:r,g,b] for RGB;
  /// [/] or [/tag] to close the innermost open tag.
  ///
  /// Returns `El.empty` for empty input. Unknown tags are silently ignored.
  ///
  /// Example:
  ///   El.markup "Loaded [bold]42[/bold] records in [green]0.3s[/green]"
  ///   El.markup "[red]ERROR[/] in [bold]main.fs[/bold]:[yellow]42[/]"
  let markup (input: string) : Element =
    match input with
    | "" -> Empty
    | _ ->
      match Markup.parse input with
      | [] -> Empty
      | [(s, st)] -> Text(s, st)
      | parts ->
        Row(parts |> List.map (fun (s, st) ->
          match st = Style.empty with
          | true -> Text(s, Style.empty)
          | false -> Text(s, st)))

  /// Printf-style inline markup builder.
  ///
  /// Example:
  ///   El.markupf "Found [bold]%d[/bold] matches in [cyan]%s[/]" count filename
  let markupf fmt = Printf.ksprintf markup fmt

  /// Render a pre-parsed list of `(text, style)` spans as an inline row.
  /// Each span is rendered as a `Styled` text node. Empty input returns `El.empty`.
  ///
  /// Example:
  ///   El.markupSpans [ ("Error: ", boldRedStyle); ("file not found", italicStyle) ]
  let markupSpans (spans: (string * Style) list) : Element =
    match spans with
    | [] -> Empty
    | [ (t, s) ] -> Styled(s, Text(t, Style.empty))
    | _ -> Row(spans |> List.map (fun (t, s) -> Styled(s, Text(t, Style.empty))))

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

  /// Vertically scroll a child element, showing only the region starting at `offset` rows.
  /// The visible height is determined by the layout area at render time.
  /// The child renders at its natural height into an offscreen buffer; only the viewport
  /// slice [offset .. offset + viewportH) is copied into the target buffer.
  ///
  /// Typical usage — pair with El.height to set the viewport size:
  ///   El.scroll model.scrollOffset (El.column rows) |> El.height 20
  let scroll (offset: int) (child: Element) : Element =
    Scroll(offset, child)

  /// Fill the entire layout area with spaces using the given style.
  /// Use as a backdrop layer in `El.overlay` for modal dialogs.
  let filledStyle (style: Style) : Element = Filled style

  /// Fill the entire layout area with spaces with the given background color.
  /// Idiom: `El.overlay [ El.filledBg Color.Black; modal ]`
  let filledBg (color: Color) : Element =
    Filled { Style.empty with Bg = Some color }

  /// Fill the entire layout area with spaces with the given foreground color.
  let filledFg (color: Color) : Element =
    Filled { Style.empty with Fg = Some color }

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

  /// Memoize a 3-argument view function.
  let lazy3
    (viewFn: 'a -> 'b -> 'c -> Element)
    : ('a -> 'b -> 'c -> Element)
    when 'a : equality and 'b : equality and 'c : equality =
    let mutable prev: struct('a * 'b * 'c * Element) voption = ValueNone
    fun a b c ->
      match prev with
      | ValueSome struct(oldA, oldB, oldC, oldElem) when oldA = a && oldB = b && oldC = c -> oldElem
      | _ ->
        let elem = viewFn a b c
        prev <- ValueSome struct(a, b, c, elem)
        elem

  /// Memoize a 4-argument view function.
  let lazy4
    (viewFn: 'a -> 'b -> 'c -> 'd -> Element)
    : ('a -> 'b -> 'c -> 'd -> Element)
    when 'a : equality and 'b : equality and 'c : equality and 'd : equality =
    let mutable prev: struct('a * 'b * 'c * 'd * Element) voption = ValueNone
    fun a b c d ->
      match prev with
      | ValueSome struct(oldA, oldB, oldC, oldD, oldElem) when oldA = a && oldB = b && oldC = c && oldD = d -> oldElem
      | _ ->
        let elem = viewFn a b c d
        prev <- ValueSome struct(a, b, c, d, elem)
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
        Bordered(Light, None,
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
          | Fill w -> if w = 1 then "Fill" else sprintf "Fill%d" w
          | Fixed n -> sprintf "W%d" n
          | Percentage p -> sprintf "%d%%" p
          | Min n -> sprintf "≥%d" n
          | Max n -> sprintf "≤%d" n
          | Ratio(n, d) -> sprintf "%d/%d" n d
        label cStr (dbg (depth + 1) child)
      | Bordered(bs, _, child) ->
        label (sprintf "Brd:%A" bs) (Bordered(bs, None, dbg (depth + 1) child))
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
      | Scroll(off, child) ->
        label (sprintf "Scroll:%d" off) (Scroll(off, dbg (depth + 1) child))
      | Filled _ -> label "Filled" elem
    dbg 0 elem

  /// Render a list of Span values as an inline Row of styled Text elements.
  ///
  /// Each span run becomes a `Constrained(Fixed n, Text(...))` cell so that the
  /// Row allocates exactly the display-column width for each run.  Long lines do
  /// not wrap; the caller should constrain the outer element to the desired width.
  ///
  /// Example:
  ///   El.richText [
  ///     Span.fg Color.red [ Span.bold [ Span.text "ERROR" ] ]
  ///     Span.text " file not found"
  ///   ]
  let richText (spans: Span list) : Element =
    let rec spanToElements (style: Style) (span: Span) : Element list =
      match span with
      | Literal s ->
        let w = s |> Seq.sumBy (fun c -> RuneWidth.getColumnWidth (System.Text.Rune c))
        [ Constrained(Fixed w, Text(s, style)) ]
      | Bold children ->
        let s = { style with Attrs = { Value = style.Attrs.Value ||| TextAttrs.bold.Value } }
        children |> List.collect (spanToElements s)
      | Italic children ->
        let s = { style with Attrs = { Value = style.Attrs.Value ||| TextAttrs.italic.Value } }
        children |> List.collect (spanToElements s)
      | Underline children ->
        let s = { style with Attrs = { Value = style.Attrs.Value ||| TextAttrs.underline.Value } }
        children |> List.collect (spanToElements s)
      | Strikethrough children ->
        let s = { style with Attrs = { Value = style.Attrs.Value ||| TextAttrs.strikethrough.Value } }
        children |> List.collect (spanToElements s)
      | Fg(color, children) ->
        let s = { style with Fg = Some color }
        children |> List.collect (spanToElements s)
      | Bg(color, children) ->
        let s = { style with Bg = Some color }
        children |> List.collect (spanToElements s)
      | Link(_, children) ->
        let s = { style with Attrs = { Value = style.Attrs.Value ||| TextAttrs.underline.Value } }
        children |> List.collect (spanToElements s)

    let baseStyle = { Fg = None; Bg = None; Attrs = TextAttrs.none }
    let cells = spans |> List.collect (spanToElements baseStyle)
    match cells with
    | []  -> Empty
    | [c] -> c
    | _   -> Row cells

  /// Lay children out in a grid of `cols` columns.
  ///
  /// `colWidths` controls how width is distributed across columns:
  ///   - `EqualWidth` — all columns share available width equally
  ///   - `FixedWidths [10; 20]` — first column is 10 wide, second 20; last value repeats
  ///   - `WeightedWidths [1; 2; 3]` — proportional widths (1:2:3 ratio)
  ///
  /// If the child count is not a multiple of `cols`, the last row is padded with Empty.
  let grid (cols: int) (colWidths: GridColumns) (children: Element list) : Element =
    match cols <= 0 with
    | true -> Empty
    | false ->
      let padded =
        let rem = List.length children % cols
        match rem with
        | 0 -> children
        | _ -> children @ List.replicate (cols - rem) Empty
      let constraints =
        match colWidths with
        | EqualWidth -> List.replicate cols (Fill 1)
        | FixedWidths [] -> List.replicate cols (Fill 1)
        | FixedWidths widths ->
          let lastW = List.last widths
          [ for i in 0 .. cols - 1 ->
              Fixed (List.tryItem i widths |> Option.defaultValue lastW) ]
        | WeightedWidths [] -> List.replicate cols (Fill 1)
        | WeightedWidths weights ->
          let lastW = List.last weights
          let totalW = List.sum weights
          match totalW with
          | 0 -> List.replicate cols (Fill 1)
          | _ ->
            [ for i in 0 .. cols - 1 ->
                let w = List.tryItem i weights |> Option.defaultValue lastW
                Ratio(w, totalW) ]
      let rows =
        padded
        |> List.chunkBySize cols
        |> List.map (fun rowItems ->
          let cells =
            rowItems |> List.mapi (fun i item ->
              Constrained(List.item i constraints, item))
          Row cells)
      Column rows

  /// Lay children out in a grid of `cols` equal-width columns.
  /// Shorthand for `El.grid cols EqualWidth children`.
  let gridEven (cols: int) (children: Element list) : Element =
    grid cols EqualWidth children

  /// Quick tabular layout from string headers and pre-rendered cell Elements.
  ///
  /// Headers are rendered bold. A `─` separator divides header from body rows.
  /// All columns are equal-width (`EqualWidth`). For typed data, fixed widths, or
  /// fill columns, use `Table.view` from Widgets.fs.
  ///
  /// Example:
  ///   El.table ["Name"; "Age"; "City"] [
  ///     [El.text "Alice"; El.text "30"; El.text "London"]
  ///     [El.text "Bob";   El.text "25"; El.text "Paris" ]
  ///   ]
  let table (headers: string list) (rows: Element list list) : Element =
    let cols = List.length headers
    match cols with
    | 0 -> Empty
    | _ ->
      let headerRow =
        headers
        |> List.map (fun h -> Text(h, { Fg = None; Bg = None; Attrs = TextAttrs.bold }))
        |> grid cols EqualWidth
      // Each separator cell is a long ─ string that clips to its column width,
      // producing a seamless full-row horizontal rule.
      let sepStr = System.String('─', 256)
      let sepRow = List.replicate cols (text sepStr) |> grid cols EqualWidth
      let bodyFlat =
        rows
        |> List.collect (fun row ->
          let n = List.length row
          match n < cols with
          | true  -> row @ List.replicate (cols - n) Empty
          | false -> List.truncate cols row)
      let bodyGrid = bodyFlat |> grid cols EqualWidth
      Column [ headerRow; sepRow; bodyGrid ]

  /// Subtree memoisation by **reference equality**.
  ///
  /// Returns a function that calls `factory` the first time or when the
  /// argument is a different heap object (via `obj.ReferenceEquals`).
  /// On subsequent calls with the same reference the cached `Element` is
  /// returned without re-invoking `factory`.
  ///
  /// ⚠ Value types (`int`, `struct`) are boxed on each call, so
  /// `ReferenceEquals` will always return `false` for them — `factory`
  /// will be called every time. Use `lazyEq` for value types.
  ///
  /// Typical usage (declare at module/let scope, never inside a view function):
  ///
  ///   let renderItems = El.lazyRef (fun (items: Item array) -> El.column (items |> Array.toList |> List.map renderItem))
  let lazyRef (factory: 'a -> Element) : 'a -> Element =
    let mutable lastArg    : obj     = null
    let mutable lastResult : Element = Empty
    fun arg ->
      let boxed = box arg
      match obj.ReferenceEquals(lastArg, boxed) with
      | true -> lastResult
      | false ->
        let result = factory arg
        lastArg    <- boxed
        lastResult <- result
        result

  /// Subtree memoisation by **structural equality** (`=`).
  ///
  /// Returns a function that calls `factory` the first time or when
  /// the argument is not equal to the last argument.  Equal values
  /// reuse the cached `Element` without re-invoking `factory`.
  ///
  /// Works correctly for value types, strings, records, and any type
  /// with an `equality` constraint.
  ///
  /// Typical usage (declare at module/let scope, never inside a view function):
  ///
  ///   let renderCount = El.lazyEq (fun (n: int) -> El.text (sprintf "Items: %d" n))
  let lazyEq<'a when 'a : equality> (factory: 'a -> Element) : 'a -> Element =
    let mutable lastArg    : 'a option = None
    let mutable lastResult : Element   = Empty
    fun arg ->
      match lastArg with
      | Some prev when prev = arg -> lastResult
      | _ ->
        let result = factory arg
        lastArg    <- Some arg
        lastResult <- result
        result

  // ── StatusSegment / MenuBar helpers ─────────────────────────────────────────

  let private segToElement (seg: StatusSegment) : Element =
    match seg with
    | StatusSegment.Text  s        -> text s
    | StatusSegment.Styled(sty, s) -> styled sty (text s)
    | StatusSegment.Sep            -> text "│"
    | StatusSegment.Fill           -> fill Empty
    | StatusSegment.Icon  s        -> text s

  let private segStringWidth (s: string) : int =
    let mutable w = 0
    for rune in s.EnumerateRunes() do
      w <- w + RuneWidth.getColumnWidth rune
    w

  // Wrap a non-Fill segment in Constrained(Fixed w) so it takes exactly its
  // content width rather than defaulting to Fill 1 in a Row layout.
  let private segToFixed (seg: StatusSegment) : Element =
    match seg with
    | StatusSegment.Fill -> Constrained(Fill 1, Empty)
    | _ ->
      let e = segToElement seg
      let w =
        match seg with
        | StatusSegment.Text s | StatusSegment.Icon s -> segStringWidth s
        | StatusSegment.Styled(_, s)                  -> segStringWidth s
        | StatusSegment.Sep                           -> 1
        | StatusSegment.Fill                          -> 0
      Constrained(Fixed w, e)

  /// Render a single-row status bar with left and right segment lists.
  /// Left segments are left-aligned; right segments are right-aligned;
  /// a flexible spacer fills the gap between them.
  let statusBar (left: StatusSegment list) (right: StatusSegment list) : Element =
    let leftPart  = left  |> List.map segToFixed
    let rightPart = right |> List.map segToFixed
    match left, right with
    | [], [] -> Empty
    | _, [] -> Row leftPart
    | [], _ -> Row (Constrained(Fill 1, Empty) :: rightPart)
    | _, _  -> Row (leftPart @ [ Constrained(Fill 1, Empty) ] @ rightPart)

  /// Render a single-row status bar from an ordered flat segment list.
  /// `Fill` segments expand to consume available space; all other segments
  /// take exactly their content width.
  let statusBarFull (segs: StatusSegment list) : Element =
    match segs with
    | [] -> Empty
    | _  -> Row (segs |> List.map segToFixed)

  /// Render a single-row menu bar from an ordered list of `MenuBarItem` values.
  /// The active item (matched by `Key`) is highlighted with bold + reverse video.
  let menuBar (items: MenuBarItem list) (activeItem: string option) : Element =
    match items with
    | [] -> Empty
    | _  ->
      let renderItem (item: MenuBarItem) : Element =
        let label =
          match item.Shortcut with
          | None   -> item.Label
          | Some s -> sprintf "%s (%s)" item.Label s
        let isActive =
          match activeItem with
          | Some k -> k = item.Key
          | None   -> false
        match isActive with
        | true  ->
          let boldRev = { Style.empty with Attrs = TextAttrs.combine TextAttrs.bold TextAttrs.reverse }
          styled boldRev (text label)
        | false -> text label
      let sep = text " "
      items
      |> List.mapi (fun i it -> (i, it))
      |> List.collect (fun (i, it) ->
          match i with
          | 0 -> [ renderItem it ]
          | _ -> [ sep; renderItem it ])
      |> Row

/// Computation expression buildersfor declarative, imperative-style layout construction.
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

  /// Build a `Column`using computation expression syntax.
  /// Supports `yield`, `yield!`, `if`, and `for`.
  let col = ColumnBuilder()

  /// Build a `Row` using computation expression syntax.
  /// Supports `yield`, `yield!`, `if`, and `for`.
  let row = RowBuilder()

  /// Alias for `col` — build a `Column` using CE syntax.
  let view = ColumnBuilder()
