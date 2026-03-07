namespace SageTUI

open System

[<Struct>]
type ElementNode =
  { Kind: byte
    StylePacked: uint64
    AttrsPacked: uint16
    ConstraintKind: byte
    ConstraintVal: int16
    FirstChild: int
    NextSibling: int
    DataStart: int
    DataLen: int }

[<Struct>]
type NodeHandle = private NodeHandle of int

module NodeHandle =
  let value (NodeHandle idx) = idx

type FrameArena =
  { Nodes: ElementNode array
    mutable NodeCount: int
    TextBuf: char array
    mutable TextPos: int
    LayoutScratch: int array
    mutable LayoutPos: int
    mutable Generation: int }

module FrameArena =
  let create maxNodes maxChars maxLayoutScratch =
    { Nodes = Array.zeroCreate maxNodes
      NodeCount = 0
      TextBuf = Array.zeroCreate maxChars
      TextPos = 0
      LayoutScratch = Array.zeroCreate maxLayoutScratch
      LayoutPos = 0
      Generation = 0 }

  let reset arena =
    arena.NodeCount <- 0
    arena.TextPos <- 0
    arena.LayoutPos <- 0
    arena.Generation <- arena.Generation + 1

  let allocNode arena =
    if arena.NodeCount >= arena.Nodes.Length then
      failwith (sprintf "FrameArena overflow: %d nodes exceeds capacity %d"
        arena.NodeCount arena.Nodes.Length)
    let idx = arena.NodeCount
    arena.NodeCount <- arena.NodeCount + 1
    NodeHandle idx

  let allocText (text: string) arena =
    let start = arena.TextPos
    text.CopyTo(0, arena.TextBuf, start, text.Length)
    arena.TextPos <- arena.TextPos + text.Length
    (start, text.Length)

  let getNode (NodeHandle idx) arena = arena.Nodes.[idx]

module Arena =
  let packStylePair (style: Style) : uint64 =
    let fg = style.Fg |> Option.map PackedColor.pack |> Option.defaultValue 0
    let bg = style.Bg |> Option.map PackedColor.pack |> Option.defaultValue 0
    (uint64 (uint32 fg) <<< 32) ||| uint64 (uint32 bg)

  let unpackStyleFg (packed: uint64) = int (packed >>> 32)
  let unpackStyleBg (packed: uint64) = int (packed &&& 0xFFFFFFFFUL)

  let packConstraint (c: Constraint) =
    match c with
    | Fixed n -> 0uy, int16 n
    | Min n -> 1uy, int16 n
    | Max n -> 2uy, int16 n
    | Percentage n -> 3uy, int16 n
    | Fill -> 4uy, 0s
    | Ratio(n, d) -> 5uy, int16 ((n <<< 8) ||| (d &&& 0xFF))

  let packBorderStyle (bs: BorderStyle) =
    match bs with
    | Light -> 0uy | Heavy -> 1uy | Double -> 2uy
    | Rounded -> 3uy | Ascii -> 4uy

  let packPadding (p: Padding) =
    (p.Top <<< 16) ||| (p.Right &&& 0xFFFF),
    (p.Bottom <<< 16) ||| (p.Left &&& 0xFFFF)

  let packTransition (t: Transition) =
    match t with
    | Fade _ -> 0uy | ColorMorph _ -> 1uy | Wipe _ -> 2uy | SlideIn _ -> 3uy
    | Dissolve _ -> 4uy | Grow _ -> 5uy | Sequence _ -> 6uy | Custom _ -> 7uy

  let packCanvasMode (cm: CanvasMode) =
    match cm with
    | Braille -> 0uy | HalfBlock -> 1uy | PixelProtocol -> 2uy

  let mkNode kind stylePacked attrsPacked ck cv fc ns ds dl =
    { Kind = kind; StylePacked = stylePacked; AttrsPacked = attrsPacked
      ConstraintKind = ck; ConstraintVal = cv
      FirstChild = fc; NextSibling = ns
      DataStart = ds; DataLen = dl }

  let rec lower (arena: FrameArena) (elem: Element) : NodeHandle =
    match elem with
    | Empty ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      arena.Nodes.[idx] <- mkNode 0uy 0UL 0us 0uy 0s -1 -1 0 0
      h
    | Text(text, style) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (start, len) = FrameArena.allocText text arena
      arena.Nodes.[idx] <-
        mkNode 1uy (packStylePair style) style.Attrs.Value 0uy 0s -1 -1 start len
      h
    | Row children ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let fc = lowerChildren arena children
      arena.Nodes.[idx] <- mkNode 2uy 0UL 0us 0uy 0s fc -1 0 0
      h
    | Column children ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let fc = lowerChildren arena children
      arena.Nodes.[idx] <- mkNode 3uy 0UL 0us 0uy 0s fc -1 0 0
      h
    | Overlay layers ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let fc = lowerChildren arena layers
      arena.Nodes.[idx] <- mkNode 4uy 0UL 0us 0uy 0s fc -1 0 0
      h
    | Styled(style, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      arena.Nodes.[idx] <-
        mkNode 5uy (packStylePair style) style.Attrs.Value 0uy 0s ci -1 0 0
      h
    | Constrained(c, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      let (ck, cv) = packConstraint c
      arena.Nodes.[idx] <- mkNode 6uy 0UL 0us ck cv ci -1 0 0
      h
    | Bordered(bs, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      arena.Nodes.[idx] <- mkNode 7uy 0UL 0us (packBorderStyle bs) 0s ci -1 0 0
      h
    | Padded(pad, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      let (ds, dl) = packPadding pad
      arena.Nodes.[idx] <- mkNode 8uy 0UL 0us 0uy 0s ci -1 ds dl
      h
    | Keyed(key, enter, exit, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      let (start, len) = FrameArena.allocText key arena
      arena.Nodes.[idx] <-
        mkNode 9uy 0UL 0us (packTransition enter) (int16 (packTransition exit)) ci -1 start len
      h
    | Canvas config ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      arena.Nodes.[idx] <-
        mkNode 10uy 0UL 0us (packCanvasMode config.Mode) 0s -1 -1 0 0
      h
    | Aligned(hAlign, vAlign, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      let hb = match hAlign with HAlign.Left -> 0uy | HAlign.HCenter -> 1uy | HAlign.Right -> 2uy
      let vb = match vAlign with VAlign.Top -> 0uy | VAlign.VCenter -> 1uy | VAlign.Bottom -> 2uy
      arena.Nodes.[idx] <- mkNode 11uy 0UL 0us hb (int16 vb) ci -1 0 0
      h
    | Gapped(gap, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      arena.Nodes.[idx] <- mkNode 12uy 0UL 0us 0uy (int16 gap) ci -1 0 0
      h

  and lowerChildren (arena: FrameArena) (children: Element list) : int =
    match children with
    | [] -> -1
    | first :: rest ->
      let (NodeHandle firstIdx) = lower arena first
      let mutable prev = firstIdx
      for child in rest do
        let (NodeHandle childIdx) = lower arena child
        arena.Nodes.[prev] <- { arena.Nodes.[prev] with NextSibling = childIdx }
        prev <- childIdx
      firstIdx
