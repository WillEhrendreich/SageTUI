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

/// Arena hit-test entry. Key is stored as a (DataStart, DataLen) pair to avoid
/// per-frame string allocation — materialise via System.String only when needed.
[<Struct>]
type HitEntry = { X: int; Y: int; Width: int; Height: int; KeyStart: int; KeyLen: int }

type FrameArena =
  { Nodes: ElementNode array
    mutable NodeCount: int
    TextBuf: char array
    mutable TextPos: int
    LayoutScratch: int array
    mutable LayoutPos: int
    mutable Generation: int
    CanvasDraws: System.Collections.Generic.List<CanvasConfig>
    HitMap: System.Collections.Generic.List<HitEntry>
    mutable PeakNodes: int
    mutable PeakChars: int
    mutable PeakLayout: int }

type ArenaStats =
  { NodeCapacity: int; NodeCount: int; PeakNodes: int
    CharCapacity: int; CharPos: int; PeakChars: int
    LayoutCapacity: int; LayoutPos: int; PeakLayout: int }

module FrameArena =
  let create maxNodes maxChars maxLayoutScratch =
    { Nodes = Array.zeroCreate maxNodes
      NodeCount = 0
      TextBuf = Array.zeroCreate maxChars
      TextPos = 0
      LayoutScratch = Array.zeroCreate maxLayoutScratch
      LayoutPos = 0
      Generation = 0
      // Pre-allocate for typical expected counts to avoid startup doubling.
      // List<T>.Clear() retains capacity — these are one-time startup costs, not per-frame.
      CanvasDraws = System.Collections.Generic.List<CanvasConfig>(8)
      HitMap = System.Collections.Generic.List<HitEntry>(32)
      PeakNodes = 0
      PeakChars = 0
      PeakLayout = 0 }

  let stats (arena: FrameArena) : ArenaStats =
    { NodeCapacity = arena.Nodes.Length; NodeCount = arena.NodeCount; PeakNodes = arena.PeakNodes
      CharCapacity = arena.TextBuf.Length; CharPos = arena.TextPos; PeakChars = arena.PeakChars
      LayoutCapacity = arena.LayoutScratch.Length; LayoutPos = arena.LayoutPos; PeakLayout = arena.PeakLayout }

  let reset (arena: FrameArena) =
    arena.PeakNodes <- max arena.PeakNodes arena.NodeCount
    arena.PeakChars <- max arena.PeakChars arena.TextPos
    arena.PeakLayout <- max arena.PeakLayout arena.LayoutPos
    arena.NodeCount <- 0
    arena.TextPos <- 0
    arena.LayoutPos <- 0
    arena.Generation <- arena.Generation + 1
    arena.CanvasDraws.Clear()
    arena.HitMap.Clear()

  let allocNode (arena: FrameArena) =
    if arena.NodeCount >= arena.Nodes.Length then
      failwith (sprintf "FrameArena overflow: %d nodes exceeds capacity %d"
        arena.NodeCount arena.Nodes.Length)
    let idx = arena.NodeCount
    arena.NodeCount <- arena.NodeCount + 1
    NodeHandle idx

  let allocText (text: string) arena =
    if arena.TextPos + text.Length > arena.TextBuf.Length then
      failwith (sprintf
        "FrameArena text overflow: tried to write %d chars at position %d but capacity is %d. \
         Increase AppConfig.ArenaChars (current: %d) in your AppConfig."
        text.Length arena.TextPos arena.TextBuf.Length arena.TextBuf.Length)
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
    | Fill w -> 4uy, int16 w
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
    | Bordered(bs, title, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      let (ds, dl) =
        match title with
        | None | Some "" -> (0, 0)
        | Some t -> FrameArena.allocText t arena
      arena.Nodes.[idx] <- mkNode 7uy 0UL 0us (packBorderStyle bs) 0s ci -1 ds dl
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
      let drawIdx = arena.CanvasDraws.Count
      arena.CanvasDraws.Add(config)
      arena.Nodes.[idx] <-
        mkNode 10uy 0UL 0us (packCanvasMode config.Mode) 0s -1 -1 drawIdx 0
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
    | Responsive breakpoints ->
      // Kind 13: each child in linked list is a "breakpoint wrapper" node (Kind 14)
      // where ConstraintVal = minWidth and FirstChild = the actual element node.
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let fc =
        match breakpoints with
        | [] -> -1
        | _ ->
          let wrappers =
            breakpoints |> List.map (fun (minW, child) ->
              let wh = FrameArena.allocNode arena
              let (NodeHandle wi) = wh
              let (NodeHandle ci) = lower arena child
              arena.Nodes.[wi] <- mkNode 14uy 0UL 0us 0uy (int16 minW) ci -1 0 0
              wi)
          // Link wrappers as siblings
          let first = wrappers.[0]
          wrappers |> List.pairwise |> List.iter (fun (a, b) ->
            arena.Nodes.[a] <- { arena.Nodes.[a] with NextSibling = b })
          first
      arena.Nodes.[idx] <- mkNode 13uy 0UL 0us 0uy 0s fc -1 0 0
      h
    | ResponsiveH breakpoints ->
      // Kind 15: same as Kind 13 but selects by height. Breakpoint wrappers are Kind 16.
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let fc =
        match breakpoints with
        | [] -> -1
        | _ ->
          let wrappers =
            breakpoints |> List.map (fun (minH, child) ->
              let wh = FrameArena.allocNode arena
              let (NodeHandle wi) = wh
              let (NodeHandle ci) = lower arena child
              arena.Nodes.[wi] <- mkNode 16uy 0UL 0us 0uy (int16 minH) ci -1 0 0
              wi)
          let first = wrappers.[0]
          wrappers |> List.pairwise |> List.iter (fun (a, b) ->
            arena.Nodes.[a] <- { arena.Nodes.[a] with NextSibling = b })
          first
      arena.Nodes.[idx] <- mkNode 15uy 0UL 0us 0uy 0s fc -1 0 0
      h
    | Scroll(offset, child) ->
      let h = FrameArena.allocNode arena
      let (NodeHandle idx) = h
      let (NodeHandle ci) = lower arena child
      // DataStart encodes the scroll offset; DataLen unused (0)
      arena.Nodes.[idx] <- mkNode 17uy 0UL 0us 0uy 0s ci -1 offset 0
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
