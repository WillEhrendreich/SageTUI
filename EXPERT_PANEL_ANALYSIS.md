# SageTUI F# TUI Library - Deep Architecture Analysis
## For Expert Panel Review

Generated: 2026-03-09 22:48:48

---

## COMPILATION ORDER (SageTUI.Library.fsproj)

The F# compilation order strictly determines visibility:

1. **Color.fs** - Color DU, TextAttrs, Style, PackedColor codec
2. **Buffer.fs** - PackedCell struct, SIMD diff algorithm
3. **Layout.fs** - Flexbox-like constraint solver  
4. **BorderRender.fs** - ASCII/box drawing borders
5. **Element.fs** - Core Element DU + El module constructors
6. **Measure.fs** - Intrinsic content sizing (min-content)
7. **CanvasRender.fs** - Braille/HalfBlock pixel rendering
8. **Arena.fs** - Element→ElementNode serialization
9. **Effects.fs** - Easing, OKLCH, Braille utilities
10. **Terminal.fs** - ANSI terminal backend
11. **Ansi.fs** - ANSI escape sequences
12. **Render.fs** - Reference recursive renderer
13. **ArenaRender.fs** - Zero-allocation arena renderer
14. **Input.fs** - Keyboard/mouse parsing
15. **Tea.fs** - Elm Architecture (Program, Cmd, Sub)
16. **Detect.fs** - Terminal capability detection
17. **Transition.fs** - Transition effects, reconciliation
18. **App.fs** - Application loop, runtime orchestrator
19. **Recording.fs** - Session recording/replay
20. **Widgets.fs** - High-level widgets
21. **Scroll.fs** - Scrolling support
22. **Testing.fs** - TestHarness framework

---

## PART 1: BUFFER.FS - SIMD Diff & PackedCell

### PackedCell Structure (16 bytes)

\\\sharp
[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type PackedCell =
  { Rune: int32          // Unicode code point
    Fg: int32            // Packed color (type + data)
    Bg: int32            // Packed color
    Attrs: uint16        // Attributes (bold, dim, italic, underline, blink, reverse, hidden, strikethrough)
    _pad: uint16 }       // Padding for Vector128<byte> alignment
\\\

**Layout:** Exactly 16 bytes = Vector128<byte> (SIMD-ready).

### Wide Character Handling

When a cell contains a 2-column character (emoji, wide kanji):
- Column X: actual character + styling
- Column X+1: **sentinel** with Rune=0

\\\sharp
let wideContinuation =
  { Rune = 0; Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }
\\\

The renderer skips Rune=0 cells—they're visually occupied by the left half of the preceding wide character, preventing corruption.

### SIMD Diff: Chunk-Skip Pattern

\\\sharp
let diffInto (changes: ResizeArray<int>) (prev: Buffer) (curr: Buffer) =
  changes.Clear()
  let prevBytes = MemoryMarshal.AsBytes(System.ReadOnlySpan(prev.Cells))
  let currBytes = MemoryMarshal.AsBytes(System.ReadOnlySpan(curr.Cells))
  let cellSz = sizeof<PackedCell>      // 16 bytes
  let chunkCells = 16                   // 16 cells per chunk
  let chunkBytes = chunkCells * cellSz  // 256 bytes
  let totalCells = curr.Cells.Length
  let mutable i = 0

  // Phase 1: Coarse-grained comparison (256 bytes at a time)
  while i + chunkCells <= totalCells do
    let s = i * cellSz
    if not (prevBytes.Slice(s, chunkBytes).SequenceEqual(currBytes.Slice(s, chunkBytes))) then
      // If chunk differs, drill down to individual cells
      for j in i .. i + chunkCells - 1 do
        let bs = j * cellSz
        if not (prevBytes.Slice(bs, cellSz).SequenceEqual(currBytes.Slice(bs, cellSz))) then
          changes.Add(j)
    i <- i + chunkCells

  // Phase 2: Handle remainder (< 16 cells)
  while i < totalCells do
    let bs = i * cellSz
    if not (prevBytes.Slice(bs, cellSz).SequenceEqual(currBytes.Slice(bs, cellSz))) then
      changes.Add(i)
    i <- i + 1
\\\

**How it works:**
1. Cast PackedCell array to byte span (zero-copy via MemoryMarshal)
2. Compare 256-byte chunks with Span.SequenceEqual (JIT-vectorized in .NET 8+)
3. If chunk differs, drill down to individual 16-byte cells
4. Return indices of changed cells in a pre-allocated ResizeArray

**Performance:** O(n) worst case, but skips large identical regions via chunk skipping.

### Buffer Layout

\\\sharp
type Buffer = { Cells: PackedCell array; Width: int; Height: int }

// Row-major 1D indexing
let get x y buf = buf.Cells[y * buf.Width + x]
let set x y cell buf = buf.Cells[y * buf.Width + x] <- cell
\\\

Two text-write paths:
- **writeString** (Render.fs): Legacy allocating path (creates StringBuilder)
- **writeCharSpan** (ArenaRender.fs): Zero-allocation path (spans directly)

---

## PART 2: COLOR.FS - Color Domain & Packing

### Color DU (4 variants)

\\\sharp
type Color =
  | Default                          // Terminal default
  | Named of BaseColor * Intensity   // 8 colors × 2 intensities
  | Ansi256 of byte                  // 256-color palette
  | Rgb of r: byte * g: byte * b: byte  // Truecolor
\\\

### PackedColor Encoding (int32)

Encodes entire color into 32 bits with tag prefix:

\\\sharp
let pack (c: Color) : int =
  match c with
  | Default -> 0  // Tag 0 (0 bytes used)

  | Named(bc, i) ->
    let bcIdx = match bc with Black->0 | Red->1 | Green->2 | Yellow->3 | Blue->4 | Magenta->5 | Cyan->6 | White->7
    let iIdx = match i with Normal->0 | Bright->1
    1 ||| (bcIdx <<< 2) ||| (iIdx <<< 5)  // Tag 1, bits [2:4] = base, bit 5 = intensity

  | Ansi256 b -> 2 ||| (int b <<< 8)      // Tag 2, bits [8:15] = byte

  | Rgb(r, g, b) ->
    3 ||| (int r <<< 8) ||| (int g <<< 16) ||| (int b <<< 24)  // Tag 3, bits [8:31] = RRGGBB
\\\

**Bit layout (little-endian int32):**
- Bits [0:1]: Type tag
- Named: bits [2:4] = base color (0-7), bit 5 = intensity (0-1)
- Ansi256: bits [8:15] = palette index
- RGB: bits [8:31] = RRGGBB

### Unpacking

\\\sharp
let unpack (packed: int) : Color =
  let tag = packed &&& 0x3
  match tag with
  | 0 -> Default
  | 1 ->
    let bcIdx = (packed >>> 2) &&& 0x7
    let iIdx = (packed >>> 5) &&& 0x1
    let bc = [| Black; Red; Green; Yellow; Blue; Magenta; Cyan; White |].[bcIdx]
    let i = if iIdx = 0 then Normal else Bright
    Named(bc, i)
  | 2 -> Ansi256 (byte ((packed >>> 8) &&& 0xFF))
  | _ -> Rgb(byte ((packed >>> 8) &&& 0xFF), byte ((packed >>> 16) &&& 0xFF), byte ((packed >>> 24) &&& 0xFF))
\\\

---

## PART 3: ELEMENT.FS - Core Element DU (Complete)

\\\sharp
[<NoEquality; NoComparison>]
type Element =
  | Empty                                    // Renders nothing
  | Text of string * Style                   // Leaf text node
  | Row of Element list                      // Horizontal layout
  | Column of Element list                   // Vertical layout
  | Overlay of Element list                  // Layered (last = frontmost)
  | Styled of Style * Element                // Apply style to subtree
  | Constrained of Constraint * Element      // Size constraint wrapper
  | Bordered of BorderStyle * string option * Element  // Box + optional title
  | Padded of Padding * Element              // Inset
  | Keyed of key: string * enter: Transition * exit: Transition * Element  // Transition tracking
  | Canvas of CanvasConfig                   // Pixel-to-cell rendering
  | Aligned of HAlign * VAlign * Element     // Alignment within area
  | Gapped of int * Element                  // Gap between row/column children
  | Responsive of (int * Element) list       // Width breakpoints (minW, elem)
  | ResponsiveH of (int * Element) list      // Height breakpoints (minH, elem)
  | Scroll of offset: int * Element          // Vertical viewport
  | Filled of Style                          // Fill area with background
\\\

**Keyed element note:**
- Key used for reconciliation (enter/exit transitions fire based on presence in tree)
- Enter transition: when element appears in tree
- Exit transition: when element leaves tree
- Handled by Reconcile module in Transition.fs

---

## PART 4: LAYOUT.FS - The Constraint Solver

### Constraint DU

\\\sharp
type Constraint =
  | Fixed of int               // Exact size
  | Min of int                 // Minimum (grows to fill)
  | Max of int                 // Maximum (clipped)
  | Percentage of int          // % of available
  | Fill of weight: int        // Proportional flex
  | Ratio of int * int         // Aspect ratio (num/den)
\\\

### Layout.solve Algorithm (2 phases)

**Phase 1: Allocate non-Fill constraints**

\\\sharp
let solve (available: int) (constraints: Constraint list) : (int * int) list =
  let n = List.length constraints
  let sizes = Array.create n 0
  let mutable remaining = available
  let hasFill = constraints |> List.exists (function Fill _ -> true | _ -> false)
  let mutable fillWeightTotal = 0
  let mutable minCount = 0

  // Process non-Fill constraints first
  constraints |> List.iteri (fun i c ->
    match c with
    | Fixed size ->
      sizes[i] <- min size remaining
      remaining <- remaining - sizes[i]
    
    | Percentage pct ->
      sizes[i] <- available * pct / 100
      remaining <- remaining - sizes[i]
    
    | Ratio(num, den) when den > 0 ->
      sizes[i] <- available * num / den
      remaining <- remaining - sizes[i]
    
    | Min minSize ->
      sizes[i] <- min minSize remaining
      remaining <- remaining - sizes[i]
      if not hasFill then minCount <- minCount + 1  // Will grow to fill if no Fill items
    
    | Max maxSize ->
      sizes[i] <- min maxSize remaining
      remaining <- remaining - sizes[i]
    
    | Fill w ->
      fillWeightTotal <- fillWeightTotal + w  // Deferred to phase 2
    
    | _ -> ())

  // Phase 2: Distribute remaining space to Fill items
  match hasFill with
  | true ->
    // Hamilton's method: sequential subtraction
    let mutable pool = remaining
    let mutable wLeft = fillWeightTotal
    constraints |> List.iteri (fun i c ->
      match c with
      | Fill w when wLeft > 0 ->
        let alloc = pool * w / wLeft
        sizes[i] <- alloc
        pool <- pool - alloc
        wLeft <- wLeft - w
      | Fill _ ->
        sizes[i] <- 0  // Last item absorbs remainder
      | _ -> ())
  
  | false ->
    // No Fill: let Min items grow equally
    match minCount > 0 with
    | true ->
      let perMin = max 0 (remaining / minCount)
      let mutable extra = max 0 (remaining % minCount)
      constraints |> List.iteri (fun i c ->
        match c with
        | Min _ ->
          let bonus = if extra > 0 then extra <- extra - 1; 1 else 0
          sizes[i] <- sizes[i] + perMin + bonus
        | _ -> ())
    | false -> ()

  // Compute (offset, size) pairs
  let mutable offset = 0
  [ for i in 0 .. n - 1 do
      yield (offset, sizes[i])
      offset <- offset + sizes[i] ]
\\\

### Layout.solveWithContent - Content-Aware Flex

For children with known content size:

\\\sharp
let solveWithContent (available: int) (constraints: Constraint list) (contentSizes: int list) : (int * int) list =
  // Phase 1: Allocate non-Fill (same as solve)
  // ...
  
  // Phase 2: Fill items get content as basis, then flex-grow on excess
  match hasFill with
  | true ->
    match fillContentTotal <= remaining with
    | true ->
      // Enough space: each Fill gets content + weighted share of excess
      let excess = remaining - fillContentTotal
      let mutable pool = excess
      let mutable wLeft = fillWeightTotal
      constraints |> List.iteri (fun i c ->
        match c with
        | Fill w when wLeft > 0 ->
          let bonus = pool * w / wLeft
          sizes[i] <- contentArr[i] + bonus  // content + flex-grow
          pool <- pool - bonus
          wLeft <- wLeft - w
        | Fill _ ->
          sizes[i] <- contentArr[i]
        | _ -> ())
    
    | false ->
      // Not enough space: proportional shrink by content size
      let pool = max 0 remaining
      let fillItems = constraints |> List.mapi (fun i c -> match c with Fill _ -> Some(i, contentArr[i]) | _ -> None) |> List.choose id
      let totalContent = fillItems |> List.sumBy snd
      match totalContent with
      | 0 ->
        // No content: distribute pool equally by weight
        let mutable pool = remaining
        let mutable wLeft = fillWeightTotal
        constraints |> List.iteri (fun i c ->
          match c with
          | Fill w when wLeft > 0 ->
            let alloc = pool * w / wLeft
            sizes[i] <- alloc
            pool <- pool - alloc
            wLeft <- wLeft - w
          | Fill _ -> ()
          | _ -> ())
      
      | _ ->
        // Proportional shrink: each Fill item gets pool * content / totalContent
        let sized = fillItems |> List.map (fun (i, c) -> (i, c * pool / totalContent))
        let used = sized |> List.sumBy snd
        let mutable rem = pool - used
        sized |> List.iter (fun (i, s) ->
          let bonus = if rem > 0 then rem <- rem - 1; 1 else 0
          sizes[i] <- sizes[i] + s + bonus)
  
  | false -> ()  // No Fill items
  
  // Compute (offset, size) pairs
  let mutable offset = 0
  [ for i in 0 .. n - 1 do
      yield (offset, sizes[i])
      offset <- offset + sizes[i] ]
\\\

### Gap Support

\\\sharp
let solveWithGap (gap: int) (available: int) (constraints: Constraint list) (contentSizes: int list) : (int * int) list =
  let n = List.length constraints
  match n <= 1 with
  | true -> solveWithContent available constraints contentSizes
  | false ->
    let totalGap = gap * (n - 1)
    let usable = max 0 (available - totalGap)
    let solved = solveWithContent usable constraints contentSizes
    solved |> List.mapi (fun i (offset, size) ->
      (offset + i * gap, size))  // Inject gap into offsets
\\\

**Key insight:** Gap is added as OFFSET adjustments after layout solving, not by reducing available space.

---

## PART 5: CANVASRENDER.FS - Braille & HalfBlock

### HalfBlock Mode (2 pixels → 1 terminal cell)

\\\sharp
let halfBlockChar = '▀'        // U+2580 (upper half block)
let lowerHalfBlockChar = '▄'   // U+2584 (lower half block)

let halfBlockCell (pb: PixelBuffer) (col: int) (row: int) =
  let topColor = getPixel pb col (row * 2)       // Pixel at y=row*2
  let botColor = getPixel pb col (row * 2 + 1)   // Pixel at y=row*2+1
  match topColor, botColor with
  | Some top, Some bot when top = bot -> (' ', None, Some top)
  | Some top, Some bot -> (halfBlockChar, Some top, Some bot)   // Top=fg, bot=bg
  | Some top, None -> (halfBlockChar, Some top, None)
  | None, Some bot -> (lowerHalfBlockChar, Some bot, None)
  | None, None -> (' ', None, None)
\\\

**Maps:**
- If both pixels same color → solid block
- If different → half-block character with one as foreground, one as background

### Braille Mode (2 wide × 4 tall pixels → 1 terminal cell)

\\\sharp
let brailleDotBits = [| 0x01; 0x08; 0x02; 0x10; 0x04; 0x20; 0x40; 0x80 |]
                      // Bit patterns for 8 braille positions

let brailleCell (pb: PixelBuffer) (col: int) (row: int) =
  let baseX = col * 2        // 2-pixel wide region
  let baseY = row * 4        // 4-pixel tall region
  let mutable pattern = 0
  let mutable firstColor = None
  
  for dy in 0..3 do          // 4 rows
    for dx in 0..1 do        // 2 cols
      match getPixel pb (baseX + dx) (baseY + dy) with
      | Some c when c <> Color.Default ->
        pattern <- pattern ||| brailleDotBits.[dy * 2 + dx]
        match firstColor with
        | None -> firstColor <- Some c
        | _ -> ()
      | _ -> ()
  
  (char (0x2800 + pattern), firstColor)
\\\

**Braille Unicode:**
- Range: U+2800–U+28FF (256 characters)
- Base: 0x2800 (empty braille ⠀)
- Add pattern bits to form visible characters
- Each pixel in 2×4 region maps to one of 8 braille dots
- Coloring: uses foreground color only (braille dots rendered as fg)

---

## PART 6: TRANSITION.FS - Animations & Reconciliation

### ActiveTransition Type

\\\sharp
type ActiveTransition = {
  Key: string
  Transition: Transition
  StartMs: int64
  DurationMs: int
  Easing: Easing
  SnapshotBefore: PackedCell array  // State before transition
  Area: Area
  Payload: TransitionPayload        // Kind-specific: DissolvePayload or NoPayload
  mutable PhaseCaptures: Map<int, PackedCell array>  // For Sequence transitions
}
\\\

### Built-in Transition Effects

**1. ColorMorph: Interpolate PackedColor by components**

\\\sharp
let lerpPackedColor (t: float) (a: int32) (b: int32) : int32 =
  let tagA = a &&& 0x3
  let tagB = b &&& 0x3
  match tagA, tagB with
  | 3, 3 ->  // Both RGB
    let rA = (a >>> 8) &&& 0xFF
    let gA = (a >>> 16) &&& 0xFF
    let bA = (a >>> 24) &&& 0xFF
    let rB = (b >>> 8) &&& 0xFF
    let gB = (b >>> 16) &&& 0xFF
    let bB = (b >>> 24) &&& 0xFF
    let r = Anim.lerpInt rA rB t
    let g = Anim.lerpInt gA gB t
    let bl = Anim.lerpInt bA bB t
    3 ||| (r <<< 8) ||| (g <<< 16) ||| (bl <<< 24)
  | _ ->
    // For non-RGB, snap at t >= 0.5
    if t >= 0.5 then b else a
\\\

**2. Fade: Interpolate from Default to content color**

\\\sharp
let applyFade (t: float) (current: PackedCell array) (area: Area) (buf: Buffer) =
  let defaultFg = PackedColor.pack Default
  for row in 0 .. area.Height - 1 do
    for col in 0 .. area.Width - 1 do
      let idx = row * area.Width + col
      let cell = current.[idx]
      let faded = {
        cell with
          Fg = lerpPackedColor t defaultFg cell.Fg
          Bg = lerpPackedColor t 0 cell.Bg  // Fade from black
      }
      buf.Cells.[(area.Y + row) * buf.Width + (area.X + col)] <- faded
\\\

**3. Wipe: Reveal from edge based on progress**

\\\sharp
let applyWipe (t: float) (dir: Direction) (snapshot: PackedCell array) (current: PackedCell array) 
              (area: Area) (buf: Buffer) =
  for row in 0 .. area.Height - 1 do
    for col in 0 .. area.Width - 1 do
      let idx = row * area.Width + col
      let revealed =
        match dir with
        | Direction.Right ->
          float (col + 1) / float (max 1 area.Width) <= t  // Left→right reveal
        | Direction.Left ->
          float (area.Width - col) / float (max 1 area.Width) <= t
        | Direction.Down ->
          float (row + 1) / float (max 1 area.Height) <= t  // Top→bottom reveal
        | Direction.Up ->
          float (area.Height - row) / float (max 1 area.Height) <= t
      let cell = if revealed then current.[idx] else snapshot.[idx]
      buf.Cells.[(area.Y + row) * buf.Width + (area.X + col)] <- cell
\\\

**4. SlideIn: New content slides in, snapshot slides out**

\\\sharp
let applySlideIn (t: float) (dir: Direction) (snapshot: PackedCell array) (current: PackedCell array) 
                 (area: Area) (buf: Buffer) =
  for row in 0 .. area.Height - 1 do
    for col in 0 .. area.Width - 1 do
      let idx = row * area.Width + col
      let cell =
        match dir with
        | Direction.Right ->
          let shift = int (float area.Width * (1.0 - t))
          if col < shift then snapshot.[idx]
          else
            let srcIdx = row * area.Width + (col - shift)
            if srcIdx < current.Length then current.[srcIdx] else snapshot.[idx]
        
        | Direction.Left ->
          let visible = int (float area.Width * t)
          let shift = area.Width - visible
          if col >= visible then snapshot.[idx]
          else
            let srcIdx = row * area.Width + (col + shift)
            if srcIdx < current.Length then current.[srcIdx] else snapshot.[idx]
        
        | Direction.Down ->
          let shift = int (float area.Height * (1.0 - t))
          if row < shift then snapshot.[idx]
          else
            let srcIdx = (row - shift) * area.Width + col
            if srcIdx < current.Length then current.[srcIdx] else snapshot.[idx]
        
        | Direction.Up ->
          let visible = int (float area.Height * t)
          let shift = area.Height - visible
          if row >= visible then snapshot.[idx]
          else
            let srcIdx = (row + shift) * area.Width + col
            if srcIdx < current.Length then current.[srcIdx] else snapshot.[idx]
      
      buf.Cells.[(area.Y + row) * buf.Width + (area.X + col)] <- cell
\\\

**5. Dissolve: Reveal cells in random order**

\\\sharp
// Pre-computed at transition start (captured in DissolvePayload)
let fisherYatesShuffle (seed: int) (n: int) =
  let rng = System.Random(seed)
  let arr: int array = Array.init n id
  for i in n - 1 .. -1 .. 1 do
    let j = rng.Next(i + 1)
    let tmp = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- tmp
  arr

let applyDissolve (t: float) (shuffleOrder: int array) (snapshot: PackedCell array) 
                  (current: PackedCell array) (area: Area) (buf: Buffer) =
  let revealCount = int (float shuffleOrder.Length * t)
  for row in 0 .. area.Height - 1 do
    for col in 0 .. area.Width - 1 do
      let idx = row * area.Width + col
      let revealed = shuffleOrder.[idx] < revealCount
      let cell = if revealed then current.[idx] else snapshot.[idx]
      buf.Cells.[(area.Y + row) * buf.Width + (area.X + col)] <- cell
\\\

**6. Grow: Expand from center via Chebyshev distance**

\\\sharp
let applyGrow (t: float) (snapshot: PackedCell array) (current: PackedCell array) 
              (area: Area) (buf: Buffer) =
  let cx = float area.Width * 0.5
  let cy = float area.Height * 0.5
  let maxR = max cx cy + 0.5  // Guarantees corners covered at t=1
  let r = maxR * t
  
  for row in 0 .. area.Height - 1 do
    for col in 0 .. area.Width - 1 do
      let idx = row * area.Width + col
      let dx = abs (float col + 0.5 - cx)
      let dy = abs (float row + 0.5 - cy)
      let cell = if max dx dy <= r then current.[idx] else snapshot.[idx]
      buf.Cells.[(area.Y + row) * buf.Width + (area.X + col)] <- cell
\\\

### Sequence Transitions: Multi-phase support

\\\sharp
let phaseAt (elapsedMs: float) (ts: Transition list) : int * float =
  match ts with
  | [] -> (0, 1.0)
  | _ ->
    let lastIdx = List.length ts - 1
    let totalMs = ts |> List.sumBy (TransitionDuration.get >> float)
    match totalMs with
    | 0.0 -> (lastIdx, 1.0)
    | _ ->
      let mutable remaining = elapsedMs
      let mutable result = (lastIdx, 1.0)
      let mutable found = false
      let mutable idx = 0
      for sub in ts do
        if not found then
          let dur = float (TransitionDuration.get sub)
          if remaining <= dur || idx = lastIdx then
            let localT = if dur = 0.0 then 1.0 else System.Math.Clamp(remaining / dur, 0.0, 1.0)
            result <- (idx, localT)
            found <- true
          remaining <- remaining - dur
          idx <- idx + 1
      result
\\\

---

## PART 7: RENDER.FS vs ARENARENDER.FS

### RENDER.FS: Reference Implementation

**Characteristics:**
- Directly matches Element DU structure
- Recursive tree traversal
- Allocates intermediate strings (writeString)
- Measures child content on-demand

**Example: Row rendering**

\\\sharp
| Row children ->
  let constraints = children |> List.map extractConstraint
  let unwrapped = children |> List.map unwrapConstrained
  let contentWidths = Measure.childWidths children  // Allocates list
  let areas = Layout.splitHWithContent constraints contentWidths area
  List.iter2 (fun childArea child ->
    render (Layout.intersectArea area childArea) inheritedStyle buf child) areas unwrapped
\\\

**Pros:** Clear semantics, easy to understand
**Cons:** Multiple allocations per frame (lists, tuples, measured sizes)

### ARENARENDER.FS: Zero-Allocation Renderer

**Characteristics:**
- Element tree pre-serialized to FrameArena (ElementNode array + TextBuf)
- Linked-list traversal via sibling pointers
- Inline layout solving using scratch array
- Direct text reads from TextBuf (no intermediate strings)

**Example: Row rendering (Arena version)**

\\\sharp
| 2uy ->  // Row in Arena
  let n = countChildren arena node.FirstChild
  let ls = arena.LayoutScratch
  let lp = arena.LayoutPos
  if lp + n + n + n > ls.Length then
    failwith "LayoutScratch overflow"
  
  let cnBase = lp           // Child node indices
  let szBase = lp + n       // Sizes
  let wzBase = lp + n + n   // Fill weights
  arena.LayoutPos <- lp + n + n + n
  
  let mutable idx = node.FirstChild
  let mutable i = 0
  let mutable remaining = area.Width
  let mutable fillWeightTotal = 0
  let mutable fillContentTotal = 0
  
  // Linked-list traversal
  while idx >= 0 do
    let cn = arena.Nodes.[idx]
    match cn.ConstraintKind with
    | 0uy ->  // Fixed
      ls.[szBase + i] <- min (int cn.ConstraintVal) remaining
      remaining <- remaining - ls.[szBase + i]
    | 4uy ->  // Fill
      let cw = measureWidth arena cn.FirstChild
      ls.[szBase + i] <- -(cw + 1)  // Negative encodes pending Fill
      ls.[wzBase + i] <- int cn.ConstraintVal
      fillWeightTotal <- fillWeightTotal + int cn.ConstraintVal
      fillContentTotal <- fillContentTotal + cw
    | _ -> ()
    idx <- cn.NextSibling
    i <- i + 1
  
  // Inline Fill distribution
  distributeFill ls szBase wzBase n fillWeightTotal fillContentTotal remaining
  
  // Render children
  let mutable offset = 0
  for j in 0 .. n - 1 do
    let sz = ls.[szBase + j]
    let childArea = Layout.intersectArea area { X = area.X + offset; Y = area.Y; Width = sz; Height = area.Height }
    render arena ls.[cnBase + j] childArea inheritedFg inheritedBg inheritedAttrs buf
    offset <- offset + sz
\\\

**Key differences:**
1. **No allocation**: Scratch slots pre-allocated; indices stored inline
2. **Linked-list traversal**: NextSibling instead of list iteration
3. **Text from TextBuf**: No intermediate string, direct char span write
4. **Inline layout**: fillWeightTotal/fillContentTotal computed directly in traversal

**Performance:** 2-3x faster due to zero allocations + cache-friendly layout.

---

## PART 8: EFFECTS.FS - Easing & Color

### Easing Functions

\\\sharp
module Ease =
  let linear: Easing = id
  
  let quadOut: Easing = fun t ->
    1.0 - (1.0 - t) * (1.0 - t)
  
  let quadIn: Easing = fun t -> t * t
  
  let cubicInOut: Easing = fun t ->
    if t < 0.5 then 4.0 * t * t * t
    else 1.0 - pown (-2.0 * t + 2.0) 3 / 2.0
\\\

### Animation Helpers

\\\sharp
module Anim =
  let lerp (a: float) (b: float) (t: float) = a + (b - a) * t
  let lerpInt (a: int) (b: int) (t: float) = int (float a + float (b - a) * t)
  
  let progress (startMs: int64) (nowMs: int64) (durationMs: int) =
    let elapsed = float (nowMs - startMs)
    min 1.0 (max 0.0 (elapsed / float durationMs))
  
  let isDone (startMs: int64) (nowMs: int64) (durationMs: int) =
    nowMs - startMs >= int64 durationMs
\\\

### OKLCH Color Space (Perceptual)

Enables smooth color interpolation via LCH polar coordinates:

\\\sharp
module Oklch =
  let fromRgb (r: byte) (g: byte) (b: byte) =
    // 1. sRGB → Linear RGB (gamma correction)
    let lr = srgbToLinear (float r / 255.0)
    let lg = srgbToLinear (float g / 255.0)
    let lb = srgbToLinear (float b / 255.0)
    
    // 2. Linear RGB → LMS (M1 matrix, Ottosson)
    let lp = 0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb
    let mp = 0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb
    let sp = 0.0883024619 * lr + 0.2024326433 * lg + 0.6892648948 * lb
    
    // 3. LMS cube root
    let l = Math.Cbrt lp
    let m = Math.Cbrt mp
    let s = Math.Cbrt sp
    
    // 4. LMS → OKLab (M2 matrix)
    let capL = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
    let a = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
    let bVal = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
    
    // 5. OKLab → OKLCH (polar)
    let capC = sqrt (a * a + bVal * bVal)
    let capH = Math.Atan2(bVal, a) * (180.0 / Math.PI)
    let capH = if capH < 0.0 then capH + 360.0 else capH
    (capL, capC, capH)

  let toRgb (capL: float) (capC: float) (capH: float) =
    let hRad = capH * (Math.PI / 180.0)
    let a = capC * cos hRad
    let bOklab = capC * sin hRad
    
    // Inverse: OKLab → LMS cube root
    let l = capL + 0.3963377774 * a + 0.2158037573 * bOklab
    let m = capL - 0.1055613458 * a - 0.0638541728 * bOklab
    let s = capL - 0.0894841775 * a - 1.2914855480 * bOklab
    
    // Cube (inverse of cube root)
    let lp = l * l * l
    let mp = m * m * m
    let sp = s * s * s
    
    // Inverse M1: LMS → linear sRGB
    let r =  4.0562053658179495 * lp - 3.2568173945365637 * mp + 0.2047061514213596 * sp
    let g = -1.2380901746958464 * lp + 2.5345477129513907 * mp - 0.3025076918388671 * sp
    let bVal = -0.1560257222986546 * lp - 0.3271459215572362 * mp + 1.5134404528807921 * sp
    
    // Linear RGB → sRGB (gamma)
    (linearToByte r, linearToByte g, linearToByte bVal)

  let lerp (l1, c1, h1) (l2, c2, h2) (t: float) =
    let capL = l1 + (l2 - l1) * t
    let capC = c1 + (c2 - c1) * t
    let dh = h2 - h1
    let dh =
      if dh > 180.0 then dh - 360.0
      elif dh < -180.0 then dh + 360.0
      else dh
    let capH = (h1 + dh * t) % 360.0
    let capH = if capH < 0.0 then capH + 360.0 else capH
    (capL, capC, capH)
\\\

---

## PART 9: ARENA.FS - Serialization

### ElementNode Flat Structure

\\\sharp
[<Struct>]
type ElementNode =
  { Kind: byte                 // 0=Empty, 1=Text, 2=Row, ..., 18=Filled
    StylePacked: uint64        // (FG << 32) | BG
    AttrsPacked: uint16        // Attributes bitmask
    ConstraintKind: byte       // 0=Fixed, 1=Min, 2=Max, 3=%, 4=Fill, 5=Ratio
    ConstraintVal: int16       // Encoded constraint
    FirstChild: int            // Child index (-1 = none)
    NextSibling: int           // Sibling index (-1 = none)
    DataStart: int             // Offset in TextBuf or encoded value
    DataLen: int               // Length in TextBuf
  }
\\\

### Lowering: Element → Arena

\\\sharp
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

  // ... (similar for Column, Overlay, Styled, Constrained, etc.)

  | Keyed(key, enter, exit, child) ->
    let h = FrameArena.allocNode arena
    let (NodeHandle idx) = h
    let (NodeHandle ci) = lower arena child
    let (start, len) = FrameArena.allocText key arena
    arena.Nodes.[idx] <-
      mkNode 9uy 0UL 0us (packTransition enter) (int16 (packTransition exit)) ci -1 start len
    h

and lowerChildren (arena: FrameArena) (children: Element list) : int =
  match children with
  | [] -> -1
  | first :: rest ->
    let (NodeHandle firstIdx) = lower arena first
    let mutable prev = firstIdx
    for child in rest do
      let (NodeHandle nextIdx) = lower arena child
      arena.Nodes.[prev] <- { arena.Nodes.[prev] with NextSibling = nextIdx }
      prev <- nextIdx
    firstIdx
\\\

**Result:**
- Nodes linked via FirstChild/NextSibling (singly-linked tree)
- Text stored once in TextBuf (copy during lowering)
- Canvas configs stored in separate List<CanvasConfig>

---

## PART 10: TESTING.FS - Deterministic Testing

### TestApp<'model, 'msg>

\\\sharp
type TestApp<'model, 'msg> = {
  Model: 'model
  Program: Program<'model, 'msg>
  Width: int; Height: int
  HasQuit: bool
  ExitCode: int option
  VirtualTime: TimeSpan                    // Advances with advanceTime
  TimerNextFire: Map<string, TimeSpan>
  PendingDelays: PendingDelay<'msg> list   // Captured Delay commands
  DelaySeq: int                            // Monotonic sequence
}

type PendingDelay<'msg> = {
  FireAt: TimeSpan
  Seq: int                                 // Tie-breaker for same FireAt times
  Message: 'msg
}
\\\

### Testing Workflow Example

\\\sharp
let app =
  TestHarness.init 80 24 myProgram
  |> TestHarness.pressKey (Key.Char (Rune 'j'))
  |> TestHarness.pressKey (Key.Char (Rune 'k'))
  |> TestHarness.advanceTime (TimeSpan.FromMilliseconds 500.0)

// Assert model state
app.Model |> Expect.equal "count should be 2" 2

// Assert rendered output
TestHarness.render app |> Expect.stringContains "should show count" "Count: 2"
\\\

**Key features:**
- No real terminal required
- Virtual time tracking
- Delay commands captured in PendingDelays
- Timer subscriptions tracked per ID
- Async commands ignored (test with Cmd.hasAsync)

---

## ARCHITECTURE SUMMARY

### Data Flow (Elm Pattern)

\\\
User Input (keyboard, mouse)
    ↓
Event Parsing (Input.fs) → Event
    ↓
Subscribe (program.Subscribe) → Sub<'msg>
    ↓ [dispatched message]
Update (program.Update) → (model', cmd)
    ↓
Command Processing (Tea.fs, App.fs)
    ↓
View (program.View) → Element tree
    ↓
Arena Lowering (Arena.fs) → FrameArena
    ↓
Measure + Layout (Measure.fs, Layout.fs) → Areas
    ↓
Render (ArenaRender.fs) → Buffer
    ↓
Transitions Apply (Transition.fs) → Buffer modified
    ↓
SIMD Diff (Buffer.fs) → Changed cell indices
    ↓
Terminal Output (Terminal.fs)
\\\

### Performance Characteristics

1. **No allocations in hot path:** ArenaRender reuses scratch; TextBuf pre-allocated
2. **SIMD diff:** Only changed cells send to terminal; 256-byte chunks skipped
3. **Packed cells:** 16-byte alignment for Vector128<byte> comparisons
4. **Content-aware layout:** Fill children respect intrinsic size
5. **Deterministic testing:** Virtual time + captured Delay commands

### Immutability & Mutability Trade-offs

**Immutable (functional):**
- Element DU (all constructors return new trees)
- Cmd, Style, messages, constraints
- PackedCell (struct value semantics)

**Mutable (performance):**
- Buffer.Cells (write-once per frame, then diff)
- FrameArena scratch arrays (reused per frame)
- ActiveTransition.PhaseCaptures (lazy computed)

---

