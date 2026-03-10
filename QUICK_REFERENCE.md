# SageTUI Quick Reference - Code Flow & Key Structures

---

## Complete Element DU (Element.fs)

\\\
Empty
Text (string, Style)
Row (Element list)
Column (Element list)
Overlay (Element list)
Styled (Style, Element)
Constrained (Constraint, Element)
Bordered (BorderStyle, string option, Element)
Padded (Padding, Element)
Keyed (key, enter_transition, exit_transition, Element)
Canvas (CanvasConfig)
Aligned (HAlign, VAlign, Element)
Gapped (gap_px, Element)
Responsive ((minWidth, Element) list)
ResponsiveH ((minHeight, Element) list)
Scroll (offset, Element)
Filled (Style)
\\\

---

## Complete Constraint DU (Layout.fs)

\\\
Fixed (size)             -- Exact: min(size, available)
Min (size)               -- Floor: grows when surplus exists
Max (size)               -- Ceiling: clipped
Percentage (pct)         -- % of available
Fill (weight)            -- Proportional (default weight=1)
Ratio (num, den)         -- num/den of available
\\\

---

## Transition DU (Element.fs)

\\\
Fade (duration)                              -- Color → Default
ColorMorph (duration)                        -- Lerp PackedColor by components
Wipe (Direction, duration)                   -- Reveal from edge (Right|Left|Down|Up)
SlideIn (Direction, duration)                -- New slides in, snapshot slides out
Dissolve (duration)                          -- Random cell reveal (pre-shuffled)
Grow (duration)                              -- Chebyshev distance expand from center
Sequence (Transition list)                   -- Multi-phase (shares snapshot)
Custom (duration, apply_fn)                  -- User-supplied cell-level blending
\\\

---

## Color DU (Color.fs)

\\\
Default                          -- Terminal default color
Named (BaseColor, Intensity)     -- 8 base × 2 intensity (Normal|Bright)
Ansi256 (byte)                   -- 256-color palette
Rgb (r, g, b)                    -- Truecolor (24-bit)
\\\

PackedColor encoding (int32):
\\\
Bits [0:1]     = Type tag (0|1|2|3)
If tag=1: bits [2:4]=base, bit 5=intensity
If tag=2: bits [8:15]=palette byte
If tag=3: bits [8:31]=RRGGBB
\\\

---

## Cmd DU (Tea.fs)

\\\
NoCmd
Batch (Cmd list)
OfAsync ((msg -> unit) -> Async<unit>)
OfCancellableAsync (id, CancellationToken -> (msg -> unit) -> Async<unit>)
CancelSub (id)
Delay (milliseconds, msg)                   -- Fire after delay
DirectMsg (msg)                             -- Dispatch synchronously (same frame)
Quit (exitCode)
TerminalOutput (string)                     -- Raw ANSI sequence
\\\

---

## PackedCell Struct (Buffer.fs)

\\\sharp
[<Struct; StructLayout(Sequential, Pack=1)>]
type PackedCell = {
  Rune: int32          // Unicode code point
  Fg: int32            // PackedColor
  Bg: int32            // PackedColor
  Attrs: uint16        // bold|dim|italic|underline|blink|reverse|hidden|strikethrough
  _pad: uint16         // Padding for Vector128<byte> alignment
}
// Size: 16 bytes = Vector128<byte>
\\\

---

## ElementNode Struct (Arena.fs)

\\\sharp
[<Struct>]
type ElementNode = {
  Kind: byte           // 0=Empty, 1=Text, 2=Row, 3=Column, ..., 18=Filled
  StylePacked: uint64  // (FG << 32) | BG
  AttrsPacked: uint16  // Attributes bitmask
  ConstraintKind: byte // 0=Fixed, 1=Min, 2=Max, 3=%, 4=Fill, 5=Ratio
  ConstraintVal: int16 // Encoded constraint value
  FirstChild: int      // Index of first child (-1 = none)
  NextSibling: int     // Index of next sibling (-1 = none)
  DataStart: int       // Offset in TextBuf or encoded value
  DataLen: int         // Length in TextBuf
}
// Size: 28 bytes (cache-friendly)
\\\

---

## Layout Constraint Resolution (Layout.fs)

### Phase 1: Non-Fill Constraints
\\\
For each constraint:
  Fixed(n)        → sizes[i] = min(n, remaining)
  Min(n)          → sizes[i] = min(n, remaining), mark for growth
  Max(n)          → sizes[i] = min(n, remaining)
  Percentage(pct) → sizes[i] = available * pct / 100
  Fill(w)         → defer to phase 2
  Ratio(n, d)     → sizes[i] = available * n / d
  Update remaining after each allocation
\\\

### Phase 2: Fill Distribution
\\\
If Fill items exist:
  pool = remaining
  For each Fill(w):
    alloc = pool * w / wLeft
    sizes[i] = alloc
    pool -= alloc
    wLeft -= w
Else if Min items exist (and no Fill):
  perMin = remaining / minCount
  Distribute with round-robin bonus (remainder % minCount)
\\\

---

## SIMD Diff Algorithm (Buffer.fs)

\\\sharp
let diffInto (changes: ResizeArray<int>) (prev: Buffer) (curr: Buffer) =
  // Phase 1: 256-byte chunk comparison (skip unchanged regions)
  for chunk in 0 .. (totalCells / 16) - 1 do
    if prev_chunk ≠ curr_chunk then  // Coarse check
      for cell in chunk do
        if prev_cell ≠ curr_cell then  // Fine check
          changes.Add(cell_index)
  
  // Phase 2: Remainder (< 16 cells)
  for i in (complete_chunks * 16) .. totalCells-1 do
    if prev_cell ≠ curr_cell then
      changes.Add(i)
\\\

---

## Rendering Paths Comparison

### Render.fs (Reference Implementation)
\\\sharp
rec render (area: Area) (inheritedStyle: Style) (buf: Buffer) (elem: Element) =
  match elem with
  | Empty -> ()
  | Text(text, style) ->
    writeString area.X area.Y fg bg attrs text buf  // ALLOCATES
  | Row children ->
    let constraints = children |> List.map extractConstraint  // ALLOCATES
    let contentWidths = Measure.childWidths children         // ALLOCATES
    let areas = Layout.splitHWithContent constraints contentWidths area
    List.iter2 (fun area child -> render area inheritedStyle buf child) areas children
  | Column children -> (similar)
  | ... -> (recursive cases)
\\\

**Allocations per frame:** 1+ lists, 1+ tuples, intermediate strings

### ArenaRender.fs (Zero-Allocation)
\\\sharp
rec render (arena: FrameArena) (nodeIdx: int) (area: Area) ... (buf: Buffer) =
  let node = arena.Nodes.[nodeIdx]
  match node.Kind with
  | 1uy ->  // Text
    let span = ReadOnlySpan<char>(arena.TextBuf, node.DataStart, node.DataLen)
    writeCharSpan area.X area.Y fg bg attrs arena.TextBuf ... area.Width buf  // NO ALLOC
  | 2uy ->  // Row
    let n = countChildren arena node.FirstChild
    let ls = arena.LayoutScratch  // REUSED (pre-allocated)
    let cnBase = lp; let szBase = lp + n; let wzBase = lp + n + n
    arena.LayoutPos <- lp + n + n + n
    // Inline constraint processing (no intermediate lists)
    let mutable idx = node.FirstChild
    while idx >= 0 do
      ls.[szBase + i] <- ...
      idx <- arena.Nodes.[idx].NextSibling
    distributeFill ls szBase wzBase n ...
  | 3uy ->  // Column (similar)
\\\

**Allocations per frame:** ZERO (scratch reused, text from TextBuf)

---

## Transition Payloads (Transition.fs)

\\\sharp
type TransitionPayload =
  | DissolvePayload of shuffleOrder: int array
  | NoPayload

type ActiveTransition = {
  Key: string
  Transition: Transition
  StartMs: int64
  DurationMs: int
  Easing: Easing
  SnapshotBefore: PackedCell array    // Captured state
  Area: Area
  Payload: TransitionPayload          // Dissolve pre-computed
  mutable PhaseCaptures: Map<int, PackedCell array>  // Sequence phases
}
\\\

### Pre-computation Strategy
- **Dissolve:** Fisher-Yates shuffle at transition start (stored in payload)
- **Other effects:** No pre-computation (computed per-frame)
- **Sequence:** Phase captures computed lazily (stored in mutable map)

---

## Test Harness Types (Testing.fs)

\\\sharp
type TestApp<'model, 'msg> = {
  Model: 'model
  Program: Program<'model, 'msg>
  Width: int; Height: int
  HasQuit: bool
  ExitCode: int option
  VirtualTime: TimeSpan
  TimerNextFire: Map<string, TimeSpan>
  PendingDelays: PendingDelay<'msg> list  // Captured Cmd.Delay
  DelaySeq: int                           // Monotonic counter
}

type PendingDelay<'msg> = {
  FireAt: TimeSpan      // Absolute fire time
  Seq: int              // Tie-breaker
  Message: 'msg
}
\\\

### Testing Workflow
\\\sharp
let app =
  TestHarness.init 80 24 myProgram
  |> TestHarness.pressKey key1
  |> TestHarness.pressKey key2
  |> TestHarness.advanceTime (TimeSpan.FromSeconds 1.0)

let model = app.Model
let rendered = TestHarness.render app  // String snapshot
\\\

---

## Pixel-to-Cell Rendering (CanvasRender.fs)

### HalfBlock Mode
\\\
2 pixels vertically → 1 terminal row

Pixel (x, y*2)   ─┐
                   ├─→ halfBlockCell → Character + (Fg, Bg)
Pixel (x, y*2+1) ─┘

Characters:
  ' ' (space)          -- Both pixels same color (fill with color)
  '▀' (upper block)    -- Top=fg, bottom=bg
  '▄' (lower block)    -- Bottom=fg, top=bg (when bottom filled, top empty)
\\\

### Braille Mode
\\\
2 pixels wide × 4 pixels tall → 1 terminal cell

┌─┬─┐
│0│1│  dy=0
├─┼─┤
│2│3│  dy=1
├─┼─┤
│4│5│  dy=2
├─┼─┤
│6│7│  dy=3
└─┴─┘

Bit pattern = 0x2800 + (d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7)
Character range: U+2800 (empty) to U+28FF (all 8 dots)
\\\

---

## Braille Dot Bits Mapping (Effects.fs & CanvasRender.fs)

\\\
brailleDotBits = [| 0x01; 0x08; 0x02; 0x10; 0x04; 0x20; 0x40; 0x80 |]
                    d0    d1    d2    d3    d4    d5    d6    d7

Index [dy * 2 + dx]:
  dy=0: [0x01, 0x08]  (top row: left=0x01, right=0x08)
  dy=1: [0x02, 0x10]  (2nd row)
  dy=2: [0x04, 0x20]  (3rd row)
  dy=3: [0x40, 0x80]  (4th row)

Example: Pixel at (0, 0) filled = pattern |= brailleDotBits[0 * 2 + 0] = 0x01
\\\

---

## OKLCH Color Space (Effects.fs)

\\\
sRGB → Linear RGB → LMS → OKLab → OKLCH

Forward (RGB → OKLCH):
  1. sRGB to linear: f(x) = x ≤ 0.04045 ? x/12.92 : ((x+0.055)/1.055)^2.4
  2. Linear RGB → LMS (M1 matrix, Ottosson)
  3. LMS → OKLab (M2 matrix on cube roots)
  4. OKLab → OKLCH: C = sqrt(a² + b²), H = atan2(b, a)

Reverse (OKLCH → RGB):
  1. OKLCH → OKLab: a = C*cos(H), b = C*sin(H)
  2. OKLab → LMS (inverse, cube instead of cube root)
  3. LMS → Linear RGB (inverse M1)
  4. Linear → sRGB (gamma correction)

Interpolation (OKLCH):
  lerp(L, C, H) preserves perceptual uniformity
  Hue wraps: use shorter arc between h1 and h2
\\\

---

## Frame Lifecycle (App.fs via Tea.fs)

\\\
1. Input Event (keyboard/mouse)
   ↓
2. Event → Subscribe (Fire events through program.Subscribe)
   ↓
3. Dispatch Message (through drain queue)
   ↓
4. Update message → (model', cmd)
   ↓
5. Process Cmd (Quit, Delay 0, DirectMsg: synchronous drain loop)
   ↓
6. View model → Element tree
   ↓
7. Arena.lower: Element → FrameArena
   ↓
8. ArenaRender.render: FrameArena → Buffer
   ↓
9. Transition.apply: Overlay transition effects
   ↓
10. Buffer.diffInto: SIMD diff (prev → curr)
    ↓
11. Terminal.update: Send changed cells to terminal (via ANSI codes)
    ↓
12. If Quit: exit; else loop to step 1
\\\

---

## Key Constants & Formulas

### SIMD Diff
\\\
cellSz = sizeof<PackedCell> = 16 bytes
chunkCells = 16 cells per chunk
chunkBytes = 16 * 16 = 256 bytes
\\\

### Rune Width Classification (RuneWidth.getColumnWidth)
\\\
v < 0x20              → width 0 (control chars)
v in wide ranges      → width 2 (CJK, etc.)
else                  → width 1 (ASCII, Latin, etc.)

Wide ranges:
  0x1100..0x115F, 0x2E80..0x9FFF, 0xAC00..0xD7A3,
  0xF900..0xFAFF, 0xFE10..0xFE6F, 0xFF01..0xFF60,
  0xFFE0..0xFFE6, 0x20000..0x2FFFD, 0x30000..0x3FFFD
\\\

### HalfBlock Compression
\\\
pixelWidth  → termWidth (1:1)
pixelHeight → termHeight (2:1, formula: (h+1)/2)
\\\

### Braille Compression
\\\
pixelWidth  → termWidth (2:1, formula: (w+1)/2)
pixelHeight → termHeight (4:1, formula: (h+3)/4)
\\\

---

## File Size Reference

| File | Purpose | Lines |
|------|---------|-------|
| Color.fs | Color DU, packing | 121 |
| Buffer.fs | PackedCell, SIMD diff | 148 |
| Layout.fs | Constraint solver | 320 |
| Element.fs | Element DU, El module | 400+ |
| Measure.fs | Content sizing | 92 |
| CanvasRender.fs | Pixel rendering | 81 |
| Arena.fs | Serialization | 310+ |
| Effects.fs | Easing, OKLCH, gradients | 193 |
| Render.fs | Reference renderer | 150+ |
| ArenaRender.fs | Zero-alloc renderer | 600+ |
| Transition.fs | Animations | 310 |
| Tea.fs | Elm arch | 250+ |
| Testing.fs | TestHarness | 200+ |

---

## Most Critical Code Sections

### For Performance Review
1. **Buffer.diffInto** (Buffer.fs:108-129) - SIMD chunk-skip logic
2. **ArenaRender.render Row/Column** (ArenaRender.fs:233-358) - Inline layout
3. **Layout.solve/solveWithContent** (Layout.fs:33-228) - Constraint resolution
4. **Arena.lower** (Arena.fs:143-305) - Serialization strategy

### For Correctness Review
1. **Layout.distributeFill** (ArenaRender.fs:39-69) - Hamilton method
2. **TransitionFx.apply*** (Transition.fs:83-237) - Cell interpolation
3. **Oklch.fromRgb/toRgb** (Effects.fs:67-103) - Color space conversion
4. **brailleCell** (CanvasRender.fs:30-44) - Braille bit mapping

### For Architecture Review
1. **Compilation order** (SageTUI.Library.fsproj:31-52) - Module dependencies
2. **Render vs ArenaRender** (Render.fs vs ArenaRender.fs) - Strategy pattern
3. **TestApp design** (Testing.fs:27-48) - Deterministic testing
4. **Transition reconciliation** (Transition.fs:284-309) - Enter/exit logic

