# SAGETUI COMPREHENSIVE CODEBASE ANALYSIS

## Executive Summary
SageTUI is an F# Terminal User Interface (TUI) library designed with:
- **Arena allocation** and **SIMD diff** as first-class concerns
- **Elm Architecture** (Model-View-Update) for state management
- **Zero-GC frame loops** for high-performance rendering
- **Blittable 16-byte PackedCell** structs for SIMD-accelerated buffer diffing
- A **9-case Element DU** for declarative UI composition
- Clean layered architecture with no circular dependencies

**Status**: This is a design synthesis from 15 expert panels + deliberations. The code does NOT currently exist — only the design/architecture documentation is present (in ~/expertPanel/synthesis.md).

---

## 1. DIRECTORY STRUCTURE

`
C:\Code\Repos\SageTUI\
├── Program.fs                    # Currently just "Hello from F#" placeholder
├── SageTUI.fsproj               # .NET 11.0 console app
├── SageTUI.slnx                 # Solution file
├── expertPanel/                 # Design documentation (COMPREHENSIVE)
│   ├── synthesis.md             # Complete architectural design (1,500+ lines)
│   ├── tui-research.md          # TUI library landscape analysis (1,200+ lines)
│   ├── {01-15}-*.md             # 15 expert panel discussions
│   ├── arena-simd-deliberation.md   # Arena + SIMD deep dive
│   ├── forms-concurrency-deliberation.md # Forms & async patterns
│   └── ...other research docs
├── hooks/                       # Git hooks for CI/CD
└── ...standard project files
`

**Key finding**: NO IMPLEMENTATION EXISTS YET. The repository contains ONLY the design synthesis.
This is a blank slate ready for implementation.

---

## 2. ELEMENT DISCRIMINATED UNION (9 Cases)

`sharp
type Element =
  | Empty                                    // Identity element (Row/Col neutral)
  | Text of string * Style                   // Styled text content
  | Row of Element list                      // Horizontal composition (equal distribution)
  | Column of Element list                   // Vertical composition (equal distribution)
  | Overlay of Element list                  // Z-axis composition (last element on top)
  | Styled of Style * Element                // Apply style to entire subtree
  | Constrained of Constraint * Element      // Explicit dimension constraint
  | Bordered of BorderStyle * Element        // Draw border around element
  | Padded of Padding * Element              // Add spacing around element
`

**Why 9 cases**:
- **Miller's decision**: Sweet spot between minimal (7 cases: too little) and maximal (13+ cases: too complex)
- **Overlay**: Third monoid dimension; enables modals, popups, floating panels
- **Styled wrapper**: Needed for subtree styling; separate from Text inline styling for tree normalization
- **Constrained**: Explicit layout control without mangling Row/Column semantics
- **Bordered/Padded**: Common operations that justify explicit cases for clarity

**Algebraic properties** (all proven):
`sharp
Row [Empty; a] = a           // Empty is identity
Row [Row [a; b]; c] = Row [a; b; c]  // Associativity (flattens)
Styled(empty, e) = e         // Empty style is identity
Styled(s1, Styled(s2, e)) = Styled(merge s1 s2, e)  // Style composition
`

---

## 3. STYLE AND COLOR SYSTEM

### Style Type
`sharp
type Style = {
  Fg: Color option         // None = inherit from parent, Some = override
  Bg: Color option         // Same inheritance semantics
  Attrs: TextAttrs         // Text attributes (bold, italic, etc.)
}

module Style =
  let empty = { Fg = None; Bg = None; Attrs = TextAttrs.none }
  
  // Monoid operation: later style wins for colors, attrs combine via OR
  let merge base' overlay =
    { Fg = overlay.Fg |> Option.orElse base'.Fg
      Bg = overlay.Bg |> Option.orElse base'.Bg
      Attrs = TextAttrs.combine base'.Attrs overlay.Attrs }
`

**Why Option for colors**: Distinguishes "inherit" (None) from "explicit default" (Some Default).
This is essential for proper style composition in nested elements.

### Color Type (Full Terminal Color Space)
`sharp
type Intensity = Normal | Bright

type BaseColor =
  | Black | Red | Green | Yellow
  | Blue | Magenta | Cyan | White

type Color =
  | Default                              // Terminal's default color
  | Named of BaseColor * Intensity       // 16 standard colors (8 base × 2 intensity)
  | Indexed of byte                      // 256-color palette mode
  | Rgb of r: byte * g: byte * b: byte   // 24-bit true color (truecolor)
`

**Why BaseColor × Intensity** (not flat enum):
- Composability: map over intensity without 16 pattern matches
- Prevents nesting: can't accidentally write Bright(Bright x)

### TextAttrs (Text Attributes)
`sharp
[<Struct>]
type TextAttrs = private TextAttrs of uint16

module TextAttrs =
  let none = TextAttrs 0us
  let bold = TextAttrs 1us
  let dim = TextAttrs 2us
  let italic = TextAttrs 4us
  let underline = TextAttrs 8us
  let reverse = TextAttrs 16us
  let strikethrough = TextAttrs 32us
  let blink = TextAttrs 64us
  let hidden = TextAttrs 128us
  let overline = TextAttrs 256us
  
  let combine (TextAttrs a) (TextAttrs b) = TextAttrs(a ||| b)
  let has (TextAttrs flag) (TextAttrs attrs) = attrs &&& flag <> 0us
  let remove (TextAttrs flag) (TextAttrs attrs) = TextAttrs(attrs &&& ~~~flag)
`

**Why bitmask (not Set<TextAttr>)**:
- O(1) operations vs O(log n) with allocations
- Type-safe wrapper prevents accidental bit manipulation by users
- Holden initially proposed Set; panel converged on bitmask after seeing GC pressure

### PackedColor (Buffer-Layer Packing)
`sharp
module PackedColor =
  // Bit layout in int32:
  // Bits 31-28: kind (0=Default, 1=Named, 2=Indexed, 3=Rgb)
  // Bits 27-24: intensity (for Named)
  // Bits 23-16: red / base color / index
  // Bits 15-8:  green
  // Bits 7-0:   blue
  
  let pack (color: Color) : int32 = ...
  let unpack (packed: int32) : Color = ...
`

**Why pack during buffer write, not diff**:
- Color DU is variable-size; can't fit in blittable struct
- Packing happens ONCE during view→buffer write
- Diff uses int32 directly (no DU allocation in hot path)
- Unpack only when outputting ANSI (happens once per changed cell)

---

## 4. CONSTRAINT SYSTEM

`sharp
type Constraint =
  | Fixed of int        // Exactly n cells
  | Min of int          // At least n cells
  | Max of int          // At most n cells
  | Percentage of int   // Proportion of parent (0-100)
  | Fill                // Take remaining space (default)
  | Ratio of int * int  // Proportional (e.g., Ratio(1, 3) = one-third)
`

**Why these 6**:
- Converged from Ratatui's proven set (used by most successful TUI libraries)
- Primeagen, Carmack, Bill all validated: real apps need exactly these—no more, no less
- Fill is default for Row/Column children

**Solver algorithm** (single-pass):
1. Allocate Fixed, Percentage, Ratio constraints → consume space
2. Allocate Min/Max with bound checking
3. Distribute remaining space to Fill constraints equally

---

## 5. ELEMENT CONSTRUCTION API (El Module)

`sharp
module El =
  // Construction
  let empty = Empty
  let text s = Text(s, Style.empty)
  let styledText style s = Text(s, style)
  let row children = Row children
  let column children = Column children
  let overlay layers = Overlay layers
  
  // Style combinators (wrap in Styled node)
  let styled style elem = Styled(style, elem)
  let fg color elem = Styled({ Style.empty with Fg = Some color }, elem)
  let bg color elem = Styled({ Style.empty with Bg = Some color }, elem)
  let bold elem = Styled({ Style.empty with Attrs = TextAttrs.bold }, elem)
  let dim elem = Styled({ Style.empty with Attrs = TextAttrs.dim }, elem)
  let italic elem = Styled({ Style.empty with Attrs = TextAttrs.italic }, elem)
  let underline elem = Styled({ Style.empty with Attrs = TextAttrs.underline }, elem)
  let reverse elem = Styled({ Style.empty with Attrs = TextAttrs.reverse }, elem)
  let strikethrough elem = Styled({ Style.empty with Attrs = TextAttrs.strikethrough }, elem)
  
  // Layout combinators
  let width n elem = Constrained(Fixed n, elem)
  let minWidth n elem = Constrained(Min n, elem)
  let maxWidth n elem = Constrained(Max n, elem)
  let fill elem = Constrained(Fill, elem)
  let percentage pct elem = Constrained(Percentage pct, elem)
  let ratio num den elem = Constrained(Ratio(num, den), elem)
  
  // Decoration combinators
  let bordered style elem = Bordered(style, elem)
  let border elem = Bordered(Light, elem)
  let padded p elem = Padded(p, elem)
  let padAll n elem = Padded({ Top = n; Right = n; Bottom = n; Left = n }, elem)
  let padHV h v elem = Padded({ Top = v; Right = h; Bottom = v; Left = h }, elem)
`

**Usage example**:
`sharp
El.text "hello"
|> El.bold
|> El.fg (Named(Cyan, Bright))
|> El.padAll 1
|> El.border
`

---

## 6. PACKEDCELL: 16-BYTE BLITTABLE STRUCT

`sharp
open System.Runtime.InteropServices

[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type PackedCell =
  { Rune: int32         // 4 bytes — System.Text.Rune scalar value
    Fg: int32           // 4 bytes — packed color (kind:4 | data:28)
    Bg: int32           // 4 bytes — packed color (kind:4 | data:28)
    Attrs: uint16       // 2 bytes — TextAttrs bitmask
    _pad: uint16 }      // 2 bytes — pad to 16 for SIMD alignment
  
  // Total: 16 bytes (power-of-2, perfect for Vector128<byte>)
  // Blittable: no pointers, no references, fixed size
  // StructLayout(Sequential, Pack=1): prevents .NET inserting padding
`

**Why Rune as int32**:
- System.Text.Rune is internally an int32
- Storing scalar directly avoids struct wrapping overhead
- Keeps cell blittable
- Reconstruct Rune(value) when needed for ANSI output

**Why 16 bytes exactly**:
- SIMD diff uses Vector128<byte> comparison (processes exactly 16 bytes)
- Power-of-2 size enables optimal cache alignment
- One cell = one SIMD operation

**At startup, assert**:
`sharp
assert(sizeof<PackedCell> = 16)
`

---

## 7. BUFFER TYPE AND OPERATIONS

`sharp
type Buffer = {
  Cells: PackedCell array      // 1D flat array (row * width + col indexing)
  Width: int
  Height: int
}

module Buffer =
  let create width height =
    { Cells = Array.create (width * height) emptyCell; Width = width; Height = height }
  
  let get x y buf =
    if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height
    then buf.Cells[y * buf.Width + x]
    else emptyCell
  
  let set x y cell buf =
    if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height
    then buf.Cells[y * buf.Width + x] <- cell
  
  let writeString x y (fg: int32) (bg: int32) (attrs: uint16) (text: string) buf =
    let mutable col = x
    for rune in text.EnumerateRunes() do
      if col < buf.Width then
        set col y { Rune = rune.Value; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us } buf
        col <- col + Rune.getColumnWidth rune
  
  let clear buf =
    System.Array.Fill(buf.Cells, emptyCell)  // JIT-vectorized in .NET 8+
`

**Why flat 1D array (not 2D)**:
- Better cache locality
- Enables MemoryMarshal.AsBytes for SIMD operations
- Span<PackedCell> slicing is natural

**Why silent clipping**:
- Bill's design: better to clip than crash
- Every real terminal clips anyway
- Validation is the domain of the application layer

---

## 8. SIMD-ACCELERATED DIFF ALGORITHM

`sharp
let diff (prev: Buffer) (curr: Buffer) =
  let changes = ResizeArray<int>()
  let prevBytes = MemoryMarshal.AsBytes(prev.Cells.AsSpan())
  let currBytes = MemoryMarshal.AsBytes(curr.Cells.AsSpan())
  let cellSize = 16  // sizeof<PackedCell>
  let chunkCells = 16
  let chunkBytes = chunkCells * cellSize  // 256 bytes per chunk
  let totalCells = curr.Cells.Length
  let mutable cellIdx = 0
  
  // Phase 1: chunk scan — skip entire 16-cell chunks that haven't changed
  while cellIdx + chunkCells <= totalCells do
    let byteStart = cellIdx * cellSize
    if not (prevBytes.Slice(byteStart, chunkBytes).SequenceEqual(currBytes.Slice(byteStart, chunkBytes))) then
      // This chunk has changes — drill into individual cells
      for i in cellIdx .. cellIdx + chunkCells - 1 do
        let bs = i * cellSize
        if not (prevBytes.Slice(bs, cellSize).SequenceEqual(currBytes.Slice(bs, cellSize))) then
          changes.Add(i)
    cellIdx <- cellIdx + chunkCells
  
  // Phase 2: remaining cells (< 16)
  while cellIdx < totalCells do
    let bs = cellIdx * cellSize
    if not (prevBytes.Slice(bs, cellSize).SequenceEqual(currBytes.Slice(bs, cellSize))) then
      changes.Add(cellIdx)
    cellIdx <- cellIdx + 1
  
  changes
`

**Performance** (200×50 terminal = 10K cells, 95% unchanged):
- **Chunk-skip pattern**: ~3-5µs total
- **Naive cell-by-cell**: 50-100µs (10-20× SLOWER)
- **Why**: Span<T>.SequenceEqual uses SIMD internally in .NET 8+
  - AVX2 on x86: ~3-5ns per chunk
  - ARM NEON on ARM: similar
  - JIT picks best instruction set automatically

**Why not hand-written SSE2/AVX**:
- .SequenceEqual gets faster with every .NET release for free
- Portable: works on any .NET 8+ platform
- Code is actually simpler than intrinsics

---

## 9. FRAME ARENA (Zero-GC Element Trees)

`sharp
[<Struct>]
type ElementNode =
  { Kind: byte           // 0=Empty, 1=Text, 2=Row, 3=Col, 4=Overlay, 5=Styled, 6=Constrained, 7=Bordered, 8=Padded
    StylePacked: uint64  // fg(32bits) + bg(32bits) packed colors
    AttrsPacked: uint16  // TextAttrs bitmask
    ConstraintKind: byte // constraint type (if Kind = 6)
    ConstraintVal: int16 // constraint value
    FirstChild: int      // index into pool, -1 = none
    NextSibling: int     // index into pool, -1 = none
    DataStart: int       // offset into text buffer
    DataLen: int }       // length in text buffer

[<Struct>]
type NodeHandle = private NodeHandle of int  // Typed index (prevents confusion)

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
      failwith \$"FrameArena overflow"
    let idx = arena.NodeCount
    arena.NodeCount <- arena.NodeCount + 1
    NodeHandle idx
  
  let allocText (text: string) arena =
    let start = arena.TextPos
    text.CopyTo(0, arena.TextBuf, start, text.Length)
    arena.TextPos <- arena.TextPos + text.Length
    (start, text.Length)

module Arena =
  let rec lower (arena: FrameArena) (elem: Element) : NodeHandle =
    match elem with
    | Empty -> ...allocate and write zero node...
    | Text(text, style) -> ...allocate node, allocate text, write node with packed colors...
    | Row children -> ...allocate node, recursively lower children, link siblings...
    | Column children -> ...same as Row...
    | Styled(style, child) -> ...allocate node, lower child, set as FirstChild...
    | Overlay children -> ...allocate node, lower children, link siblings...
    | Constrained(constraint, child) -> ...encode constraint in Kind/ConstraintKind/ConstraintVal...
    | Bordered(style, child) -> ...encode border style...
    | Padded(padding, child) -> ...encode padding...
`

**Why arena allocation**:
- User's iew function builds Element DU tree every frame (GC'd, unavoidable)
- lower converts DU to flat struct arrays in O(n): ONE PASS, ZERO heap allocations in arena
- Layout/render read from arena, not DU tree
- eset is O(1): just reset counters
- Pre-allocated arrays mean predictable memory layout

**Lifetime grouping**:
| What | Lifetime | Strategy |
|------|----------|----------|
| Element nodes, temp strings, layout scratch | Per frame | Arena (reset per frame) |
| Front/back buffers | Two frames | Double-buffered, swap references |
| App config, model, subscriptions | Application | Normal GC heap |

**Key decision**: DU stays as public API, arena is internal optimization.
Users get pattern matching + exhaustiveness checking. Arena is invisible.

---

## 10. RENDER LOOP AND RUNTIME ARCHITECTURE

### Elm Architecture Types

`sharp
type TerminalEvent =
  | KeyPressed of key: Key * modifiers: Modifiers
  | MouseInput of MouseEvent
  | Resized of width: int * height: int
  | FocusGained
  | FocusLost
  | Pasted of string

type Cmd<'msg> =
  | NoCmd
  | Batch of Cmd<'msg> list
  | OfAsync of (('msg -> unit) -> Async<unit>)                    // Fire-and-forget
  | OfCancellableAsync of id: string * (CancellationToken -> ('msg -> unit) -> Async<unit>)  // Long-lived
  | CancelSub of string                                            // Explicit cancellation
  | Delay of milliseconds: int * 'msg                              // Timer
  | Quit

type Sub<'msg> =
  | KeySub of (Key * Modifiers -> 'msg option)                    // Filter key events
  | TimerSub of id: string * interval: TimeSpan * tick: (unit -> 'msg)  // Recurring timer
  | ResizeSub of (int * int -> 'msg)                              // Terminal resize
  | Custom of id: string * start: (('msg -> unit) -> CancellationToken -> Async<unit>)  // Arbitrary event source

type Program<'model, 'msg> = {
  Init: unit -> 'model * Cmd<'msg>
  Update: 'msg -> 'model -> 'model * Cmd<'msg>
  View: 'model -> Element
  Subscribe: 'model -> Sub<'msg> list
}
`

**Why effects-as-data (Cmd DU, not callbacks)**:
- Testable: inspect the command data, don't run it
- Serializable: for debugging/replay
- Pure update function: no side effects in Update
- Composability: Cmd.map for component composition

**Why OfCancellableAsync with string IDs**:
- Long-lived subscriptions (WebSocket, file watcher) need cancellation
- String ID is sufficient for v0; upgrade to typed DU if typos cause real bugs
- Auto-cancels on re-issue with same ID (prevents orphaned tasks)

### Render Loop

`sharp
module App =
  let run (backend: TerminalBackend) (program: Program<'model, 'msg>) =
    let width, height = backend.Size()
    let mutable model, initCmd = program.Init()
    let mutable frontBuf = Buffer.create width height
    let mutable backBuf = Buffer.create width height
    let arena = FrameArena.create 4096 65536 4096  // Configurable
    let mutable running = true
    
    // Thread-safe dispatch from async callbacks
    let msgChannel = System.Collections.Concurrent.ConcurrentQueue<'msg>()
    let dispatch msg = msgChannel.Enqueue(msg)
    
    // Subscription lifecycle management
    let activeSubs = System.Collections.Generic.Dictionary<string, CancellationTokenSource>()
    
    let rec interpretCmd = function
      | NoCmd -> ()
      | Batch cmds -> cmds |> List.iter interpretCmd
      | OfAsync run ->
          async { try do! run dispatch with _ -> () } |> Async.Start
      | OfCancellableAsync(id, run) ->
          match activeSubs.TryGetValue(id) with
          | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
          | _ -> ()
          let cts = new CancellationTokenSource()
          activeSubs.[id] <- cts
          Async.Start(async {
            try do! run cts.Token dispatch
            with :? OperationCanceledException -> ()
          }, cts.Token)
      | CancelSub id ->
          match activeSubs.TryGetValue(id) with
          | true, cts -> cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
          | _ -> ()
      | Delay(ms, msg) ->
          async { do! Async.Sleep ms; dispatch msg } |> Async.Start
      | Quit ->
          for kvp in activeSubs do kvp.Value.Cancel(); kvp.Value.Dispose()
          activeSubs.Clear()
          running <- false
    
    let reconcileSubs (currentSubs: Sub<'msg> list) =
      let currentIds =
        currentSubs
        |> List.choose (function
          | TimerSub(id, _, _) -> Some id
          | Custom(id, _) -> Some id
          | _ -> None)
        |> Set.ofList
      // Cancel subs that disappeared
      for KeyValue(id, cts) in activeSubs |> Seq.toList do
        if not (Set.contains id currentIds) then
          cts.Cancel(); cts.Dispose(); activeSubs.Remove(id) |> ignore
      // Start new subs
      for sub in currentSubs do
        match sub with
        | TimerSub(id, interval, tick) when not (activeSubs.ContainsKey(id)) ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(async {
              while not cts.Token.IsCancellationRequested do
                do! Async.Sleep (int interval.TotalMilliseconds)
                dispatch (tick())
            }, cts.Token)
        | Custom(id, start) when not (activeSubs.ContainsKey(id)) ->
            let cts = new CancellationTokenSource()
            activeSubs.[id] <- cts
            Async.Start(start dispatch cts.Token, cts.Token)
        | _ -> ()
    
    backend.EnterRawMode()
    backend.Write(Ansi.enterAltScreen + Ansi.hideCursor)
    
    interpretCmd initCmd
    
    // ========== MAIN LOOP ==========
    while running do
      // 1. Drain ALL pending messages (thread-safe, single-threaded model access)
      let mutable msg = Unchecked.defaultof<'msg>
      while msgChannel.TryDequeue(&msg) do
        let newModel, cmd = program.Update msg model
        model <- newModel
        interpretCmd cmd
      
      // 2. Reconcile subscriptions based on current model
      let subs = program.Subscribe model
      reconcileSubs subs
      
      // 3. User's view function produces Element DU
      let elem = program.View model
      
      // 4. Lower DU into frame arena (one tree walk, zero heap alloc in arena)
      FrameArena.reset arena
      let _rootHandle = Arena.lower arena elem
      
      // 5. Render into back buffer (reads from Element DU, writes PackedCells)
      Buffer.clear backBuf
      let area = { X = 0; Y = 0; Width = width; Height = height }
      Render.render area Style.empty backBuf elem
      
      // 6. SIMD-accelerated diff (chunk-skip pattern, Span.SequenceEqual)
      let changes = Buffer.diff frontBuf backBuf
      
      // 7. Present changed cells as ANSI output (batched)
      if changes.Count > 0 then
        let output = Presenter.present changes backBuf
        backend.Write(output)
        backend.Flush()
      
      // 8. Swap buffers (O(1), just swap references)
      let temp = frontBuf
      frontBuf <- backBuf
      backBuf <- temp
      
      // 9. Poll for terminal events → dispatch as messages
      match backend.PollEvent 16 with  // ~60fps max when events flowing
      | Some event ->
          for sub in subs do
            match sub, event with
            | KeySub handler, KeyPressed(key, mods) ->
                handler (key, mods) |> Option.iter dispatch
            | ResizeSub handler, Resized(w, h) ->
                handler (w, h) |> dispatch
            | _ -> ()
      | None -> ()
    
    backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)
    backend.LeaveRawMode()
`

### Performance Budget (200×50 terminal = 10K cells)

| Step | Time | Allocations |
|------|------|-------------|
| View (build Element DU) | ~5-20µs | DU nodes (Gen0, GC'd) |
| Arena.lower | ~2-5µs | 0 |
| Buffer.clear | <1µs | 0 |
| Render.render | ~10-50µs | 0 |
| Buffer.diff (SIMD) | ~3-5µs | 0 |
| Presenter.present | ~5-20µs | 1 StringBuilder |
| backend.Write | 1-10ms | 0 |
| **Total per frame** | **~1-10ms** | **<5 heap objects** |

**Key insight**: Terminal I/O (1-10ms) is the bottleneck, not rendering. Everything else is noise compared to the write.

---

## 11. ANSI BACKEND AND PRESENTER

### Ansi Module (Pure String Building)
`sharp
module Ansi =
  let esc = "\x1b["
  
  let moveCursor row col = sprintf "%s%d;%dH" esc (row + 1) (col + 1)
  let hideCursor = sprintf "%s?25l" esc
  let showCursor = sprintf "%s?25h" esc
  let enterAltScreen = sprintf "%s?1049h" esc
  let leaveAltScreen = sprintf "%s?1049l" esc
  let enableMouseTracking = sprintf "%s?1000h%s?1006h" esc esc
  let disableMouseTracking = sprintf "%s?1000l%s?1006l" esc esc
  let resetStyle = sprintf "%s0m" esc
  
  let fgColor = function
    | Default -> sprintf "%s39m" esc
    | Named(base', intensity) ->
        let code = match base' with
                   | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
                   | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
        let offset = match intensity with Normal -> 30 | Bright -> 90
        sprintf "%s%dm" esc (offset + code)
    | Indexed idx -> sprintf "%s38;5;%dm" esc idx
    | Rgb(r, g, b) -> sprintf "%s38;2;%d;%d;%dm" esc r g b
  
  let bgColor = function
    | Default -> sprintf "%s49m" esc
    | Named(base', intensity) ->
        let code = match base' with
                   | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3
                   | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7
        let offset = match intensity with Normal -> 40 | Bright -> 100
        sprintf "%s%dm" esc (offset + code)
    | Indexed idx -> sprintf "%s48;5;%dm" esc idx
    | Rgb(r, g, b) -> sprintf "%s48;2;%d;%d;%dm" esc r g b
  
  let textAttrs (attrs: TextAttrs) =
    let sb = System.Text.StringBuilder()
    if TextAttrs.has TextAttrs.bold attrs then sb.Append(sprintf "%s1m" esc) |> ignore
    if TextAttrs.has TextAttrs.dim attrs then sb.Append(sprintf "%s2m" esc) |> ignore
    if TextAttrs.has TextAttrs.italic attrs then sb.Append(sprintf "%s3m" esc) |> ignore
    if TextAttrs.has TextAttrs.underline attrs then sb.Append(sprintf "%s4m" esc) |> ignore
    if TextAttrs.has TextAttrs.blink attrs then sb.Append(sprintf "%s5m" esc) |> ignore
    if TextAttrs.has TextAttrs.reverse attrs then sb.Append(sprintf "%s7m" esc) |> ignore
    if TextAttrs.has TextAttrs.hidden attrs then sb.Append(sprintf "%s8m" esc) |> ignore
    if TextAttrs.has TextAttrs.strikethrough attrs then sb.Append(sprintf "%s9m" esc) |> ignore
    sb.ToString()
  
  // Packed variants — work directly with int32/uint16 from PackedCell
  let fgColorPacked (packed: int32) = fgColor (PackedColor.unpack packed)
  let bgColorPacked (packed: int32) = bgColor (PackedColor.unpack packed)
  let textAttrsPacked (packed: uint16) = textAttrs (TextAttrs packed)
`

**Why own the ANSI** (Fluery's contribution):
- No System.Console (limited to 16 colors, no cursor control)
- No crossterm interop (interop hell, Stannard ejected this)
- ~200 lines of pure F# string building
- Debuggable: pipe stdout to xxd to inspect raw bytes

### Presenter Module (Batched ANSI Output)

`sharp
module Presenter =
  let present (changes: ResizeArray<int>) (buf: Buffer) =
    let sb = System.Text.StringBuilder()
    let mutable lastRow = -1
    let mutable lastCol = -1
    let mutable lastFg = 0
    let mutable lastBg = 0
    let mutable lastAttrs = 0us
    
    for idx in changes do
      let x = idx % buf.Width
      let y = idx / buf.Width
      let cell = buf.Cells.[idx]
      
      // Only emit cursor move if not contiguous
      if y <> lastRow || x <> lastCol + 1 then
        sb.Append(Ansi.moveCursor y x) |> ignore
      
      // Only emit style if changed
      if cell.Fg <> lastFg || cell.Bg <> lastBg || cell.Attrs <> lastAttrs then
        sb.Append(Ansi.resetStyle) |> ignore
        sb.Append(Ansi.fgColorPacked cell.Fg) |> ignore
        sb.Append(Ansi.bgColorPacked cell.Bg) |> ignore
        sb.Append(Ansi.textAttrsPacked cell.Attrs) |> ignore
        lastFg <- cell.Fg
        lastBg <- cell.Bg
        lastAttrs <- cell.Attrs
      
      let rune = Rune(cell.Rune)
      sb.Append(rune.ToString()) |> ignore
      lastCol <- x + (Rune.getColumnWidth rune) - 1
      lastRow <- y
    
    sb.ToString()
`

**Why batch** (Carmack's 30 years of rendering wisdom):
- Every cursor move: ~8 bytes
- Every color change: ~12 bytes
- Contiguous same-colored cells: ZERO overhead
- **Result**: Full-screen update from ~20KB to ~2KB of escape sequences
- **Single biggest performance win** in any TUI library

---

## 12. TERMINAL BACKEND (Record of Functions)

`sharp
type TerminalBackend = {
  Size: unit -> int * int
  Write: string -> unit
  Flush: unit -> unit
  PollEvent: int -> TerminalEvent option  // timeout in ms
  EnterRawMode: unit -> unit
  LeaveRawMode: unit -> unit
}

module TerminalBackend =
  // Real terminal backend using ANSI sequences
  let ansi () : TerminalBackend =
    { Size = fun () -> Console.WindowWidth, Console.WindowHeight
      Write = fun s -> Console.Out.Write(s)
      Flush = fun () -> Console.Out.Flush()
      PollEvent = fun timeout ->
        if Console.KeyAvailable || timeout = 0 then
          let key = Console.ReadKey(true)
          Some (KeyPressed(mapKey key, mapModifiers key))
        else
          System.Threading.Thread.Sleep(timeout)
          if Console.KeyAvailable then
            let key = Console.ReadKey(true)
            Some (KeyPressed(mapKey key, mapModifiers key))
          else None
      EnterRawMode = fun () -> () // Platform-specific: Windows vs Unix
      LeaveRawMode = fun () -> () }
  
  // Test backend for unit testing
  let test (width: int) (height: int) (events: TerminalEvent list) : TerminalBackend * (unit -> string) =
    let output = System.Text.StringBuilder()
    let eventQueue = System.Collections.Generic.Queue(events)
    let backend =
      { Size = fun () -> width, height
        Write = fun s -> output.Append(s) |> ignore
        Flush = fun () -> ()
        PollEvent = fun _ ->
          if eventQueue.Count > 0 then Some(eventQueue.Dequeue())
          else None
        EnterRawMode = fun () -> ()
        LeaveRawMode = fun () -> () }
    backend, fun () -> output.ToString()
`

**Why record of functions (not interface)** (Stannard/Syme):
- More idiomatic F#
- Compose via { backend with Write = ... } (structural override)
- Enables partial mocking without 	ype MyTestTerminal() = interface ITerminal with ...

**Test backend as public API** (Miller):
- Every TUI library should ship with a way to test without a real terminal
- Records output, replays events
- Combined with Buffer.toString: snapshot-test entire UI states

---

## 13. ANIMATION AND VISUAL EFFECTS ARCHITECTURE

**Current position** (from expert panel deliberation):
- **No built-in animation framework**
- Animations are **subscriptions that send Tick messages**
- User handles state transitions explicitly

### Recommended Pattern for Animation

`sharp
// Animation state in model
type AnimationState = {
  StartTime: DateTime
  Duration: TimeSpan
  Current: float  // 0.0 to 1.0 (progress)
}

// Model includes animation
type Model = {
  IsAnimating: bool
  Animation: AnimationState option
  // ... other state
}

type Msg =
  | StartAnimation
  | AnimationTick of DateTime
  | AnimationComplete
  | ...

// Subscription: emit ticks during animation
let subscribe model =
  if model.IsAnimating then
    [ TimerSub("animation", TimeSpan.FromMilliseconds 16., fun () ->
        AnimationTick DateTime.UtcNow) ]
  else
    []

// Update: compute progress, update model
let update msg model =
  match msg with
  | AnimationTick now ->
      match model.Animation with
      | Some anim ->
          let elapsed = now - anim.StartTime
          if elapsed >= anim.Duration then
            { model with IsAnimating = false; Animation = None }, Cmd.none
          else
            let progress = elapsed.TotalMilliseconds / anim.Duration.TotalMilliseconds
            let newAnim = { anim with Current = progress }
            { model with Animation = Some newAnim }, Cmd.none
      | None -> model, Cmd.none
  | ...

// View uses progress for visual state
let view model =
  match model.Animation with
  | Some anim ->
      let width = int (40.0 * anim.Current) |> max 1  // Grow from 1 to 40
      let color =
        if anim.Current < 0.5 then
          Named(Red, Normal)
        else
          Named(Green, Normal)
      El.text "Loading"
      |> El.width (Fixed width)
      |> El.fg color
  | None ->
      El.text "Ready"
      |> El.fg (Named(Cyan, Normal))
`

### Performance Architecture for Animation

From Carmack/Haynes deliberation:
- **Event-driven rendering by default** (0 fps when idle)
- **Switch to fixed-rate during animations** (16-60ms ticks = smooth scroll)
- **Budget applies only during animation** (not idle→render→idle)

`sharp
// Extend Cmd with animation support (future extension)
type Cmd<'msg> =
  | ...existing cases...
  | Animate of interval: TimeSpan * tick: (DateTime -> 'msg) * cancel: (unit -> 'msg)
`

### Color Gradients / Transitions

While no built-in gradient system exists, the architecture supports user-defined helpers:

`sharp
// User-defined color interpolation
module ColorInterpolation =
  let lerpRgb (r1, g1, b1) (r2, g2, b2) (t: float) : byte * byte * byte =
    let lerp a b t = byte (float a + (float b - float a) * t)
    (lerp r1 r2 t, lerp g1 g2 t, lerp b1 b2 t)
  
  let colorGradient startColor endColor progress =
    match startColor, endColor with
    | Rgb(r1, g1, b1), Rgb(r2, g2, b2) ->
        let (r, g, b) = lerpRgb (r1, g1, b1) (r2, g2, b2) progress
        Rgb(r, g, b)
    | _ -> startColor  // Non-RGB: use start color

// Usage in view
let animatedColor =
  ColorInterpolation.colorGradient
    (Rgb(255uy, 0uy, 0uy))
    (Rgb(0uy, 255uy, 0uy))
    model.Animation.Current
`

### Spinner / Progress Examples

`sharp
// Spinner indicator
let spinnerChars = [| '⠋'; '⠙'; '⠹'; '⠸'; '⠼'; '⠴'; '⠦'; '⠧'; '⠇'; '⠏' |]

let spinnerFrame tickCount =
  spinnerChars.[tickCount % spinnerChars.Length]

// Progress bar
let progressBar progress maxWidth =
  let filled = int (float maxWidth * progress) |> min maxWidth
  let empty = maxWidth - filled
  let bar = String.replicate filled "█" + String.replicate empty "░"
  El.text bar
  |> El.fg (Named(Green, Normal))
`

---

## 14. EXISTING EXAMPLE/HELPER CODE (Planned, Not Yet Implemented)

These are planned as example modules (~1,700 LOC total), shipped separately from core:

### TextInput Module (~80 LOC)
- State: { Value: string; Cursor: int }
- Handles: insert, delete, move cursor, select
- View: renders text with cursor at position

### FocusRing Module (~25 LOC)
- Generic focus cycling for forms
- Wraps a list of focusable elements
- Handles Up/Down/Tab navigation

### Select Module (~60 LOC)
- Dropdown/combobox
- State: { Options: string list; Selected: int }
- View: shows current selection, expands on Enter

### Login Form Example (~150 LOC)
- Composes TextInput + FocusRing
- Shows pattern for multi-field form

### Dashboard Example (~200 LOC)
- 3 concurrent async streams (API calls, timers, file watcher)
- Shows OfCancellableAsync pattern
- Demonstrates subscription lifecycle management

### Constraint Solver Tests (~FsCheck properties)
- Element algebra properties (identity, associativity)
- Style monoid laws
- Layout bounds verification
- Buffer diff correctness

---

## 15. WHAT'S NOT BEING BUILT (v0)

Explicitly deferred by the expert panel:

| Feature | Why Deferred |
|---------|-------------|
| Middleware system | Patterns haven't stabilized; v1 when real extensions exist |
| FSharp.Data.Adaptive | Optional package; core works without reactive bindings |
| Actor-based runtime | MailboxProcessor ejected; simple loop is sufficient |
| crossterm bindings | Interop hell; own the ANSI instead |
| Widget library | Community builds this on top; ship examples, not widgets |
| CSS-style layout | Universal rejection; rectangle splitting is sufficient |
| Multi-target (Fable/WASM) | Target .NET; portable design, not polyglot runtime |
| Custom operators | Optional module; named functions are primary API |
| Theming system | Just named styles; darkMode = Style.mapFg invert suffices |
| Scrollable element | v1; requires scroll state in model |
| Focus ring (mandatory) | v1; adds state management complexity |
| Mouse support (primary) | v1; keyboard-first is correct for v0 |

---

## SUMMARY: KEY ARCHITECTURAL DECISIONS

| Decision | Rationale | Alternative Rejected |
|----------|-----------|---------------------|
| **Element DU (9 cases)** | Algebraic, composable, pattern-matched | Renderable function (less inspectable) |
| **Elm Architecture** | Pure state transitions, testable, serializable | Raw event matching (implicit) |
| **Arena allocation** | Zero-GC frame loops, O(1) reset | GC every frame (simple but wasteful) |
| **PackedCell (16B blittable)** | SIMD diff via Vector128; Color DU pre-resolved to int32 | Color DU in cell (variable-size, breaks SIMD) |
| **SIMD diff chunk-skip** | 10-20× faster than cell-by-cell, JIT-vectorized | Naive comparison (slower) |
| **Mutable buffer** | Write-once-per-frame is not "shared mutable state" | Immutable (allocation pressure) |
| **Flat 1D buffer** | Cache locality, Span slicing, SIMD-friendly | 2D array (worse locality) |
| **Single-pass layout** | Terminals are small; constraints are simple | Multi-pass algebraic solver (overkill) |
| **Record of functions** | Idiomatic F#, composable via override | Interface (ceremony) |
| **Test backend public** | TDD for TUI apps | Internal testing only |
| **No animation framework** | User handles via subscriptions + Tick messages | Built-in tweening library (over-engineering) |
| **Own ANSI (not crossterm)** | 200 lines, no interop, debuggable | crossterm (opaque, platform abstraction hell) |

---

## NEXT STEPS FOR IMPLEMENTATION

1. **Phase 0: Foundation Types** (~250 LOC)
   - Color, BaseColor, Intensity
   - TextAttrs (bitmask)
   - Style (record + merge)
   - PackedColor (pack/unpack)
   - Area, Padding, BorderStyle
   - Constraint
   - Element DU
   - El module

2. **Phase 1: PackedCell + Buffer + SIMD Diff** (~350 LOC)
   - PackedCell struct
   - Buffer (create, get, set, writeString, clear)
   - SIMD diff (chunk-skip)
   - Property tests

3. **Phase 2: Frame Arena** (~200 LOC)
   - ElementNode, NodeHandle
   - FrameArena (bump allocator)
   - Arena.lower (DU → arena)

4. **Phase 3: ANSI Backend** (~200 LOC)
   - Ansi module (escape sequences)
   - Presenter (batched ANSI)
   - TerminalBackend (record + ansi + test implementations)

5. **Phase 4: Layout Engine** (~200 LOC)
   - Layout.solve (constraint solver)
   - Layout.splitH, Layout.splitV

6. **Phase 5: Renderer** (~300 LOC)
   - Render.render (Element → PackedCell writes)
   - Border rendering
   - Style inheritance

7. **Phase 6: Elm Runtime + Concurrency** (~300 LOC)
   - TerminalEvent, Key, Modifiers
   - Cmd<'msg>, Sub<'msg>
   - Program<'model, 'msg>
   - App.run (render loop with ConcurrentQueue dispatch)

8. **Phase 7: Examples + Benchmarks** (~500 LOC)
   - Counter, TextInput, FocusRing, Select
   - Login form, Dashboard
   - BenchmarkDotNet harness

**Total: ~1,800 LOC of core library**

