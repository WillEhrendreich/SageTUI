# SageTUI F# TUI Library: API Ergonomics Analysis

## Executive Summary
SageTUI is a **production-ready Elm Architecture TUI framework** for F# with exceptional API ergonomics, zero-allocation rendering, and sophisticated layout semantics. The design is remarkably composable and closely mirrors Elm/Textual patterns while adding terminal-specific conveniences.

---

## 1. APP MODULE: Creating & Running Apps

### API Surface
**File:** App.fs (lines 19-284)

#### Main Entry Points
\\\sharp
App.run : Program<'model, 'msg> -> unit              // Main entry point, auto-detects backend
App.runWithBackend : TerminalBackend -> Program<'model, 'msg> -> unit  // Explicit backend
App.runWith : AppConfig -> TerminalBackend -> Program<'model, 'msg> -> unit  // Full control

App.simple : init -> update -> view -> Program<'model, 'msg>  // Skip subscriptions
App.display : (unit -> Element) -> unit              // Static view, Esc to quit

Backend.auto () : TerminalBackend                    // Zero-ceremony backend creation
Backend.create : TerminalProfile -> TerminalBackend  // Explicit profile
\\\

### Ergonomics Score: **9/10**

**Strengths:**
- ✅ **Minimal ceremony** — \App.run program\ is sufficient for 99% of use cases
- ✅ **Backend auto-detection** — Environment inspection + OS detection in one call
- ✅ **Layered control** — Start simple, add complexity only when needed
- ✅ **App.simple** convenience reducer — Skip subscription infrastructure when unused
- ✅ **Static display mode** — Perfect for dashboards/info panels

**Rough Edges:**
- ⚠️ **No error recovery outside app loop** — Exceptions during setup aren't caught (though the try-catch at line 253 protects terminal state inside the loop)
- ⚠️ **Backend creation requires profile knowledge** — Users need to understand TerminalProfile structure if not using auto-detection

### Configuration
\\\sharp
type AppConfig =
  { ArenaNodes: int    // Pre-allocate space for element tree nodes (default: 4096)
    ArenaChars: int    // Pre-allocate text buffer (default: 65536)
    ArenaLayout: int }  // Pre-allocate layout scratch space (default: 4096)
\\\

**How it works:** Arena allocation is one-shot during init; reset happens at frame end. Excellent for predictable memory footprint.

---

## 2. EL MODULE: Element Constructors & Composition

### API Surface
**File:** Element.fs (lines 54-267)

#### Core Constructors
\\\sharp
// Primitives
El.empty : Element
El.text : string -> Element
El.styledText : Style -> string -> Element

// Layout
El.row : Element list -> Element          // Horizontal flex
El.column : Element list -> Element       // Vertical flex
El.overlay : Element list -> Element      // Z-order layering

// Styling (chainable, composable)
El.styled : Style -> Element -> Element
El.fg : Color -> Element -> Element       // Foreground color
El.bg : Color -> Element -> Element       // Background color
El.bold, El.dim, El.italic, El.underline, El.reverse, El.strikethrough : Element -> Element

// Constraints
El.width, El.minWidth, El.maxWidth : int -> Element -> Element
El.height, El.minHeight, El.maxHeight : int -> Element -> Element
El.fill : Element -> Element              // Flex: 1 1 auto
El.percentage : int -> Element -> Element
El.ratio : int -> int -> Element -> Element

// Box Model
El.padded : Padding -> Element -> Element
El.padAll, El.padHV : int -> int -> Element -> Element

// Borders
El.bordered : BorderStyle -> Element -> Element
El.border : Element -> Element            // Light border

// Alignment
El.align : HAlign -> VAlign -> Element -> Element
El.center, El.alignLeft, El.alignRight, El.alignTop, El.alignMiddle, El.alignBottom, El.alignBottomRight

// Text utilities
El.textf : Printf.StringFormat<'a> -> 'a  // Formatted text
El.paragraph, El.paragraphStyled : int -> string -> Element  // Word-wrapped

// Canvas
El.canvas, El.canvasBraille, El.canvasWithMode : (int -> int -> PixelBuffer) -> Element

// Memoization
El.lazy' : ('a -> Element) -> 'a -> Element
El.lazy2 : ('a -> 'b -> Element) -> 'a -> 'b -> Element

// Transitions
El.keyed : string -> Element -> Element
El.onEnter, El.onExit, El.transition : Transition -> Element -> Element
\\\

### Ergonomics Score: **9/10** (Same as Elm/Ratatui, with terminal-specific additions)

**Strengths:**
- ✅ **Piping-friendly** — Every function takes element as last argument
- ✅ **Chainable styling** — El.text "hi" |> El.bold |> El.fg Color.red is natural
- ✅ **Memoization built-in** — \El.lazy'\ prevents unnecessary re-renders
- ✅ **Terminal-aware** — Padding, borders, alignment understand terminal cells
- ✅ **Transitional elements** — \El.keyed\ + \El.transition\ for animations is elegant

**Rough Edges:**
- ⚠️ **Padding model isn't symmetric** — El.padHV h v applies to all sides, but to apply different top/bottom requires Padded directly
- ⚠️ **Height constraint wraps in Column** — El.height n wraps element in Column [Constrained(Fixed n, elem)] which can surprise users
- ⚠️ **Gap applied to Gapped wrapper, not Row/Column** — El.gap 2 (El.row [...]) works, but feels less integrated than CSS
- ⚠️ **No semantic/grid layout** — Unlike Ratatui, no named grid areas (e.g., "header | sidebar | main")

### Example: Composable Form
\\\sharp
El.column [
  El.text "Name" |> El.fg Theme.Primary |> El.bold
  TextInput.view isFocused model.Name
    |> El.bordered (if isFocused then Rounded else Light)
    |> El.padAll 1
  El.text (errorMsg |> Option.defaultValue "") |> El.fg Theme.Error
]
\\\

---

## 3. LAYOUT MODULE: Flexbox Engine

### API Surface
**File:** Layout.fs (lines 29-239)

#### Core API
\\\sharp
type Constraint =
  | Fixed of int               // Exact width
  | Min of int                 // Minimum (grows to fill)
  | Max of int                 // Maximum (shrinks if needed)
  | Percentage of int          // % of available
  | Fill                       // Flex: 1 (equal distribution)
  | Ratio of int * int         // num/den of available

// Solvers
Layout.solve : int -> Constraint list -> (int * int) list
Layout.solveWithContent : int -> Constraint list -> int list -> (int * int) list
Layout.solveWithGap : int -> int -> Constraint list -> int list -> (int * int) list

// Splitters (produce Areas)
Layout.splitH : Constraint list -> Area -> Area list
Layout.splitV : Constraint list -> Area -> Area list
Layout.splitHWithContent : Constraint list -> int list -> Area -> Area list
Layout.splitVWithContent : Constraint list -> int list -> Area -> Area list
Layout.splitHWithGap, Layout.splitVWithGap : int -> Constraint list -> int list -> Area -> Area list

// Alignment
Layout.alignArea : HAlign -> VAlign -> contentW -> contentH -> Area -> Area
Layout.applyConstraint : Constraint -> Area -> Area
Layout.shrinkForBorder : Area -> Area
Layout.intersectArea : Area -> Area -> Area  // Clipping
\\\

### Ergonomics Score: **8.5/10**

**Strengths:**
- ✅ **CSS vocabulary** — Developers familiar with flexbox immediately recognize Fill, Percentage, Ratio
- ✅ **Content-aware** — solveWithContent respects intrinsic sizes (e.g., text width) before distributing space
- ✅ **Proportional shrink** — Unlike naive flexbox, shrinking is proportional when space is insufficient (line 134-140)
- ✅ **Single-pass** — All constraint solving happens in O(n), no iterations
- ✅ **Gap support** — Configurable spacing between children

**Rough Edges:**
- ⚠️ **No aspect ratio constraints** — Can't lock 1:1 aspect (e.g., for aspect-ratio-aware canvases)
- ⚠️ **No justify/align-content** — Children can't "spread" with space-between/space-around
- ⚠️ **Manual area coordinate math** — Users rarely interact with Layout directly; it's handled by Arena rendering
- ⚠️ **No grid-template** — Unlike CSS Grid, can't name areas or use complex 2D layout patterns

### Comparison to CSS Flexbox
| Feature | SageTUI | CSS Flexbox |
|---------|---------|-----------|
| Fixed | ✅ | ✅ |
| Min/Max | ✅ | ✅ (min-width, max-width) |
| Flex-grow | ✅ (Fill) | ✅ |
| Flex-basis: auto | ✅ (solveWithContent) | ✅ |
| Flex-shrink (proportional) | ✅ | ✅ |
| Gap | ✅ | ✅ |
| Justify-content | ❌ | ✅ |
| Align-items | ❌ (only align wrapper) | ✅ |

---

## 4. STYLING & ATTRIBUTES SYSTEM

### API Surface
**File:** Color.fs (entire) + Element.fs (lines 71-101)

#### Colors
\\\sharp
type Color =
  | Default                                 // Terminal default (no override)
  | Named of BaseColor * Intensity          // 16 named: {Black..White} × {Normal|Bright}
  | Ansi256 of byte                         // 256-color palette
  | Rgb of byte * byte * byte               // 24-bit TrueColor

module Color =
  let black, red, green, ..., brightCyan, brightWhite : Color
  let rgb : byte -> byte -> byte -> Color
  let hex : string -> Color                 // "#ff0000" → Rgb(255, 0, 0)
\\\

#### Text Attributes
\\\sharp
type TextAttrs = { Value: uint16 }  // Bitflag: Bold|Dim|Italic|Underline|Blink|Reverse|Hidden|Strikethrough

TextAttrs.bold, TextAttrs.dim, TextAttrs.italic, TextAttrs.underline, TextAttrs.reverse, TextAttrs.strikethrough
TextAttrs.combine : TextAttrs -> TextAttrs -> TextAttrs  // Bitwise OR
\\\

#### Style
\\\sharp
type Style = { Fg: Color option; Bg: Color option; Attrs: TextAttrs }

module Style =
  let empty : Style
  let merge : Style -> Style -> Style          // Overlay merges, underlay fills blanks
  let withFg, withBg : Color -> Style -> Style
  let withBold, withItalic, withUnderline : Style -> Style
\\\

### Ergonomics Score: **9/10**

**Strengths:**
- ✅ **Full color spectrum** — Default (fallback) → 16 named → 256 palette → true 24-bit RGB
- ✅ **Graceful degradation** — Terminal can downgrade Rgb to Indexed256 or Basic16
- ✅ **Hex color literals** — \Color.hex "#ff0000"\ is discoverable
- ✅ **Attribute composition** — Combining bold+italic+underline is straightforward with bitflags
- ✅ **Style merging** — Overlay (more specific) shadows underlay for clean inheritance

**Rough Edges:**
- ⚠️ **No semantic colors** — Can't use theme-relative colors like "primary", "error" (workaround: Theme helpers)
- ⚠️ **Attributes bitflag not exposed** — TextAttrs.Value is public but opaque; can't pattern-match on individual flags
- ⚠️ **No gradient API** — Gradients require manual Seq.mapi (though Effects.fs provides Gradient helpers)

### Theme System
**File:** Widgets.fs (lines 1-110)

\\\sharp
type Theme =
  { Primary: Color; Secondary: Color; Accent: Color
    Success: Color; Warning: Color; Error: Color
    TextFg: Color; TextDim: Color; Background: Color
    Border: BorderStyle }

module Theme =
  let dark : Theme       // Cyan/Blue/Magenta palette on black
  let light : Theme      // Blue/Cyan/Magenta palette on white
  let nord : Theme       // Nord palette
  let dracula : Theme    // Dracula palette
  let catppuccin : Theme // Catppuccin palette

  let apply : Theme -> Element -> Element  // Apply theme fg/bg
  let heading, subheading, success, warning, error : Theme -> string -> Element
  let panel : Theme -> title -> Element -> Element  // Themed bordered box
\\\

### Theme Ergonomics Score: **8/10**

**Strengths:**
- ✅ **Pre-built themes** — 5 carefully chosen color schemes (dark, light, nord, dracula, catppuccin)
- ✅ **Semantic slots** — Primary/Secondary/Accent/Success/Warning/Error cover 90% of UI needs
- ✅ **One-liner application** — Theme.apply theme elem sets fg/bg globally

**Rough Edges:**
- ⚠️ **No theme composition** — Can't layer themes or override individual colors
- ⚠️ **Limited slots** — No Info/Notice/Debug slots (workaround: use theme colors as literals)
- ⚠️ **Theme.apply is global** — Applies to entire tree; no way to apply to subtree
- ⚠️ **No dynamic theme switching** — Would require embedding theme in Model

---

## 5. WIDGET MODULES

### Available Widgets
**File:** Widgets.fs (entire)

#### TextInput
\\\sharp
type TextInputModel = { Text: string; Cursor: int }

module TextInput =
  let empty, ofString : TextInputModel
  let insertText : string -> TextInputModel -> TextInputModel
  let handleKey : Key -> TextInputModel -> TextInputModel  // Left/Right/Home/End/Backspace/Delete
  let handlePaste : string -> TextInputModel -> TextInputModel
  let view : focused: bool -> TextInputModel -> Element
  let viewWithPlaceholder : placeholder: string -> focused: bool -> TextInputModel -> Element
\\\

**Ergonomics:** ✅ Excellent. Simple state machine, composable with application logic.

#### Select (Dropdown)
\\\sharp
type SelectModel<'a> = { Options: 'a list; Selected: int; IsOpen: bool }

module Select =
  let create : 'a list -> SelectModel<'a>
  let toggle, moveUp, moveDown, confirm : SelectModel<'a> -> SelectModel<'a>
  let selectedValue : SelectModel<'a> -> 'a option
  let view : toString: ('a -> string) -> focused: bool -> SelectModel<'a> -> Element
\\\

**Ergonomics:** ✅ Good. Generic over 'a, requires ToString for display.

#### ProgressBar
\\\sharp
type ProgressBarConfig =
  { Percent: float; Width: int; FilledChar: char; EmptyChar: char
    FilledColor, EmptyColor: Color option; ShowLabel: bool }

module ProgressBar =
  let defaults : ProgressBarConfig  // 20 width, █/░, labels on
  let view : ProgressBarConfig -> Element
\\\

**Ergonomics:** ✅ Very good. Config record allows customization without overloading.

#### Tabs
\\\sharp
type TabsConfig<'a> =
  { Items: 'a list; ActiveIndex: int; ToString: 'a -> string
    ActiveColor, InactiveColor: Color option }

module Tabs =
  let view : TabsConfig<'a> -> Element
\\\

**Ergonomics:** ✅ Good. Stateless view function.

#### Table
\\\sharp
type TableColumn<'a> = { Header: string; Width: int; Render: 'a -> Element }

module Table =
  let view : columns: TableColumn<'a> list -> rows: 'a list -> selectedRow: int option -> Element
\\\

**Ergonomics:** ⚠️ Functional but rigid. No sorting, no horizontal scroll, columns must have fixed width.

#### Modal
\\\sharp
type ModalConfig =
  { Backdrop: Color option; BorderStyle: BorderStyle; MaxWidth, MaxHeight: int option }

module Modal =
  let defaults, view, simple : ModalConfig -> Element -> Element
\\\

**Ergonomics:** ✅ Good. Centered, with optional backdrop and size constraints.

#### TreeView
\\\sharp
type TreeNode<'a> = Leaf of 'a | Branch of 'a * children: TreeNode<'a> list
type TreeState = { Expanded: Set<int list>; Cursor: int list }

module TreeView =
  let init : unit -> TreeState
  let toggleExpand, expand, collapse : int list -> TreeState -> TreeState
  let expandAll : TreeNode<'a> list -> TreeState -> TreeState
  let moveCursorUp, moveCursorDown, handleKey : Key -> TreeNode<'a> list -> TreeState -> TreeState
  let view : toString: ('a -> string) -> focused: bool -> TreeNode<'a> list -> TreeState -> Element
\\\

**Ergonomics:** ✅ Excellent. Full keyboard navigation, expand/collapse state is separate from data.

#### Checkbox & Toggle
\\\sharp
module Checkbox =
  let toggle : bool -> bool
  let view : label: string -> focused: bool -> checked: bool -> Element

module Toggle =
  let toggle : bool -> bool
  let view : onLabel: string -> offLabel: string -> focused: bool -> value: bool -> Element
\\\

**Ergonomics:** ✅ Simple and stateless.

#### RadioGroup
\\\sharp
type RadioGroupModel<'a> = { Options: 'a list; Selected: int }

module RadioGroup =
  let create, moveUp, moveDown : RadioGroupModel<'a> -> RadioGroupModel<'a>
  let selectedValue : RadioGroupModel<'a> -> 'a option
  let handleKey : Key -> RadioGroupModel<'a> -> RadioGroupModel<'a>
  let view : toString: ('a -> string) -> focused: bool -> RadioGroupModel<'a> -> Element
\\\

**Ergonomics:** ✅ Good. Similar to Select but exclusive (radio) semantic.

#### SpinnerWidget, Toast, Form
\\\sharp
module SpinnerWidget =
  let view : tick: int -> Element                    // Braille spinner
  let viewDots, viewBars : tick: int -> Element

module Toast =
  type ToastModel = { Message: string; RemainingTicks: int; Style: Style }
  let create, createStyled : message: string -> ticks: int -> Style -> ToastModel
  let tick : ToastModel -> ToastModel option        // Returns None when expired
  let isExpired : ToastModel -> bool
  let view : ToastModel -> Element

module Form =
  type FormField<'model, 'msg> = { Key: string; View: bool -> 'model -> Element; HandleKey: Key -> 'model -> 'msg option }
  let field, view, handleKey, handleFocus : ...
\\\

**Ergonomics:** ✅ Excellent. Toast uses Option to signal expiration. Form composes field definitions.

### FocusRing (Focus Management)
\\\sharp
type FocusRing<'a> = { Items: 'a list; Index: int }

module FocusRing =
  let create : 'a list -> FocusRing<'a>
  let current : FocusRing<'a> -> 'a option
  let next, prev : FocusRing<'a> -> FocusRing<'a>
  let isFocused : 'a -> FocusRing<'a> -> bool
\\\

**Ergonomics:** ✅ Excellent. Cyclic, generic, easy to integrate into forms.

### Widget Ergonomics Summary: **8.5/10**

**Strengths:**
- ✅ No global state — All widgets are data structures + pure functions
- ✅ Composable — Can nest widgets arbitrarily (e.g., TreeView inside Modal)
- ✅ Generic where sensible — Select<'a>, RadioGroup<'a>, TreeNode<'a>
- ✅ Focus management — FocusRing makes Tab navigation trivial
- ✅ Variety — Covers TextInput, Select, Table, TreeView, Form, Toast, Modal

**Rough Edges:**
- ⚠️ **No built-in validation** — Validation logic lives in application (design choice, arguably correct)
- ⚠️ **Table is basic** — No sorting, no horizontal scroll, no cell selection
- ⚠️ **No combo-box** — Can't have a filterable Select
- ⚠️ **No slider/stepper** — Numeric input requires TextInput + parsing
- ⚠️ **Modal is overlay-only** — No modal dialog with form inside (workaround: compose manually)

---

## 6. SUBSCRIPTION SYSTEM

### API Surface
**File:** Tea.fs (lines 44-64)

\\\sharp
type Sub<'msg> =
  | KeySub of (Key * Modifiers -> 'msg option)
  | MouseSub of (MouseEvent -> 'msg option)
  | ClickSub of (MouseEvent * string option -> 'msg option)
  | FocusSub of (FocusDirection -> 'msg option)
  | TimerSub of id: string * interval: TimeSpan * tick: (unit -> 'msg)
  | ResizeSub of (int * int -> 'msg)
  | CustomSub of id: string * start: (('msg -> unit) -> CancellationToken -> Async<unit>)

and FocusDirection = FocusNext | FocusPrev
\\\

### Subscriptions in App Loop
**File:** App.fs (lines 70-102)

- **KeySub** — Called per key press, can return None to ignore
- **MouseSub** — All mouse events (move, click, scroll)
- **ClickSub** — Hit-tested clicks with Z-order (gets keyed element if it was hit)
- **FocusSub** — Tab/Shift+Tab navigation (automatic via Focus.focusSub wrapper)
- **TimerSub** — Interval-based ticks, automatically reconciled (started/stopped based on subscription list)
- **ResizeSub** — Terminal resize events
- **CustomSub** — User-defined async subscription (e.g., HTTP requests, file watchers)

### Ergonomics Score: **9/10**

**Strengths:**
- ✅ **Automatic subscription lifecycle** — Subs are reconciled per model change; starting/stopping is automatic
- ✅ **Cancellation built-in** — CustomSub receives CancellationToken for cleanup
- ✅ **Optional dispatch** — Returning None filters out messages
- ✅ **Hit-tested clicks** — ClickSub automatically provides which keyed element was hit (Z-order aware)
- ✅ **Timer IDs** — Multiple timers can coexist by ID, allowing independent control

**Rough Edges:**
- ⚠️ **No cleanup hook per subscription** — If CustomSub needs setup/teardown, it's user's responsibility
- ⚠️ **MouseSub receives all events** — No built-in filtering (e.g., "only on element X")
- ⚠️ **No paste event** — Line 227 shows KeySub gets Paste events, but they're not in Sub enum
- ⚠️ **No time-based events** — Can't subscribe to "once per second" differently than "on every frame"

### Example: Timers + Key Events
\\\sharp
type Msg = Tick | KeyInput of Key * Modifiers | Quit

let subscribe model =
  [ KeySub (fun (k, m) -> match k with Escape -> Some Quit | _ -> Some (KeyInput(k, m)))
    TimerSub("anim", TimeSpan.FromMilliseconds(16.0), fun () -> Tick)  // 60fps
    ResizeSub (fun (w, h) -> Resized(w, h))
    match model.Loading with
    | true -> CustomSub("fetch", fun dispatch ct ->
        async {
          try
            let! result = httpClient.GetAsync(url)
            dispatch (FetchComplete result)
          with :? OperationCanceledException -> ()
        })
    | false -> () ]
\\\

---

## 7. TRANSITION & ANIMATION SYSTEM

### API Surface
**File:** Transition.fs + Element.fs (lines 27-35, 136-156)

\\\sharp
[<Measure>] type ms

type Transition =
  | Fade of duration: int<ms>
  | ColorMorph of duration: int<ms>
  | Wipe of direction: Direction * duration: int<ms>
  | SlideIn of direction: Direction * duration: int<ms>
  | Dissolve of duration: int<ms>
  | Grow of duration: int<ms>
  | Sequence of Transition list
  | Custom of (float -> int -> int -> int)

type Direction = Left | Right | Up | Down

module El =
  let keyed : string -> Element -> Element                  // Tag for tracking
  let onEnter : Transition -> Element -> Element           // Enter animation
  let onExit : Transition -> Element -> Element            // Exit animation
  let transition : Transition -> Element -> Element        // Both enter & exit
  let viewTransition : string -> Element -> Element        // ColorMorph enter, Fade exit
\\\

### How It Works (App.fs lines 129-201)
1. **Reconciliation** — Finds keyed elements from old and new trees
2. **Entering elements** — Start with empty snapshot, apply enter transition
3. **Exiting elements** — Capture current screen snapshot, apply exit transition
4. **Rendering** — Each frame applies active transitions with easing (cubicInOut by default)
5. **Cleanup** — Remove completed transitions

### Ergonomics Score: **8.5/10**

**Strengths:**
- ✅ **No manual animation state** — Framework handles timing and easing
- ✅ **Snapshot-based** — Transitions preserve visual state without re-rendering
- ✅ **Sequenceable** — Sequence [Fade; ColorMorph] chains multiple effects
- ✅ **Keyed tracking** — El.keyed "card-1" is all you need for identification
- ✅ **Full-screen transitions** — Works seamlessly with element replacement

**Rough Edges:**
- ⚠️ **Full-screen only** — Transitions capture entire frame, not scoped regions (line 131 TODO comment)
- ⚠️ **No custom easing per transition** — Always cubicInOut (could be Transition parameter)
- ⚠️ **Custom transition opaque** — Custom (float -> int -> int -> int) signature is unclear
- ⚠️ **Interpolation basic** — ColorMorph uses OKLCH which is perceptually good, but only for Rgb colors
- ⚠️ **No animation frames** — Can't subscribe to "animation finished" event
- ⚠️ **No simultaneous animations** — Only one enter+exit per keyed element

### Example: Card Transitions
\\\sharp
let cardView idx card =
  card
  |> El.keyed (sprintf "card-%d" idx)
  |> El.onEnter (Fade 300<ms>)
  |> El.onExit (Wipe(Left, 500<ms>))
  |> El.transition (Sequence [Fade 200<ms>; ColorMorph 300<ms>])  // Both enter & exit

// In view function, cards automatically animate in/out on list changes
\\\

---

## 8. CANVAS MODULE: Braille & HalfBlock Graphics

### API Surface
**File:** Element.fs (lines 3-18, 172-180) + Effects.fs (lines 16-193)

\\\sharp
type CanvasMode = Braille | HalfBlock | PixelProtocol

type PixelBuffer = { Width: int; Height: int; Pixels: Color array }

type CanvasConfig = { Draw: int -> int -> PixelBuffer; Mode: CanvasMode; Fallback: CanvasMode option }

module El =
  let canvas : (int -> int -> PixelBuffer) -> Element              // HalfBlock (fallback preferred)
  let canvasBraille : (int -> int -> PixelBuffer) -> Element       // Braille dots
  let canvasWithMode : CanvasMode -> (int -> int -> PixelBuffer) -> Element

// Rendering helpers
module Braille =
  let dotAt : col: int -> row: int -> int
  let toChar : bits: int -> char

module Gradient =
  let lerpRgb, oklch : startColor -> endColor -> width -> text -> Element  // Color gradients
  let hueToRgb : hue: float -> byte * byte * byte
  let rainbow : width: int -> text: string -> Element             // Hue gradient
\\\

### Canvas Rendering Pipeline
1. **Draw callback** invoked with terminal (w, h)
2. **User returns PixelBuffer** with width, height, and color array
3. **Canvas mode determines encoding**:
   - **HalfBlock** — 2 pixels per cell (top/bottom half, ▀/▄)
   - **Braille** — 8 pixels per cell (⠿ glyph, 2 columns × 4 rows)
   - **PixelProtocol** — Kitty/Sixel graphics (stub for future)

### Ergonomics Score: **7.5/10**

**Strengths:**
- ✅ **Declarative** — Draw callback is pure, framework handles timing
- ✅ **Multiple modes** — Braille for fine-grained, HalfBlock for blocky
- ✅ **Gradient helpers** — Rainbow, oklch lerp eliminate boilerplate
- ✅ **Color-aware** — Full RGB support within canvas cells

**Rough Edges:**
- ⚠️ **Manual pixel layout** — Must understand HalfBlock stacking and Braille bit ordering
- ⚠️ **No drawing primitives** — No lines, circles, text rendering (user must calculate)
- ⚠️ **Pixel buffer is full color** — Can't use sparse/sparse approaches (always allocates width×height)
- ⚠️ **Fallback mode optional** — If terminal doesn't support chosen mode, rendering fails (Fallback=None)

### Example: Braille Canvas
\\\sharp
let draw w h =
  let pixels = Array.create (w * h) Color.Default
  for y in 0 .. h - 1 do
    for x in 0 .. w - 1 do
      let idx = y * w + x
      pixels.[idx] <- Color.Rgb(byte (x * 255 / w), byte (y * 255 / h), 128uy)
  { Width = w; Height = h; Pixels = pixels }

El.canvasBraille draw
|> El.width 80
|> El.height 24
\\\

---

## 9. ERROR HANDLING & RECOVERY

### Current State
**File:** App.fs (lines 253-257)

\\\sharp
try
  // ... main loop
with ex ->
  backend.Write(Ansi.showCursor + Ansi.leaveAltScreen)  // Restore cursor, exit alt-screen
  backend.Flush()
  backend.LeaveRawMode()                                 // Restore terminal mode
  raise ex                                               // Re-throw for debugging
\\\

### Ergonomics Score: **7/10**

**Strengths:**
- ✅ **Terminal always restored** — Even on panic, cursor/mode/alt-screen are recovered
- ✅ **No corrupted state** — Terminal is guaranteed usable after crash
- ✅ **Exception bubbles** — Caller can handle with outer try/catch if desired

**Rough Edges:**
- ⚠️ **No error boundary for user code** — If View throws, it crashes the app
- ⚠️ **No recovery mechanism** — Can't continue after exception (all-or-nothing)
- ⚠️ **No error logging** — Exception details go to stderr, not captured
- ⚠️ **No graceful degradation** — If rendering fails, whole frame is lost
- ⚠️ **No Cmd.ofAsync error handling** — Async exceptions are eprintfn'd (line 44) but don't dispatch

### Workaround: User-Level Error Boundary
\\\sharp
let view model =
  try
    renderNormalView model
  with ex ->
    El.text (sprintf "Render error: %s" ex.Message)
    |> El.fg Color.red |> El.padAll 1

// Or in Update:
let update msg model =
  try
    handleNormalMessage msg model
  with ex ->
    { model with Error = Some ex.Message }, Cmd.none
\\\

---

## 10. QUICK REFERENCE: API PATTERNS

### The TEA Boilerplate
\\\sharp
// Minimal program
type Model = { Count: int }
type Msg = Increment | Decrement | Quit

let init () = { Count = 0 }, Cmd.none

let update msg model =
  match msg with
  | Increment -> { model with Count = model.Count + 1 }, Cmd.none
  | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
  | Quit -> model, Cmd.quit

let view model =
  El.column [
    El.text (sprintf "Count: %d" model.Count)
    El.text "[+] [−] [q]" |> El.dim
  ]

let subscribe _ =
  [ KeySub (fun (k, _) ->
      match k with
      | Char '+' -> Some Increment
      | Char '-' -> Some Decrement
      | Char 'q' | Escape -> Some Quit
      | _ -> None) ]

[<EntryPoint>]
let main _ =
  { Init = init; Update = update; View = view; Subscribe = subscribe }
  |> App.run
  0
\\\

### Styling Composition
\\\sharp
// Chainable
El.text "Hello"
|> El.bold
|> El.fg (Color.Rgb(255uy, 100uy, 50uy))
|> El.bg (Color.Named(Blue, Bright))
|> El.underline
|> El.padAll 2
|> El.bordered Rounded

// Or with Style record
let headingStyle = { Fg = Some Color.cyan; Bg = None; Attrs = TextAttrs.bold }
El.styledText headingStyle "Heading"
\\\

### Layout Combinators
\\\sharp
// Flexbox-style
El.row [
  El.text "Left" |> El.width 20
  El.text "Center" |> El.fill
  El.text "Right" |> El.width 10
]

// With gap
El.column [
  El.text "A"
  El.text "B"
  El.text "C"
] |> El.gap 1

// Nested with alignment
El.column [
  El.text "Title" |> El.bold |> El.center
  El.row [
    El.column [...]  // Left sidebar
    El.text " " |> El.width 1
    El.column [...] |> El.fill  // Main content
  ] |> El.fill
] |> El.padAll 1 |> El.bordered Rounded
\\\

---

## 11. COMPARISON MATRIX: Elm vs Ratatui vs Textual vs SageTUI

| Feature | Elm | Ratatui | Textual | SageTUI |
|---------|-----|---------|---------|----------|
| **Architecture** | TEA | Custom | Reactive | TEA |
| **Language** | Elm | Rust | Python | F# |
| **Layout Engine** | CSS-like | Crossterm | Reactive | CSS-like |
| **Flexbox** | ✅ Fill/Min/Max | ✅ Constraints | ✅ Dock/Container | ✅ Fill/Min/Max/Ratio |
| **Animations** | Subscriptions | Manual | Native | Keyed transitions |
| **Widgets** | Community | Built-in | Built-in | Built-in (15+) |
| **Canvas Graphics** | ❌ | ⚠️ (text-based) | ✅ Canvas API | ✅ Braille/HalfBlock |
| **Colors** | 16 named | 16 named + Indexed256 + RGB | Full RGB | 16 named + Indexed256 + RGB |
| **Hit Testing** | ❌ | ❌ | ✅ | ✅ (Z-order aware) |
| **Theme System** | ❌ | ⚠️ (manual) | ✅ Built-in | ✅ 5 themes |
| **Performance** | JS-based | Ultra-fast | Good | Zero-GC frame loop, SIMD diff |
| **Learning Curve** | Easy (if know Elm) | Moderate | Moderate | Easy (TEA is familiar) |
| **Error Recovery** | N/A | N/A | Limited | Terminal always restored |
| **HTML Rendering** | ❌ | ❌ | ✅ (limited) | ✅ Full HTML bridge |

---

## 12. OVERALL API ERGONOMICS SCORE

### By Category:
- **App Initialization:** 9/10 (simplicity is exceptional)
- **Element Construction:** 9/10 (piping, composability)
- **Layout Engine:** 8.5/10 (CSS vocabulary, content-aware, slight gaps in justify/align)
- **Styling:** 9/10 (full color space, themes, attributes)
- **Widgets:** 8.5/10 (rich suite, mostly stateless, Table could be better)
- **Subscriptions:** 9/10 (automatic lifecycle, optional dispatch, hit-testing)
- **Transitions:** 8.5/10 (elegant keyed approach, limited to full-screen)
- **Canvas:** 7.5/10 (functional but manual, no primitives)
- **Error Handling:** 7/10 (terminal always safe, but no user-level recovery)

### **Overall: 8.3/10**

---

## 13. RECOMMENDATIONS FOR USERS

### What SageTUI Excels At:
✅ **Interactive forms** — TextInput, Select, TreeView, Form composition shine  
✅ **Dashboards** — Layout engine + Theme system are production-ready  
✅ **Terminal UIs with animations** — Keyed transitions are elegant  
✅ **Low-latency apps** — Zero-GC frame loop, SIMD diff beat competitors  
✅ **Rich styling** — Full RGB + themes + text attributes  
✅ **F# integration** — Functional-first, immutable state, type-safe  

### Gaps / Workarounds:
⚠️ **Complex tables** — Use TreeView or manual layout for sorting/scrolling  
⚠️ **Scoped animations** — Only full-screen transitions; compose elements to fake scoped  
⚠️ **Custom widgets** — Build as Element builders (e.g., let customButton label = ...)  
⚠️ **Keyboard shortcuts** — Use KeySub with Modifiers flags to detect Ctrl/Alt/Shift  
⚠️ **Aspect ratio** — Use Ratio constraint for 2:1 or 1:2 aspect (e.g., El.ratio 2 1)  

### Best Practices:
1. **Use FocusRing for Tab navigation** — Much simpler than manual focus management
2. **Separate business logic from View** — Keep model/update pure, render in view function
3. **Memoize expensive views** — Use El.lazy' for subtrees that don't change often
4. **Use keyed elements for lists** — Enables animations on add/remove
5. **Leverage Theme system** — Define once, use everywhere for consistency
6. **Test rendering independently** — Use TestBackend to capture output without terminal

---

## 14. CONCLUSION

**SageTUI is a world-class F# TUI framework.** It brings Elm Architecture clarity to terminal development while adding sophisticated features like arena rendering, SIMD diffing, and keyed transitions. The API is **eminently discoverable and composable**, with few rough edges and an excellent widget suite.

For developers familiar with Elm, Elmish, or Fabulous, SageTUI feels like a natural extension into terminal space. The zero-allocation rendering philosophy is rare and valuable for latency-sensitive applications.

**Recommendation:** Excellent for production terminal UIs. Notable for sophisticated layouts, animations, and performance characteristics.

