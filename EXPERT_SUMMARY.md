# SageTUI Architecture: Executive Summary for Expert Panel

## Overview
SageTUI is a high-performance F# functional TUI library implementing:
- **Elm Architecture** (functional reactive program structure)
- **Flexbox layout** (content-aware constraint solver)
- **SIMD-accelerated rendering** (16-byte PackedCell, Vector128 comparisons)
- **Rich animations** (6 built-in transitions + custom effects)
- **Zero-allocation hot paths** (Arena-based serialization + scratch reuse)

---

## Key Architectural Insights

### 1. SIMD Diff Pipeline (Buffer.fs)
- **PackedCell:** 16-byte struct (Rune|Fg|Bg|Attrs|_pad) → Vector128<byte> aligned
- **Chunk-skip diff:** 256-byte chunks compared first; fine-grain only on mismatch
- **Result:** Only changed cells sent to terminal (typically 1-5% of buffer per frame)

### 2. Layout Algorithm (Layout.fs)
- **2-phase solver:** Non-Fill constraints first, then weighted Fill distribution
- **Hamilton method:** Sequential subtraction for proportional allocation (no rounding bias)
- **Content-aware:** Fill children measure intrinsic size, flex-grow on surplus
- **Gap support:** Injected as offsets post-layout (not reducing available space)

### 3. Rendering Strategy: Reference vs Arena

| Aspect | Render.fs | ArenaRender.fs |
|--------|-----------|----------------|
| **Allocation style** | Recursive + allocating | Pre-serialized + reusable scratch |
| **Text writes** | StringBuilder | Direct TextBuf span |
| **Child traversal** | List iteration | Linked-list (NextSibling) |
| **Layout storage** | List of (offset, size) tuples | Array slots (bump allocator) |
| **Performance** | Clear semantics | 2-3x faster (zero allocs) |

### 4. Element Serialization (Arena.fs)
- **ElementNode struct:** 28-byte packed representation (Kind, StylePacked, Constraint, FirstChild, NextSibling, DataStart, DataLen)
- **TextBuf:** Single char[] with offsets (no per-node allocation)
- **Canvas list:** Separate List<CanvasConfig> (avoid storing config in node)
- **Linked tree:** FirstChild + NextSibling (cache-friendly array traversal)

### 5. Transitions (Transition.fs)
- **6 built-in effects:** ColorMorph, Fade, Wipe, SlideIn, Dissolve, Grow
- **Dissolve pre-computation:** Fisher-Yates shuffle at transition start (stored in payload)
- **Sequence support:** Multi-phase transitions share original snapshot
- **Custom transitions:** User-supplied cell-level blending function

### 6. Color Encoding (Color.fs)
- **Packed int32:** Tag (2 bits) + variant-specific data (30 bits)
  - Tag 0: Default
  - Tag 1: Named (base 3 bits + intensity 1 bit)
  - Tag 2: Ansi256 (byte)
  - Tag 3: RGB (RRGGBB)
- **OKLCH interpolation:** Perceptual color space for smooth animations (ColorMorph transitions)

### 7. Pixel Rendering (CanvasRender.fs)
- **HalfBlock:** 2 pixels → 1 terminal cell (▀/▄ blocks; 2:1 vertical compression)
- **Braille:** 2×4 pixel grid → 1 terminal cell (⠀ to ⡿; 8 dot positions; 2:1 horiz, 4:1 vert)
- **Colorization:** Pixel color becomes cell foreground (terminal cell = one color only)

### 8. Testing Framework (Testing.fs)
- **Virtual time:** No real sleep; TestApp tracks elapsed TimeSpan
- **Delay capture:** Cmd.Delay commands queued in PendingDelays list
- **Deterministic playback:** advanceTime fires delays in causal order (with Seq tie-breaker)
- **Render snapshot:** TestHarness.render returns string for assertion

---

## Compilation Order Implications

F# compilation order strictly determines module visibility. The order is NOT arbitrary:

`
Color (foundation: Color DU, TextAttrs)
  ↓
Buffer (uses Color via PackedColor.pack)
  ↓
Layout (independent, no Color/Buffer deps)
  ↓
Element (uses Color, Layout)
  ↓
Measure (measures Element)
  ↓
Render (renders Element to Buffer, uses Measure & Layout)
  ↓
Arena (serializes Element)
  ↓
ArenaRender (renders ElementNode to Buffer, uses Arena)
  ↓
Tea (Elm arch, independent)
  ↓
App (orchestrates all)
`

This order ensures:
- Color definitions available before anything that needs PackedColor
- Buffer fully defined before rendering modules
- Element fully defined before serialization/rendering
- Tea protocol stable before App integration

---

## Performance Characteristics

### Hot Path Allocations
- ✅ **Zero allocations:** ArenaRender (scratch reuse), writeCharSpan (span-only)
- ❌ **Allocating paths:** Render.fs (reference impl), Measure (intrinsic sizing)

### SIMD Optimization Points
1. **Diff:** SequenceEqual on 256-byte chunks (JIT-vectorized .NET 8+)
2. **PackedCell:** 16-byte alignment for Vector128 comparisons
3. **TextBuf:** Reused across frames (single pre-allocated char array)

### Layout Cost
- O(children) per Row/Column (linear in children count)
- Content measurement: O(tree size) first frame, cached thereafter
- Constraint resolution: O(children) with Hamilton method

---

## Design Trade-offs

### Immutability vs Performance
- **Immutable:** Element, Cmd, Style, messages → easy testing & reasoning
- **Mutable:** Buffer (write-once), Arena scratch (reused), ActiveTransition → performance

### Reference Renderer vs Arena Renderer
- **Render.fs:** Semantically clear, slow (allocations, list ops)
- **ArenaRender.fs:** Optimized, complex (linked-list traversal, bit-packed nodes)
- **Both available:** Use Render for prototyping, ArenaRender in production

### Responsive Breakpoints
- **Responsive:** Width-based (minW breakpoints)
- **ResponsiveH:** Height-based (minH breakpoints)
- **Compose:** Use both for 2D adaptive layouts

---

## Critical Code Sections for Expert Review

### 1. SIMD Diff (Buffer.fs:108-129)
- How chunk-skip avoids scanning identical regions
- MemoryMarshal.AsBytes zero-copy cast
- Span.SequenceEqual vectorization

### 2. Constraint Solver (Layout.fs:33-99, 103-228)
- Hamilton method (sequential subtraction) for weighted distribution
- Content-aware flex (Min grows vs Fill shrinks)
- Gap injection post-layout

### 3. Arena Rendering (ArenaRender.fs:210-599)
- Inline layout solving using scratch array
- Linked-list traversal via NextSibling
- distributeFill function (negative-encoding for pending Fill)

### 4. Transition Effects (Transition.fs:37-237)
- Cell-level interpolation (lerpCell for colors, Rune snap at 0.5)
- Chebyshev distance for Grow (max dx dy ≤ r)
- SlideIn with offset calculation

### 5. OKLCH Color Space (Effects.fs:50-116)
- 5-step transformation (sRGB → linear → LMS → OKLab → OKLCH)
- Hue wrapping (shortest arc between h1 and h2)
- Perceptual smoothness for color animations

---

## Summary of Strengths

1. **Functional + Performant:** Elm architecture with SIMD optimization
2. **Two rendering paths:** Reference (clarity) + Arena (speed)
3. **Comprehensive animation:** 6 built-in transitions + custom support
4. **Deterministic testing:** Virtual time, no real I/O in tests
5. **Content-aware layout:** Intrinsic sizing respected during flex
6. **Zero-allocation in steady state:** Scratch reuse, pre-allocated buffers
7. **Packed encoding:** Color, constraint, padding, borders all bit-encoded

---

## Areas for Enhancement (Potential)

1. **Async command cancellation:** CancelSub is defined but check usage in App.fs
2. **Scroll optimization:** Currently allocates vBuf per frame; could buffer pool
3. **Responsive preview:** No breakpoint debugging utilities
4. **Layout debugging:** No visualization of constraint solutions
5. **Transition composition:** Limited to Sequence; no parallel multi-region transitions
6. **Memory profiling:** ArenaStats exists but not exposed in public API

---

## Conclusion

SageTUI achieves **high-performance functional TUI programming** through:
- Faithful implementation of Elm Architecture
- SIMD-accelerated differential rendering
- Zero-allocation arena-based serialization
- Comprehensive animation support (6 effects + custom)
- Dual rendering paths (clarity vs performance)

The architecture demonstrates how functional programming principles (immutability, pure functions) can coexist with performance optimizations (mutable buffers, specialized data structures) when carefully isolated in modules.

