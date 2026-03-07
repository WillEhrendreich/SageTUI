# SageTUI Architecture: Quick Reference Guide

## CRITICAL: DESIGN ONLY (NO IMPLEMENTATION)
SageTUI is a DESIGN that hasn't been implemented yet.
Only documentation exists in expertPanel/. This is a blank canvas.

## THE 9 ELEMENT CASES
Empty | Text | Row | Column | Overlay | Styled | Constrained | Bordered | Padded

## COLORS: Full Terminal Color Space
- Default (terminal default)
- Named(BaseColor, Intensity) — 16 colors (8 base × 2 intensity)
- Indexed byte — 256-color palette
- Rgb(byte, byte, byte) — 24-bit truecolor

## CONSTRAINTS (Layout)
Fixed | Min | Max | Percentage | Fill | Ratio

## PACKEDCELL (16-byte Blittable)
struct { Rune: int32; Fg: int32; Bg: int32; Attrs: uint16; _pad: uint16 }
- Exactly 16 bytes for Vector128 SIMD
- Color DU pre-packed to int32 during write
- No pointers, fully blittable

## RENDER LOOP (9 Steps)
1. Drain message queue (ConcurrentQueue)
2. Run Update (pure state transition)
3. Interpret Cmd (async/timers)
4. Reconcile subscriptions
5. Call View → Element DU
6. Lower to Frame Arena (O(n), zero-GC)
7. Render to Buffer (Element → PackedCell)
8. SIMD Diff (chunk-skip, 10-20× faster)
9. Present ANSI (batched escape sequences)

## FRAME ARENA (Zero-GC)
User builds Element DU every frame (GC'd).
Arena lowers DU to flat struct arrays: O(n) one-pass, zero allocations.
Reset at frame end: O(1).

## ANIMATION APPROACH
- No framework; subscriptions send Tick messages
- Store progress (0.0-1.0) in model
- View uses progress for visual state
- Color gradients: user-defined lerp helpers

## SIMD DIFF SPEED
- Chunk-skip via Span.SequenceEqual
- 16 cells (256 bytes) at a time
- Skips unchanged chunks entirely
- 10-20× faster than cell-by-cell
- JIT-vectorized: AVX2, NEON, etc.

## PERFORMANCE (200×50 terminal)
Total: 1-10ms per frame (<5 heap objects)
Bottleneck: Terminal I/O (1-10ms), not rendering

