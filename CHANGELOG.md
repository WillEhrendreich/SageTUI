# Changelog

All notable changes to SageTUI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Performance

- **Zero-alloc arena render path** (Sprints 21–22): Per-frame heap allocation in the hot path reduced from ~3,640 bytes/frame to < 100 bytes/frame — a **97 % reduction**.
  - _Before_: `Area` was a reference record (one heap object per layout child per frame), text rendering used `StringBuilder` + `string`-per-rune, keyed nodes materialised a `string` per `El.keyed` node per frame into `HitEntry.Key: string`.
  - _After_: `[<Struct>] Area` (zero allocation for layout traversal); `Buffer.writeCharSpan` uses `ReadOnlySpan<char>` + `SpanRuneEnumerator` (both ref-structs, zero allocation); `[<Struct>] HitEntry` stores `(KeyStart, KeyLen)` offsets into the shared `TextBuf` — string materialisation deferred to `hitTest`/`keyAreas` call sites only.
  - `measureWidth` text case (kind=1) aligned with the render path: uses `ReadOnlySpan<char>` instead of `System.String`.
  - `writeCharSpan` inner loop: `y >= 0 && y < buf.Height` hoisted out of the rune loop (y is loop-invariant).

### Changed

- **`FieldId` is now a reference DU** (was `[<Struct>]`). This is technically a breaking
  change for any code relying on struct-specific behavior (stack allocation, `Span<FieldId>`
  compatibility, or the `[<Struct>]` default zero-value constructor `FieldId()`). In practice
  `FieldId` only appears in `Map`, `list`, and record fields — all heap-allocated contexts —
  so the behavioral impact is negligible. The motivation: the struct default constructor
  produced `FieldId(null)`, a footgun eliminated by the reference DU. If you were using
  `FieldId()` anywhere, replace it with `FieldId.create "your-id"`.

### Added

- `FieldId.create` — factory function that validates the input string is non-null. Prefer
  this over the raw `FieldId` constructor when the value originates from external input.
- Theme integration for all major widgets (all additive, no breaking changes):
  - `ProgressBar.withTheme` — applies `theme.Primary`/`theme.TextDim` to filled/empty segments
  - `TextInput.viewThemed` / `TextInput.viewWithPlaceholderThemed`
  - `Checkbox.viewThemed` — accent for checked, TextDim for unchecked, bold when focused
  - `Toggle.viewThemed` — success for on-state, TextDim for off-state, bold when focused
  - `Tabs.withTheme` / `VirtualList.withTheme`
- Benchmark CI regression gate (`benchmark-regression` job): 150% threshold, 15 iterations,
  compares against stored gh-pages baseline on every push and PR.
- Steady-state allocation tests in `ArenaAllocationTests.fs` (4 `testSequenced` tests):
  - Dashboard tree: ≤ 1,024 B/frame after 100-frame JIT warmup
  - Keyed tree (exercises `HitMap`): ≤ 1,024 B/frame — verifies `HitEntry` struct stores offsets, not strings
  - NodeCount determinism across resets
  - PeakNodes high-water mark tracking
- **Sprint 26 — TransitionPayload DU, reconcile cleanup, new allocation/regression tests**:
  - `TransitionPayload` DU (`DissolvePayload of int array | NoPayload`) replaces `DissolveOrder: int array option` in `ActiveTransition` — eliminates the illegal `None` state for Dissolve transitions; missing payload now fails fast at construction rather than silently producing wrong behaviour.
  - `Reconcile.reconcile` now returns `(entering list, exiting list)` 2-tuple; the `staying` computation (`Map.filter + Map.toList` per frame) was a pure overhead that no call site used.
  - `NoPayload` branch in `App.fs` `applyDissolve` includes `at.Key` in the `failwith` message — zero-cost debuggability improvement.
  - XML doc on `El.keyed` explains the layout-modifier ordering constraint: `El.width`/`El.fill`/`El.height` must be applied _outside_ the `El.keyed` call, not inside — the inner constraint is invisible to the Row/Column layout pass.
  - Added reconcile-tier allocation test (≤ 8,192 B/frame for 8 keyed elements; Map-based baseline).
  - Added `App.run keyAreas gate regression` test: verifies that `prevKeyAreas` is updated for staying elements (guards against reintroducing the Sprint 24 bug).

### Fixed

- Benchmark CI `benchmark-push-baseline` job now has `permissions: {}` on the read-only
  `benchmark-regression` job, ensuring the write token is never present in PR contexts.
- `undo` algebraic laws (Laws 1–6): generator-first `SingleLineContent` arbitrary eliminates
  `Arb.filter` shrinking defect; Laws 5 and 6 correctly guard for no-op edits.



Initial public release.

### Added

**Core Architecture**
- Elm Architecture (TEA) runtime with `init`, `update`, `view`, `subscribe`
- `Program<'model, 'msg>` record — wire up and call `App.run`
- `App.display` for static content (no state management needed)
- `App.simple` for apps without subscriptions
- `Cmd` module: `none`, `ofMsg`, `batch`, `map`, `quit`, `ofTask`
- `Sub` module: `KeySub`, `TimerSub`, `ResizeSub`, `MouseSub`, `map`
- `Keys.bind` / `Keys.bindWithMods` — zero-ceremony keyboard subscriptions
- `Program.map` — component composition with model/message projection
- Error boundary — terminal always restored on crash

**Layout Engine**
- `El.row` / `El.column` — terminal-native flexbox
- Constraint system: `El.fill`, `El.width`, `El.percentage`, `El.ratio`, `El.minWidth`, `El.maxWidth`
- `El.padAll`, `El.padHV`, `El.padLTRB` — box model padding
- `El.bordered` with 5 styles: `Light`, `Heavy`, `Double`, `Rounded`, `Ascii`
- `El.center`, `El.alignRight`, `El.alignBottom` — 9-position alignment
- `El.gap` — spacing between children
- Flex-shrink — proportional shrinking when content exceeds available space
- Overflow clipping — content clipped to parent bounds

**Rendering**
- SIMD-accelerated buffer diff (Vector256, identical frames diff in <1μs)
- Arena-allocated render tree (pre-allocated nodes, single pass)
- Tree renderer (lower allocation) and arena renderer (better for complex UIs)
- 24-bit TrueColor, Ansi256, and named color support
- Auto-detection of terminal capabilities (TrueColor, 256-color, multiplexer)

**Widgets** (17 total)
- `TextInput` — single-line text input with cursor
- `Select` — dropdown selection
- `ProgressBar` — configurable progress indicator
- `Tabs` — tab bar navigation
- `Table` — data table with selected row
- `Modal` — overlay modal dialogs
- `TreeView` — expandable/collapsible tree
- `Checkbox` / `Toggle` / `RadioGroup` — boolean and choice inputs
- `SpinnerWidget` — animated loading spinner
- `Toast` — timed notification messages
- `Form` — composable form with field types and validation
- `FocusRing` — keyboard focus cycling
- `ScrollableList` — virtualized scrollable list

**Scrolling**
- `ScrollState` — immutable scroll position with clamping
- `Scroll.view` — composable scroll viewport
- Keyboard navigation (up/down) and scroll indicators

**Canvas**
- `HalfBlock` mode — 2 vertical pixels per cell (▀/▄)
- `Braille` mode — 8 dots per cell (2×4 grid)

**Transitions**
- `Fade`, `Wipe`, `SlideIn`, `Dissolve`, `ColorMorph`, `Grow`, `Custom`
- `Keyed` elements with enter/exit transitions

**Themes**
- 5 built-in: `dark`, `light`, `nord`, `dracula`, `catppuccin`
- `Theme.heading`, `Theme.panel`, `Theme.accent` helpers

**HTML Bridge**
- `HtmlString.parseFragment` — parse HTML fragments to Element trees
- Table, style attribute, and semantic tag support

**Mouse**
- Click subscriptions with hit-testing
- Z-order resolution for overlapping elements

**Developer Tools**
- `El.debugLayout` — visual layout inspector with colored borders and constraint labels
- BenchmarkDotNet suite for performance regression testing
- `dotnet new sagetui` project template

**Samples** (9 included)
- HelloWorld, Dashboard, HtmlRenderer, InteractiveForm, ColorPalette
- Kanban, Transitions, Sparklines, SystemMonitor

### Performance

Benchmarked on .NET 10.0, i7-11800H:

| Benchmark | Mean | Allocated |
|-----------|------|-----------|
| Buffer.diff identical (80×24) | 637 ns | 32 B |
| Buffer.diff 10% changed (80×24) | 3.3 μs | 2.2 KB |
| Render dashboard tree (80×24) | 19.4 μs | 6.6 KB |
| Arena render dashboard (80×24) | 31.8 μs | 310 KB |
<!-- 310 KB is initial arena allocation (pre-allocated node pool, amortized across frames). -->
<!-- Steady-state per-frame allocation after warmup trends toward 0 as the arena is reused. -->
| Layout 50-item column | 26.1 μs | 91 KB |
| Layout nested 3-level | 102 μs | 143 KB |

### API Stability

v0.x releases may include breaking API changes between minor versions.
We'll document all changes here. The goal is a stable v1.0 once the API
surface has been validated by real-world usage.
