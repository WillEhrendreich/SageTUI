# Changelog

All notable changes to SageTUI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.9.2] - 2026-03-11

### Fixed

- **CI: macOS Hex1b E2E tests skipped on CI runners** — PTY input delivery is unreliable
  on GitHub Actions macOS runners; tests now skip when `CI=true` (still run locally).
- **CI: benchmark output filename mismatch** — Added `namespace SageTUI.Benchmarks` to
  `SageTUI.Benchmarks/Program.fs` so BenchmarkDotNet generates the expected
  `SageTUI.Benchmarks.BufferDiffBenchmarks-report-full-compressed.json` filename.
- **CI: `Sub.fileWatch` flaky tests on busy runners** — Wrapped `sprint72FileWatchTests`
  in `testSequenced` and increased pre-write sleep to 2000ms to eliminate race between
  OS thread scheduling and `FileSystemWatcher` setup. All 7 fileWatch tests now pass
  reliably on Ubuntu, Windows, and macOS.
- **CI pack step: NETSDK1064/1005 on Ubuntu** — Removed `--no-build` from
  `dotnet pack` in CI so it always performs a fresh restore+build with correct assets.

## [0.9.1] - 2025-01-01 (unreleased)

### Breaking Changes

- **`Program.OnError` handler: `None` return value semantics changed** (was: reraise; now: continue silently)

  In previous releases a handler that returned `None` caused the original exception to be
  re-raised (identical to having no handler at all). This was a footgun: the idiomatic F#
  reading of `None` is "no result / do nothing", not "crash". The new behavior:

  | Handler return | Old behavior     | **New behavior**          |
  |----------------|------------------|---------------------------|
  | `Some msg`     | dispatch `msg`   | dispatch `msg` *(unchanged)* |
  | `None`         | reraise exception | **continue silently**     |
  | No handler (`OnError = None`) | reraise | reraise *(unchanged)* |

  **Migration:** if you relied on `handler ex -> None` to propagate the crash, replace it
  with an explicit `raise ex` inside the handler body:

  ```fsharp
  OnError = Some (fun ex ->
    logError ex       // do your logging
    raise ex)         // explicitly re-raise — now the only way to crash from a handler
  ```


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

- **SIGTERM graceful shutdown** (`runWith` and `runInlineWith`): When the process receives
  `SIGTERM` (e.g. `docker stop`, `systemd stop`, `kill <pid>`) on non-Windows platforms,
  SageTUI now cancels all active subscriptions, restores the terminal (cursor, alt-screen, raw
  mode, mouse/paste tracking), and exits with code **143** (= 128 + 15, POSIX convention).
  Previously a SIGTERM left the terminal in raw mode.
- **SIGTSTP/SIGCONT on inline** (`runInlineWith`): Inline-mode programs now suspend/resume
  the terminal correctly on Ctrl-Z, matching the behavior already present in full-screen `runWith`.
- **Active subscriptions cancelled on unhandled crash**: When an exception propagates out of the
  main loop (no `OnError` handler, or handler re-raises), background timer/custom subscriptions
  are now cancelled before the terminal is restored. Previously, orphan async loops continued
  running until the process died.
- **Drain loop de-duplicated**: `runWith` and `runInlineWith` previously contained identical
  per-frame message drain implementations. Both now delegate to a single private
  `drainMessages` function, eliminating the maintenance risk of the two diverging silently.
- `AppConfig.LogSink: string -> unit` — override the default `sagetui-errors.log` file write
  to integrate with your structured logging infrastructure. All async exception log lines are
  routed through this sink.

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

- **Sprint 28 — SlideIn / Grow transitions implemented, Sequence dispatcher, NuGet release pipeline**:
  - `TransitionFx.applySlideIn` — new directional slide-in effect. Content enters from any of four edges (Right/Left/Down/Up), sliding to its final position as `t` advances 0→1. Implemented as a shifted clip: the new content is offset and reveals progressively into the area, while uncovered cells show the snapshot. 19 new unit tests cover all four directions, boundary conditions, and vertical offset.
  - `TransitionFx.applyGrow` — new center-expand effect. Content expands outward from the area center using Chebyshev distance, producing a rectangular grow envelope. At `t=0` only the center is visible; at `t=1` the full area is revealed.
  - `TransitionFx.applySequenceDuration` — helper returning total duration of a `Sequence` transition list (including nested). Powers the `Sequence` dispatcher in `App.fs`.
  - `App.fs` apply loop refactored into `let rec applyTransition` — handles all cases including `Sequence` (sub-transitions dispatched in order with local `t` scaling, sharing the original snapshot). `Dissolve` inside `Sequence` generates a seeded shuffle inline rather than failing on `NoPayload`.
  - NuGet publish workflow (`.github/workflows/nuget-publish.yml`): triggered on `v*` tags; builds release, runs tests, packs with tag version, pushes to NuGet.org via `NUGET_API_KEY` secret.
  - `RepositoryUrl` and `PackageProjectUrl` corrected to `https://github.com/WillEhrendreich/SageTUI`.
  - `scratch.fsx` added at repo root — interactive SageFs validation script for iterating on transition logic.
  - `benchmark-regression` job permissions hardened: write token is never present in PR contexts.

- `undo` algebraic laws (Laws 1–6): generator-first `SingleLineContent` arbitrary eliminates
  `Arb.filter` shrinking defect; Laws 5 and 6 correctly guard for no-op edits.

### Added (initial public release baseline)

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
