# Changelog

All notable changes to SageTUI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] — 2026-03-07

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
| Layout 50-item column | 26.1 μs | 91 KB |
| Layout nested 3-level | 102 μs | 143 KB |

### API Stability

v0.x releases may include breaking API changes between minor versions.
We'll document all changes here. The goal is a stable v1.0 once the API
surface has been validated by real-world usage.
