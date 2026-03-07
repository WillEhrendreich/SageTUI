# SageTUI Samples

Nine sample applications demonstrating the current SageTUI surface, grouped by how polished and representative they are.

## Running a Sample

```bash
dotnet run --project samples/01-HelloWorld
```

## Sample Tiers

- **Flagship:** 09 SystemMonitor
- **Showcase:** 04 InteractiveForm, 06 Kanban, 08 Sparklines
- **Supporting:** 01 HelloWorld, 02 Dashboard, 05 ColorPalette, 07 Transitions
- **Experimental:** 03 HtmlRenderer

## The Samples

### 01 — Hello World
The simplest SageTUI app: a tiny counter with borders, color, and keyboard bindings.
**Shows**: TEA architecture, `Keys.bind`, styled text, padding.

### 02 — Dashboard
Multi-panel operations dashboard with a clock, CPU bars, memory gauge, event log, and uptime.
**Shows**: `TimerSub`, `ResizeSub`, borders, `fill`/`ratio` layouts, progress bars, dynamic colors.

### 03 — HTML Renderer
Experimental HTML rendering lab. Useful for exploring the bridge, but not the primary front-door sample.
**Shows**: `HtmlString.parseFragment`, CSS color support, semantic tags, inline styles.

### 04 — Interactive Form
Registration form with text inputs, a dropdown select, Tab-based focus ring, and validation feedback.
**Shows**: `TextInput`, `FocusRing`, `Select`, keyboard navigation, error display, styled badges.

### 05 — Color Palette
Four-tab showcase of the complete color system: Base-16 named colors, 256-color palette grid, TrueColor RGB gradients (rainbow, fire, per-channel), and all text attributes with styled badges.
**Shows**: `Color` DU (Named, Ansi256, Rgb), `TextAttrs`, `Style` composition, tab navigation.

### 06 — Kanban Board
Four-column Kanban with cards you can navigate and drag between columns. Cards show priority badges and assignees.
**Shows**: Complex layouts, `fill` distribution, nested borders, keyboard-driven state machine, `Map`-based state.

### 07 — Transitions
Animated card carousel cycling through SageTUI feature cards. Switch between multiple transition styles and toggle auto-advance.
**Shows**: `Keyed` elements, `Transition` DU, `viewTransition`, `TimerSub` for auto-cycle.

### 08 — Sparklines
Compact telemetry dashboard using HalfBlock and Braille rendering for real-time charts.
**Shows**: canvas rendering, data visualization, `TimerSub`, dense operator-console layouts.

### 09 — System Monitor
The flagship sample. A dense terminal dashboard with tabs, live telemetry, scrolling process views, and themed status hierarchy.
**Shows**: themes, tabs, scroll regions, live updates, telemetry composition, sample-grade layout polish.
