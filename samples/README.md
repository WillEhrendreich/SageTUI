# SageTUI Samples

Seven showcase applications demonstrating the best features of SageTUI.

## Running a Sample

```bash
dotnet run --project samples/01-HelloWorld
```

## The Samples

### 01 — Hello World
The simplest SageTUI app. Type your name, see it styled with TrueColor and borders.
**Shows**: TEA architecture, keyboard input, styled text, padding.

### 02 — Dashboard
Real-time system monitor with auto-updating CPU bars, memory gauge, event log, and uptime clock.
**Shows**: `TimerSub`, borders, `fill`/`ratio` layouts, progress bars, dynamic colors.

### 03 — HTML Renderer ⭐
**The killer feature.** Browse 6 pages of raw HTML rendered directly to your terminal — styled text, tables, lists, forms, RGB gradients, and a full dashboard layout. All from plain HTML strings.
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
Animated card carousel cycling through SageTUI feature cards. Switch between 6 animation types (Fade, ColorMorph, Wipe, Dissolve, Grow) and toggle auto-advance.
**Shows**: `Keyed` elements, `Transition` DU, `viewTransition`, `TimerSub` for auto-cycle.
