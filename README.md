# SageTUI

**Build beautiful terminal UIs in F# with zero ceremony.**

Elm Architecture • SIMD rendering • 990+ tests • Zero external dependencies

<!-- TODO: Add GIF of Dashboard or Kanban sample here -->
<!-- ![SageTUI Demo](docs/demo.gif) -->

## Install

```bash
dotnet add package SageTUI
```

Or start from a template:

```bash
dotnet new install SageTUI.Templates
dotnet new sagetui -n MyApp
cd MyApp && dotnet run
```

## 5-Line Hello World

```fsharp
open SageTUI

App.display (fun () ->
  El.text "Hello from SageTUI!" |> El.bold |> El.fg (Color.Named(Cyan, Bright))
  |> El.bordered Rounded |> El.padAll 1)
```

That's it. No boilerplate. `App.display` handles terminal setup, rendering, and cleanup.

## Interactive App (Elm Architecture)

```fsharp
open SageTUI

type Msg = Increment | Quit

let init () = 0, Cmd.none

let update msg count =
  match msg with
  | Increment -> count + 1, Cmd.none
  | Quit -> count, Cmd.quit

let view count =
  El.column [
    El.text (sprintf "Count: %d" count) |> El.bold
    El.text "[j] increment  [q] quit" |> El.dim
  ] |> El.padAll 1 |> El.bordered Rounded

let program : Program<int, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [
      Keys.bind [
        Key.Char 'j', Increment
        Key.Char 'q', Quit
        Key.Escape, Quit
      ] ] }

[<EntryPoint>]
let main _ = App.run program; 0
```

`App.run` auto-detects your terminal (TrueColor, 256-color, multiplexer) and handles everything.

## Features

| Category | What You Get |
|----------|-------------|
| **Architecture** | Elm Architecture (init/update/view/subscribe), pure state management |
| **Layout** | Row, Column, Fill, Percentage, Min/Max, padding, borders, alignment, gap, flex-shrink |
| **Rendering** | Arena-allocated zero-GC frame loop, SIMD-accelerated diff, 24-bit TrueColor |
| **Widgets** | TextInput, Select, Table, Tabs, Modal, TreeView, ProgressBar, Checkbox, Toggle, RadioGroup, Spinner, Toast, Form |
| **Scrolling** | ScrollState, scroll indicators, mouse wheel, ScrollableList, keyboard navigation |
| **Canvas** | HalfBlock (▀/▄) and Braille (⠿) pixel modes |
| **Transitions** | Fade, Wipe, SlideIn, Dissolve, ColorMorph, Grow, Custom |
| **Themes** | 5 built-in themes (dark, light, nord, dracula, catppuccin) |
| **HTML Bridge** | Parse HTML fragments → Element trees (tables, styles, semantic tags) |
| **Mouse** | Click subscriptions, hit-testing with Z-order, focus cycling |
| **Safety** | Error boundary restores terminal on crash. Always. |

## Layout Engine

Terminal-native flexbox with CSS vocabulary:

```fsharp
// Rows and columns
El.row [ El.text "Left" |> El.fill; El.text "Right" |> El.width 20 ]

// Box model
El.text "Content" |> El.padAll 1 |> El.bordered Rounded

// Alignment
El.text "Centered" |> El.center

// Gap between children
El.column [ El.text "A"; El.text "B"; El.text "C" ] |> El.gap 1

// Percentage sizing
El.row [
  El.text "Sidebar" |> El.percentage 30
  El.text "Main" |> El.fill
]
```

## Widgets

```fsharp
TextInput.view focused model.Input
ProgressBar.view { ProgressBar.defaults with Percent = 0.75; Width = 40 }
Tabs.view ["Home"; "Settings"; "Help"] activeTab
Table.view headers rows selectedRow focused
Modal.view { Modal.defaults with Title = Some "Confirm" } content
TreeView.view toString focused nodes treeState
Form.view fields focusedKey model
```

## Themes

```fsharp
let themed = Theme.dark  // or: light, nord, dracula, catppuccin
El.text "Styled heading" |> Theme.heading themed
El.column [...] |> Theme.panel themed "My Panel"
```

## .NET Interop

Call any .NET library from your update function:

```fsharp
let update msg model =
  match msg with
  | FetchData ->
    model, Cmd.ofTask
      (fun () -> httpClient.GetStringAsync("https://api.example.com/data"))
      (fun result -> DataReceived result)
  | DataReceived data -> { model with Data = data }, Cmd.none
```

## HTML Rendering

```fsharp
open SageTUI.Html

let html = """<div>
  <h1 style="color: cyan">Dashboard</h1>
  <table>
    <tr><th>Service</th><th>Status</th></tr>
    <tr><td>API</td><td style="color: green">● Online</td></tr>
  </table>
</div>"""

let element = HtmlString.parseFragment html
```

## Performance

Benchmarked with [BenchmarkDotNet](https://github.com/dotnet/BenchmarkDotNet) on .NET 10.0, i7-11800H:

| Benchmark | Mean | Allocated |
|-----------|------|-----------|
| Buffer.diff identical (80×24) | **637 ns** | 32 B |
| Buffer.diff 10% changed (80×24) | **3.3 μs** | 2.2 KB |
| Render dashboard tree (80×24) | **19.4 μs** | 6.6 KB |
| Arena render dashboard (80×24) | **31.8 μs** | 310 KB |
| Layout 50-item column | **26.1 μs** | 91 KB |
| Layout nested 3-level (10 rows) | **102 μs** | 143 KB |

Buffer diff uses SIMD (Vector256) to compare 16-byte PackedCells — identical frames diff in under 1μs.
The tree renderer allocates less; the arena renderer is optimized for large/complex UIs with pre-allocated nodes.

Run benchmarks yourself: `dotnet run -c Release --project SageTUI.Benchmarks`

## Architecture

```
┌─────────────────────────────────────────┐
│          Your Application               │
│  init → update → view → subscribe       │
├─────────────────────────────────────────┤
│            Element Tree                 │
│  Text · Row · Column · Styled · ...     │
├─────────────────────────────────────────┤
│         Arena Rendering (zero-GC)       │
│  Pre-allocated nodes, single pass       │
├─────────────────────────────────────────┤
│       SIMD Diff (hardware accel)        │
│  Only changed cells hit the terminal    │
├─────────────────────────────────────────┤
│         Terminal Backend                │
│  Auto-detect, TrueColor, mouse, raw     │
└─────────────────────────────────────────┘
```

## Samples

Each sample builds on concepts from the previous ones:

| # | Sample | What You Learn |
|---|--------|----------------|
| 01 | **HelloWorld** | TEA basics — `init`/`update`/`view`, `Keys.bind`, borders |
| 02 | **Dashboard** | Layout (`row`/`column`/`fill`), `TimerSub`, progress bars |
| 03 | **HtmlRenderer** | HTML→Element bridge, page navigation |
| 04 | **InteractiveForm** | TextInput widget, raw `KeySub` for forms, validation |
| 05 | **ColorPalette** | TrueColor/Ansi256/Named colors, text styles |
| 06 | **Kanban** | Complex state, keyboard-driven UI, multi-column layout |
| 07 | **Transitions** | Animated enter/exit, `Keyed` elements, conditional subscriptions |
| 08 | **Sparklines** | Canvas rendering (HalfBlock/Braille), real-time data |
| 09 | **SystemMonitor** | **Everything together** — sparklines, tabs, scrolling, themes, progress bars |

```bash
cd samples/09-SystemMonitor && dotnet run
```

## Concepts

SageTUI uses the [Elm Architecture](https://guide.elm-lang.org/architecture/) (TEA):

- **Model** — your app's state (an immutable F# type)
- **Msg** — a discriminated union of everything that can happen
- **init** — returns `(model, Cmd)` — your starting state
- **update** — `msg → model → (model, Cmd)` — handle events, return new state
- **view** — `model → Element` — render state as a terminal UI tree
- **subscribe** — `model → Sub list` — declare ongoing subscriptions (keyboard, timers, resize)
- **Cmd** — side effects (async tasks, quit signal). `Cmd.none` means "no side effect"
- **Sub** — ongoing event sources. `Keys.bind` for keyboard, `TimerSub` for polling

## Advanced: Custom Backend

For testing or custom terminal implementations:

```fsharp
let profile = Detect.fromEnvironment envReader sizeGetter
let backend = Backend.create profile
App.runWithBackend backend program
```

## Requirements

- .NET 10.0+
- A terminal with ANSI escape sequence support (all modern terminals)

## License

MIT
