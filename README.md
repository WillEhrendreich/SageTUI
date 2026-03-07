# SageTUI

**Build beautiful terminal UIs in F# with zero ceremony.**

Elm Architecture • SIMD rendering • 980+ tests • Zero external dependencies

<!-- TODO: Add GIF of Dashboard or Kanban sample here -->
<!-- ![SageTUI Demo](docs/demo.gif) -->

## Install

```bash
dotnet add package SageTUI --prerelease
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

let init () = 0, Cmd.none
let update msg count =
  match msg with
  | "inc" -> count + 1, Cmd.none
  | "quit" -> count, Cmd.quit
  | _ -> count, Cmd.none

let view count =
  El.column [
    El.text (sprintf "Count: %d" count) |> El.bold
    El.text "[j] increment  [q] quit" |> El.dim
  ] |> El.padAll 1 |> El.bordered Rounded

let program : Program<int, string> =
  { Init = init
    Update = update
    View = view
    Subscribe = fun _ -> [
      KeySub (fun (k, _) ->
        match k with
        | Key.Char 'j' -> Some "inc"
        | Key.Char 'q' -> Some "quit"
        | _ -> None) ] }

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

| Sample | Description |
|--------|-------------|
| `01-HelloWorld` | Minimal TEA app |
| `02-Dashboard` | Multi-panel dashboard with progress bars |
| `03-HtmlRenderer` | Render HTML in the terminal |
| `04-InteractiveForm` | TextInput, Select, validation |
| `05-ColorPalette` | TrueColor gradients and named colors |
| `06-Kanban` | Drag-and-drop Kanban board |
| `07-Transitions` | Animated enter/exit transitions |
| `08-Sparklines` | Real-time sparkline charts |

```bash
cd samples/06-Kanban && dotnet run
```

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
