# SageTUI

**An Elm Architecture TUI framework for F# with SIMD-accelerated rendering, zero external dependencies, and a zero-GC frame loop.**

SageTUI brings the Elm Architecture (TEA) to the terminal. Build interactive terminal applications with a functional, composable API — the same `init → update → view` pattern you know from Elm, Elmish, and Fabulous, but purpose-built for character grids.

## Features

- **Elm Architecture** — `init`, `update`, `view`, `subscribe`. Pure state management, predictable updates, composable views.
- **Arena Rendering** — All frame data lives in pre-allocated arena buffers. Zero allocations per frame, zero GC pauses.
- **SIMD Diff** — Frame-to-frame diffing uses hardware SIMD instructions. Only changed cells are written to the terminal.
- **Keyed Transitions** — Fade, wipe, dissolve, and color morph animations for element enter/exit.
- **CSS-Vocabulary Layout** — `Row`, `Column`, `Fill`, `Percentage`, `Min`, `Max`, padding, borders, alignment, gap — terminal-native semantics with familiar names.
- **HTML Bridge** — Parse HTML fragments and render them directly in the terminal. Tables, lists, inline styles, semantic tags — all mapped to `Element` trees.
- **Rich Widget Suite** — TextInput, Select, ProgressBar, Tabs, Table, Modal, TreeView, Checkbox, Toggle, RadioGroup, Spinner, Toast, Form composition.
- **Canvas Rendering** — HalfBlock (▀/▄) and Braille (⠿) modes for pixel-level graphics in terminal cells.
- **TrueColor** — Full 24-bit RGB, named colors, hex (`#ff0000`), and `rgb(255,0,0)` parsing.
- **Mouse & Focus** — Hit-testing with Z-order, click subscriptions, Tab/Shift+Tab focus cycling.
- **Error Boundary** — Terminal state is always restored on unhandled exceptions — no corrupted terminals.
- **965+ Tests** — Expecto-based test suite covering layout, rendering, widgets, hit-testing, scrolling, and more.

## Quick Start

### Hello World (Minimal TEA)

```fsharp
open SageTUI

type Model = { Name: string }
type Msg = KeyPressed of Key * Modifiers | Quit

let init () = { Name = "" }, Cmd.none

let update msg model =
  match msg with
  | Quit -> model, Cmd.quit
  | KeyPressed(Char c, _) -> { Name = model.Name + string c }, Cmd.none
  | _ -> model, Cmd.none

let view model =
  El.column [
    El.text (sprintf "Hello, %s!" (match model.Name with "" -> "World" | n -> n))
      |> El.bold |> El.fg (Color.Named(Cyan, Bright))
    El.text "Type your name, [Esc] to quit" |> El.dim
  ] |> El.padAll 1

let subscribe _ =
  [ KeySub (fun (k, m) ->
      match k with Escape -> Some Quit | _ -> Some (KeyPressed(k, m))) ]

[<EntryPoint>]
let main _ =
  let profile =
    Detect.fromEnvironment
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
      (fun () -> System.Console.WindowWidth, System.Console.WindowHeight)
  let backend = Backend.create profile
  App.run backend
    { Init = init; Update = update; View = view; Subscribe = subscribe }
  0
```

### Even Simpler — `App.simple`

Skip subscriptions when you don't need them:

```fsharp
let program = App.simple init update view
App.run backend program
```

### Static Display

Show a view with no interaction (Esc to quit):

```fsharp
App.display (fun () ->
  El.column [
    El.text "Hello from SageTUI!" |> El.bold |> El.fg (Color.Named(Green, Bright))
    El.text "Press Esc to exit" |> El.dim
  ] |> El.bordered Rounded |> El.padAll 1
) backend
```

## Layout Engine

SageTUI's layout engine uses CSS vocabulary with terminal-native semantics:

```fsharp
// Rows and columns
El.row [ El.text "Left"; El.text "Right" |> El.fill ]
El.column [ El.text "Top"; El.text "Bottom" ]

// Constraints
El.text "Fixed" |> El.width 20
El.text "Flexible" |> El.fill
El.text "Half" |> El.percentage 50

// Box model
El.text "Padded" |> El.padAll 2 |> El.bordered Rounded

// Alignment
El.text "Centered" |> El.center
El.text "Bottom-right" |> El.alignBottomRight

// Gap between children
El.column [ El.text "A"; El.text "B"; El.text "C" ] |> El.gap 1
```

## Widgets

```fsharp
// TextInput with cursor
TextInput.view focused model.Input

// Progress bar
ProgressBar.view { ProgressBar.defaults with Percent = 0.75; Width = 40 }

// Tabs
Tabs.view ["Home"; "Settings"; "Help"] activeTab

// Table with headers and rows
Table.view headers rows selectedRow focused

// Modal overlay
Modal.view { Modal.defaults with Title = Some "Confirm" } content

// TreeView with keyboard navigation
TreeView.view toString focused nodes treeState

// Form composition
Form.view fields focusedKey model
```

## HTML Rendering

```fsharp
open SageTUI.Html

let html = """
<div>
  <h1 style="color: cyan">Dashboard</h1>
  <table>
    <tr><th>Service</th><th>Status</th></tr>
    <tr><td>API</td><td style="color: green">● Online</td></tr>
    <tr><td>DB</td><td style="color: yellow">● Degraded</td></tr>
  </table>
</div>"""

let element = HtmlString.parseFragment html
```

## Architecture

```
┌─────────────────────────────────────────┐
│            Your Application             │
│  init → update → view → subscribe       │
├─────────────────────────────────────────┤
│              Element Tree               │
│  Text · Row · Column · Styled · ...     │
├─────────────────────────────────────────┤
│           Arena Rendering               │
│  Pre-allocated nodes, zero GC           │
├─────────────────────────────────────────┤
│            SIMD Diff                    │
│  Hardware-accelerated change detection  │
├─────────────────────────────────────────┤
│          Terminal Backend               │
│  ANSI escape sequences, raw mode        │
└─────────────────────────────────────────┘
```

## Performance

- **Zero-GC frame loop** — Arena allocation means no garbage collector pressure during rendering
- **SIMD diff** — Only changed cells are written to the terminal, minimizing I/O
- **O(1) arena reset** — Frame buffers clear in constant time
- **Content-aware layout** — Flex distribution, proportional shrink, overflow clipping all in a single pass
- **Memoized views** — `El.lazy'` skips re-computation when model reference hasn't changed

## Requirements

- .NET 10.0+
- A terminal with ANSI escape sequence support (all modern terminals)

## License

MIT
