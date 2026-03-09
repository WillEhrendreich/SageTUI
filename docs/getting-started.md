# Getting Started with SageTUI

This guide walks you from "empty project" to a working interactive TUI application
with a form, focus management, keyboard navigation, and theme support.

Prerequisites: .NET 8+ and a terminal that supports ANSI escape codes (any modern
terminal on Linux, macOS, or Windows Terminal on Windows).

---

## 1. Create a Project

```bash
# Option A — use the built-in template (recommended)
dotnet new install SageTUI.Templates
dotnet new sagetui -o MyApp
cd MyApp
dotnet run

# Option B — from scratch
mkdir MyApp && cd MyApp
dotnet new console -lang F#
dotnet add package SageTUI
```

---

## 2. The Elm Architecture in 60 Seconds

SageTUI uses the **Elm Architecture** (TEA). Every app is three pure functions:

```
Model → (Msg → Model) → (Model → Element)
init      update              view
```

Plus a `Subscribe` function that maps the current model to a list of active subscriptions
(keyboard listeners, timers, resize handlers).

```fsharp
type Program<'model, 'msg> = {
    Init:      unit -> 'model * Cmd<'msg>
    Update:    'msg -> 'model -> 'model * Cmd<'msg>
    View:      'model -> Element
    Subscribe: 'model -> Sub<'msg> list
}
```

`App.run program` takes care of the render loop, input dispatch, and terminal
cleanup — including restoring the terminal on crash.

---

## 3. Hello, World

```fsharp
module HelloWorld

open SageTUI

type Msg = Quit

type Model = { Text: string }

let init () = { Text = "Hello, SageTUI!" }, Cmd.none

let update msg model =
    match msg with
    | Quit -> model, Cmd.quit

let view model =
    El.text model.Text
    |> El.bold
    |> El.fg (Color.Rgb(255uy, 200uy, 50uy))
    |> El.padAll 1
    |> El.bordered Rounded

let keyBindings = Keys.bind [ Key.Char 'q', Quit; Key.Escape, Quit ]

let program : Program<Model, Msg> = {
    Init      = init
    Update    = update
    View      = view
    Subscribe = fun _ -> [ keyBindings ]
}

[<EntryPoint>]
let main _ = App.run program; 0
```

> **`Keys.bind` allocation tip:** Declare key bindings at module level (as above),
> not inside the `Subscribe` lambda. The lambda is called on every update; if `Keys.bind`
> is inside it, a new `Dictionary` is allocated every frame.

---

## 4. Layout Primitives

Elements compose with `El.row` and `El.column`. Sizing follows a flexbox model:

| Function | Effect |
|---|---|
| `El.fill elem` | Expand to fill remaining space |
| `El.width 20 elem` | Fixed width in columns |
| `El.percentage 50 elem` | 50% of parent width |
| `El.minWidth 10 elem` | At least 10 columns |
| `El.maxWidth 40 elem` | At most 40 columns |

```fsharp
El.row [
    El.width 20 (El.text "Sidebar" |> El.bordered Light)
    El.fill (
        El.column [
            El.text "Title" |> El.bold
            El.text "Content goes here"
        ]
        |> El.padAll 1
    )
]
```

Borders: `Rounded`, `Light`, `Heavy`, `Double`, `Ascii`.

---

## 5. A Contact Form with Focus Ring

Here is a complete, runnable app with two text inputs and a submit button,
navigated with Tab/Shift-Tab.

```fsharp
module ContactForm

open SageTUI

// ── Domain ────────────────────────────────────────────────────────────────────

type Field = Name | Email | Submit

type Model = {
    Name:      TextInputModel
    Email:     TextInputModel
    Focus:     FocusRing<Field>
    Submitted: bool
}

type Msg =
    | Key of Key * Modifiers
    | Quit

// ── Init ─────────────────────────────────────────────────────────────────────

let init () =
    { Name      = TextInput.empty
      Email     = TextInput.empty
      Focus     = FocusRing.create [ Name; Email; Submit ]
      Submitted = false },
    Cmd.none

// ── Update ───────────────────────────────────────────────────────────────────

let update msg model =
    match msg with
    | Quit -> model, Cmd.quit

    | Key(Key.Tab, mods) ->
        let focus =
            match mods.HasFlag Modifiers.Shift with
            | true  -> FocusRing.prev model.Focus
            | false -> FocusRing.next model.Focus
        { model with Focus = focus }, Cmd.none

    | Key(Key.Enter, _) when FocusRing.isFocused Submit model.Focus ->
        { model with Submitted = true }, Cmd.none

    | Key(k, _) ->
        match FocusRing.current model.Focus with
        | Some Name  ->
            { model with Name  = TextInput.handleKey k model.Name  }, Cmd.none
        | Some Email ->
            { model with Email = TextInput.handleKey k model.Email }, Cmd.none
        | _ -> model, Cmd.none

// ── View ─────────────────────────────────────────────────────────────────────

let theme = Theme.nord

let view model =
    if model.Submitted then
        El.column [
            El.text "✓ Submitted!" |> El.bold |> El.fg theme.Success
            El.text (sprintf "  Name:  %s" model.Name.Text)
            El.text (sprintf "  Email: %s" model.Email.Text)
            El.text ""
            El.text "Press q to quit" |> El.dim
        ]
        |> El.padAll 2
        |> El.bordered Rounded
    else
        El.column [
            Theme.heading theme "Contact Form"
            El.text ""
            El.text "Name"  |> El.dim
            TextInput.viewThemed theme (FocusRing.isFocused Name model.Focus) model.Name
            El.text ""
            El.text "Email" |> El.dim
            TextInput.viewThemed theme (FocusRing.isFocused Email model.Focus) model.Email
            El.text ""
            (if FocusRing.isFocused Submit model.Focus
             then El.text "[ Submit ]" |> El.bold |> El.fg theme.Primary
             else El.text "[ Submit ]" |> El.dim)
            El.text ""
            El.text "Tab / Shift-Tab to navigate • Enter to submit • q to quit"
            |> El.dim
        ]
        |> El.padAll 2
        |> El.bordered Rounded
        |> El.width 50

// ── Subscriptions ─────────────────────────────────────────────────────────────

let quitBinding = Keys.bind [ Key.Char 'q', Quit; Key.Escape, Quit ]

let program : Program<Model, Msg> = {
    Init      = init
    Update    = update
    View      = view
    Subscribe = fun _ ->
        // Forward all key events for focus navigation and TextInput handling.
        // quitBinding is checked first; all other keys fall through to Key msg.
        [ quitBinding
          Sub.KeySub(fun (k, mods) -> Some (Key(k, mods))) ]
}

[<EntryPoint>]
let main _ = App.run program; 0
```

### How `FocusRing` works

- `FocusRing.create fields` builds the ring from a list of field identifiers.
- `FocusRing.next` / `FocusRing.prev` advance focus, wrapping at either end.
- `FocusRing.current ring` returns `Some field` (or `None` for an empty ring).
- `FocusRing.isFocused field ring` returns `true` if `field` is currently focused —
  the idiomatic way to pass a focused flag to widget view functions.

---

## 6. Applying a Theme

SageTUI ships five built-in themes: `Theme.dark`, `Theme.light`, `Theme.nord`,
`Theme.dracula`, `Theme.catppuccin`.

A theme is a record of `Color` values:

```fsharp
type Theme = {
    Primary:    Color
    Secondary:  Color
    Accent:     Color
    Success:    Color
    Warning:    Color
    Error:      Color
    TextFg:     Color
    TextDim:    Color
    Background: Color
    Border:     BorderStyle
}
```

**Convenience helpers** apply theme colors to elements:

```fsharp
Theme.heading    theme "Title"       // El.text "Title" |> El.bold |> El.fg theme.Primary
Theme.subheading theme "Subtitle"    // El.text "Subtitle" |> El.fg theme.Secondary
Theme.panel      theme "Title" inner // bordered column with heading
Theme.success    theme "OK"          // El.text "OK" |> El.fg theme.Success
Theme.warning    theme "Caution"     // El.text "Caution" |> El.fg theme.Warning
Theme.error      theme "Fail"        // El.text "Fail" |> El.fg theme.Error
Theme.bordered   theme inner         // El.bordered theme.Border inner
```

**Widget-level theming** applies a theme to widget configs or view calls:

```fsharp
// Config-based widgets:
let bar = ProgressBar.withTheme Theme.dracula { ProgressBar.defaults with Percent = 0.7 }
let tab = Tabs.withTheme Theme.nord myTabConfig

// View-based widgets:
TextInput.viewThemed theme focused model
TextInput.viewWithPlaceholderThemed theme "Enter email…" focused model
Checkbox.viewThemed theme "Accept terms" focused isChecked
Toggle.viewThemed   theme "Enabled" "Disabled" focused isOn
```

---

## 7. Commands and Async

`Cmd<'msg>` represents effects. The most common commands:

```fsharp
Cmd.none                    // no effect
Cmd.ofMsg MyMsg             // dispatch a message immediately
Cmd.batch [cmd1; cmd2]      // run multiple commands
Cmd.quit                    // exit the app cleanly
Cmd.delay 500 MyMsg         // dispatch MyMsg after 500 ms
Cmd.map MsgWrapper innerCmd // wrap a child component's Cmd
```

For async effects:

```fsharp
Cmd.ofTask (fun () -> task {
    let! data = fetchDataAsync ()
    return DataLoaded data
})
```

---

## 8. Subscriptions

Subscriptions are active input sources. Return them from `Subscribe`:

```fsharp
Subscribe = fun model ->
    [ Keys.bind [ Key.Char 'q', Quit ]          // specific key bindings
      Sub.KeySub(fun (k, mods) -> Some (Key(k,mods))) // all keypresses
      Sub.TimerSub("tick", TimeSpan.FromMilliseconds(1000.), fun () -> Tick) // every 1000 ms
      Sub.ResizeSub(fun (w, h) -> Some (Resize(w, h))) ] // terminal resize
```

`Sub.KeySub` fires on every keypress and maps it to an optional message.
Returning `None` ignores that key.

---

## 9. What's Next

| Topic | Where to look |
|---|---|
| Scrollable lists | `samples/04-InteractiveForm` — uses `VirtualList` |
| Animated transitions | `samples/07-Transitions` |
| Canvas / pixel art | `samples/08-Sparklines` — uses `HalfBlock` canvas |
| Real-time dashboard | `samples/09-SystemMonitor` |
| HTML rendering | `samples/03-HtmlRenderer` |
| Kanban board | `samples/06-Kanban` — FocusRing across columns |
| Benchmarks | `SageTUI.Benchmarks/` — BenchmarkDotNet suite |
| Full API reference | XML doc comments on every public function |

Run any sample with:

```bash
dotnet run --project samples/01-HelloWorld
```

---

## Troubleshooting

**Colors look wrong / no colors:** Set `SAGETUI_COLOR=truecolor` (or `256` / `ansi`).

**Box-drawing characters are broken:** Set `SAGETUI_UNICODE=full` (or `basic` / `ascii`).

**App doesn't restore the terminal on exit:** This is usually a signal-handler gap
on Linux. SageTUI registers `Console.CancelKeyPress` and `AppDomain.ProcessExit`
handlers; if your app exits via `Environment.Exit()` from a background thread,
call `Terminal.restore()` explicitly before exiting.

**E2E tests are ignored in CI:** The two ignored tests require a PTY (pseudo-terminal)
not available in all CI environments. They are integration smoke tests only.
