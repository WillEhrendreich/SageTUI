# SageTUI — Copilot Instructions

## Build & Test

```bash
# Build everything
dotnet build SageTUI.slnx

# Run all tests (~1,112 Expecto tests)
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj

# Run a single test or filtered subset by name
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj -- --filter "PackedCell roundtrip"
dotnet run --project SageTUI.Tests/SageTUI.Tests.fsproj -- --filter "Phase 3"

# Watch mode (rebuilds on .fs changes)
dotnet watch run --project SageTUI.Tests/SageTUI.Tests.fsproj

# Run benchmarks
dotnet run -c Release --project SageTUI.Benchmarks

# Run a sample
dotnet run --project samples/09-SystemMonitor
```

The test project (`SageTUI.Tests/`) is separate from the library. `SageTUI.Tests/Program.fs` calls `runTestsInAssemblyWithCLIArgs`. Test files are split by domain: `Tests.fs` (core/phases 0–10), `LayoutTests.fs`, `ScrollTests.fs`, `CanvasTests.fs`, `WidgetTests.fs`, `HitTestTests.fs`, `HtmlTests.fs`, `CssTests.fs`, `IntegrationTests.fs`, `SnapshotTests.fs`, `SampleShowcaseTests.fs`.

## Architecture

SageTUI is an F# TUI library using the **Elm Architecture** (Model-View-Update) with a **double-buffered SIMD diff** renderer targeting .NET 10.

### Render Pipeline

```
User Model → View function → Element tree → Render.render → back Buffer
→ Buffer.diff (chunk-skip) → Presenter.present → ANSI escapes → terminal
```

The diff compares 16-cell chunks (256 bytes) via `Span.SequenceEqual` (JIT-vectorized on .NET 8+), then falls through to per-cell comparison for dirty chunks. Only changed cells produce ANSI output.

### Compile Order (dependency chain)

The `.fsproj` compile order is load-bearing — F# requires files ordered by dependency. The actual order in `SageTUI.Library.fsproj`:

```
Color → Buffer → Layout → BorderRender → Element → Measure → CanvasRender
→ Arena → Effects → Terminal → Ansi → Render → ArenaRender → Input → Tea
→ Detect → Transition → App → Widgets → Scroll
```

New files must be inserted at the correct position. The core render pipeline is `Color → Buffer → Element → Render → Ansi → App`.

### Key Types

| Type | File | Description |
|------|------|-------------|
| `PackedCell` | Buffer.fs | 16-byte blittable struct `{Rune:int32; Fg:int32; Bg:int32; Attrs:uint16; _pad:uint16}` |
| `Color` | Color.fs | `Default \| Named(BaseColor,Intensity) \| Ansi256(byte) \| Rgb(byte,byte,byte)` |
| `PackedColor` | Color.fs | int32 bit-packed: tag in bits 0-1, payload in upper bits |
| `TextAttrs` | Color.fs | `{Value: uint16}` — 8 boolean flags OR'd together |
| `Style` | Color.fs | `{Fg: Color option; Bg: Color option; Attrs: TextAttrs}` |
| `Element` | Element.fs | 11-case DU: Empty, Text, Row, Column, Overlay, Styled, Constrained, Bordered, Padded, Keyed, Canvas |
| `Buffer` | Buffer.fs | `{Cells: PackedCell array; Width: int; Height: int}` |
| `Program<'model,'msg>` | Tea.fs | `{Init; Update; View; Subscribe}` — Elm Architecture record |
| `Cmd<'msg>` | Tea.fs | NoCmd, Batch, OfAsync, OfCancellableAsync, CancelSub, Delay, Quit |
| `Sub<'msg>` | Tea.fs | KeySub, TimerSub, ResizeSub, CustomSub |
| `TerminalProfile` | Terminal.fs | 5 capability enums (Color, Unicode, Graphics, Input, Output) |

### TEA Loop (App.fs)

`App.run` is the main loop:
1. Drain message queue → `program.Update` for each → new model + commands
2. Reconcile subscriptions via `program.Subscribe`
3. `program.View model` → Element tree
4. `Render.render` into back buffer
5. `Buffer.diff` front vs back → changed indices
6. `Presenter.present` changed cells → ANSI string → write to terminal
7. Swap front/back buffers
8. Poll for terminal events, dispatch to subscribers

### Terminal Detection (Detect.fs)

Reads environment variables (`COLORTERM`, `WT_SESSION`, `TERM`, `TERM_PROGRAM`, `LANG`, `TMUX`, `STY`) to build a `TerminalProfile` with capability levels. Multiplexer detection downgrades capabilities (tmux caps graphics at HalfBlock, screen caps color at Indexed256). User can override via `SAGETUI_COLOR`/`SAGETUI_UNICODE`/`SAGETUI_GRAPHICS` env vars.

## Conventions

### F# Style

- **2-space indentation** everywhere — never 4
- **Pattern matching over if/else for ALL control flow** — including booleans: use `match x with true -> ... | false -> ...` not `if/then/else`
- Pipeline operators (`|>`) for data transformations
- Immutable by default; `mutable` only at IO boundaries
- `Result<'T,'E>` for fallible operations, not Option (Option hides failure reasons)
- Discriminated unions for domain types representing finite exclusive choices
- Modules with same name as their type contain pure transformation functions

### Element Builder API

The `El` module is the public API for building UI trees:

```fsharp
El.column [
  El.bordered BorderStyle.Rounded (
    El.bold (El.fg Color.Green (El.text "Title"))
  )
  El.row [
    El.width 20 (El.text "Left")
    El.fill (El.text "Right")
  ]
]
```

### PackedColor Bit Layout

```
Tag (bits 0-1): 0=Default, 1=Named, 2=Ansi256, 3=Rgb
Named:  bits 2-4 = BaseColor index, bit 5 = Intensity
Ansi256: bits 8-15 = palette index
Rgb:    bits 8-15 = R, bits 16-23 = G, bits 24-31 = B
```

### Testing

Tests use **Expecto** with **Expecto.Flip** argument order, **FsCheck** for property-based tests, and **Verify.Expecto** for snapshot tests:

```fsharp
// Expecto.Flip: message is ALWAYS the first argument, actual piped in last
actual |> Expect.equal "should be 42" 42
actual |> Expect.isTrue "should be true"
list   |> Expect.hasLength "should have 3" 3

// Comparison functions take a TUPLE
(actual, expected) |> Expect.isGreaterThan "should be greater"

// FsCheck property tests
testProperty "roundtrip" <| fun (x: Color) ->
  x |> PackedColor.pack |> PackedColor.unpack |> Expect.equal "roundtrip" x

// Snapshot tests (Verify.Expecto) — rendered output stored in SageTUI.Tests/snapshots/
testTask "snapshot" {
  let element = El.text "Hello" |> El.bordered Rounded
  do! Verify(renderToString element).UseDirectory("snapshots")
}
```

Core tests are organized by implementation phase (Phase 0–10) in `SageTUI.Tests/Tests.fs`. Snapshot files live in `SageTUI.Tests/snapshots/`.

### Dual Render Path (by design)

`Render.fs` and `ArenaRender.fs` are both active and intentionally maintained in parallel:
- `Render.fs` — the **reference implementation**: simple recursive Element DU walk, used heavily in unit tests (`Tests.fs`, `LayoutTests.fs`)
- `ArenaRender.fs` — the **production path**: walks the arena-lowered struct representation used by `App.run`
- `LayoutTests.fs` contains **parity tests** (`arenaParityTests`) that assert both produce identical `Buffer` output

**Any new `Element` case must be added to both render paths.** Adding to only one will break parity tests. This is the maintenance cost of the two-path design.

### Keys.bind Allocation Pattern

`Keys.bind` allocates a `Dictionary`. Declare bindings at **module/let level**, not inside the `Subscribe` lambda:

```fsharp
// ✅ Dictionary created once
let keyBindings = Keys.bind [ Key.Char 'q', Quit; Key.Escape, Quit ]
let program = { ...; Subscribe = fun _ -> [keyBindings] }

// ⚠️ Dictionary created on every model update
let program = { ...; Subscribe = fun _ -> [ Keys.bind [ Key.Char 'q', Quit ] ] }
```

### Unix Raw Mode

Raw mode on Linux/macOS uses `tcgetattr`/`tcsetattr` via `DllImport("libc")` in `Detect.fs`. No subprocess spawning. The saved `termios` snapshot is stored in `SavedModes.TermiosSnapshot` and restored on exit.

### Known Limitations

- **Transitions are full-screen only** — `App.run` wires transitions, but `Area` is always the full terminal size. Per-element scoped transitions require the layout pass to record each keyed node's area.

### Mouse Support (fully implemented)

Mouse input is fully wired end-to-end:
- `Detect.fs` `EnterRawMode` writes `?1000h ?1002h ?1006h ?1004h ?2004h` to enable normal button tracking, button-event motion tracking, SGR extended coordinates, focus tracking, and bracketed paste.
- `Console.In.Read()` feeds a raw char queue via a dedicated reader thread; `AnsiParser.parseEscape` / `parseSgrMouse` parses SGR mouse sequences from that queue.
- `MouseSub` fires on press/release, `DragSub` fires on motion/drag, `ClickSub` fires on press at a Keyed element (using the hit map built by `ArenaRender`).

### Confidential: expertPanel/

The `expertPanel/` directory is gitignored and contains design deliberations. Never reference panel member names or roster details in git-tracked files. Use generic terms like "architecture review" or "design analysis" when referencing panel feedback.
