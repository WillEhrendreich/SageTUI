# SageTUI Widget Gallery

Visual showcase of every widget and layout primitive, rendered from the test suite's snapshot fixtures.

---

## Borders

Five built-in border styles: `Rounded`, `Light`, `Heavy`, `Double`, `Ascii`.

**Rounded**
```
╭────────────────────────────╮
│Hello World                 │
│                            │
│                            │
╰────────────────────────────╯
```

**Heavy**
```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃Hello World                 ┃
┃                            ┃
┃                            ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

---

## Layout Primitives

**Column / Row / Fill / Pad**
```
╭──────────────────────────────╮
│         Hello World          │   ← El.centered (El.text "Hello World")
│                              │
│ Left              Right Fill │   ← El.row [ El.text "Left"; El.fill (El.text "Right Fill") ]
╰──────────────────────────────╯
```

**SplitPane (horizontal)**
```
╭──────────────────╮╭──────────────────╮
│Left Pane         ││Right Pane        │
│                  ││                  │
│                  ││                  │
│                  ││                  │
│                  ││                  │
│                  ││                  │
│                  ││                  │
│                  ││                  │
╰──────────────────╯╰──────────────────╯
```

---

## Tabs

```
Home       Settings        About
```

`Tabs.view` renders a horizontal tab bar. `Tabs.viewKeyed` enables click-to-activate.

---

## Table

```
Name        Score
────────────────────
Alice       95

Bob         87

Carol       91
```

`Table.view` renders sortable, scrollable data grids with customisable column widths.

---

## Progress Bar

```
██████████░░░░░░░░░░    50%
```

`Gauge.bar` takes a `float` in `[0.0, 1.0]` and renders filled/empty blocks with a percentage label.

---

## Spinner

```
⠋
```

`Spinner.braille` cycles 8 Braille frames. `Spinner.dots` uses ASCII fallback.

---

## Select (dropdown)

```
▸ Alpha

  Beta


  Gamma
```

`Select.view` renders an open list with cursor arrow. `Select.handleMouse` supports click-to-select.

---

## TextInput

```
Hello World
```

`TextInput.view` renders a single-line editable field with cursor, selection highlight, and placeholder.
`TextInput.clickAt` maps a terminal column click to a cursor position using the Unicode width oracle.

---

## Checkbox

```
[x] Accept terms and conditions
[ ] Subscribe to newsletter
```

`Checkbox.view` toggles on `Space` / `Enter`.

---

## Radio Group

```
(•) Option A
( ) Option B
( ) Option C
```

`Radio.view` renders mutually exclusive choices.

---

## Toggle

```
[ON ]
[OFF]
```

`Toggle.view` renders a compact boolean switch.

---

## Toast

```
 ┌──────────────────────────┐
 │File saved!               │
 │                          │
 │                          │
 └──────────────────────────┘
```

`Toast.view` renders a transient overlay notification that auto-dismisses.
`ToastQueue` stacks multiple toasts and ticks them down in the update cycle.

---

## Scrollable List

```
1. Item 01                        █
2. Item 02                        ░
3. Item 03                        ░
4. Item 04                        ░
5. Item 05                        ░
```

`VirtualList` renders only visible rows. Supports multi-select, `clickToggleAt`, and keyboard navigation.

---

## Tree View

```
▾ Documents
  ▾ Work
      Report.doc
      Budget.xlsx
  ▾ Personal

      Photo.jpg

  README.md
```

`Tree.view` renders hierarchical data with expand/collapse. Keyboard and click navigation.

---

## Modal

`Modal.view` renders a centred overlay with a backdrop. Useful for confirmation dialogs.

---

## Scroll Indicator

```
1. Item 01                        █
2. Item 02                        ░
3. Item 03                        ░
4. Item 04                        ░
5. Item 05                        ░
```

`Scroll.indicator` renders a proportional scrollbar on the right edge.

---

## Real-World Example: System Monitor

A full sample combining Tabs, Charts, VirtualList, Gauge, and more:

```
  FLAGSHIP    System Monitor          dense operator console         LIVE          uptime 00:00
  Overview       Network       About                     ←/→ or Tab to switch views

 ╭────────────────────╮ ╭────────────────────╮ ╭──────────────────────╮ ╭────────────────────╮
 │ CPU LOAD           │ │ MEMORY             │ │ THROUGHPUT           │ │ UPTIME             │
 │ 24.2%              │ │ 40.7%              │ │ 17.2 Mbps            │ │ 00:00              │
 │ Nominal envelope   │ │ Working set stays  │ │ Ingress + egress com │ │ search-index at 36 │
 ╰────────────────────╯ ╰────────────────────╯ ╰──────────────────────╯ ╰────────────────────╯

 ╭──────────────────────────────────────────╮ ╭────────────────────────────────────────────╮
 │ CPU history         current 24.2         │ │ Memory history       current 40.7          │
 │ ▂▁▁▂▁▁▂▂▁▂▁▁▂▁▁▁▂▁▂▂▁▁▁▂▂▂▁▂▁▂▂▂▁▁▁▂▂▁▂▂│ │ ▄▄▄▃▃▄▄▃▄▄▄▄▄▃▄▄▃▄▄▄▄▃▄▄▃▄▃▄▃▄▄▄▄▃▄▃▃▃▄▃  │
 │ 24.2% of threshold                       │ │ 40.7% of threshold                         │
 ╰──────────────────────────────────────────╯ ╰────────────────────────────────────────────╯

 ╭────────────────────────────────────────────────────────────────────────────────────────╮
 │ Top processes                                               ↑/↓ scroll                 │
 │   PROCESS           CPU     MEM       STATE   DETAIL                                   │
 │ ─────────────────────────────────────────────────────────────────                      │
 │ ● search-index       36.7%   472 MB   run     serving requests                         │
 │ ● postgres           36.3%   837 MB   run     serving requests                         │
 │ ● api-edge           36.1%   736 MB   sync    checkpointing                            │
 ╰────────────────────────────────────────────────────────────────────────────────────────╯
```

Run it: `dotnet run --project samples/09-SystemMonitor`

---

## Widget API Quick Reference

| Widget | Module | Key functions |
|--------|--------|---------------|
| Text input | `TextInput` | `init`, `update`, `view`, `clickAt` |
| Checkbox | `Checkbox` | `view`, `toggle` |
| Radio group | `Radio` | `view`, `navigate` |
| Toggle | `Toggle` | `view`, `toggle` |
| Select dropdown | `Select` | `view`, `handleMouse`, `open`, `close` |
| Table | `Table` | `view`, `sort`, `select` |
| Tabs | `Tabs` | `view`, `viewKeyed`, `navigate` |
| Progress bar | `Gauge` | `bar`, `ring` |
| Spinner | `Spinner` | `braille`, `dots`, `tick` |
| Tree view | `Tree` | `view`, `expand`, `collapse` |
| Virtual list | `VirtualList` | `view`, `scrollTo`, `clickToggleAt` |
| Scroll indicator | `Scroll` | `indicator`, `handleKey` |
| Viewport | `Viewport` | `ofString`, `view`, `scrollDown`, `scrollUp` |
| Toast | `Toast` | `view`, `dismiss` |
| Toast queue | `ToastQueue` | `push`, `tickAll`, `view` |
| Modal | `Modal` | `view` |
| SplitPane | `SplitPane` | `view`, `resize` |
| FuzzyFinder | `FuzzyFinder` | `view`, `filter` |
| Chart | `Chart` | `sparkline`, `barChart`, `lineChart` |

See [docs/getting-started.md](docs/getting-started.md) for usage examples and the full API guide.
