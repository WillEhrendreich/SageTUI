# SageTUI Migration Guide

## Versioning Policy

SageTUI follows **0.x semantic versioning**:

- **Minor versions** (0.x → 0.y) **may contain breaking changes** until 1.0
- **Patch versions** (0.x.y → 0.x.z) are backwards-compatible bug fixes only
- **1.0** will commit to stable API + semantic versioning from that point forward

Migration notes for every minor version are listed below, newest first.
Breaking changes are listed explicitly; non-breaking additions are summarised.

---

## 0.9.0 (current)

### Breaking Changes

**`Key.Char` now takes `System.Text.Rune`**

In F# there is no implicit `char` → `Rune` coercion. Code that compiled against
an older prerelease build using `Key.Char 'q'` must be updated:

```fsharp
// Before (compile error)
Key.Char 'q'

// After — use the new helper (AutoOpen, no import needed)
keyChar 'q'

// Or explicitly
Key.Char (System.Text.Rune 'q')
```

The `keyChar` helper is available without any `open` statement because `InputHelpers`
is `[<AutoOpen>]`.

### Additions

- `keyChar : char -> Key` — ergonomic char→Key constructor (see above)
- `Cmd.saveString` / `Cmd.loadString` — local storage commands
- `TestHarness.scrollAt` — virtual scroll simulation in tests
- `TextInput.clickAt` — maps terminal column to cursor position
- `VirtualList.clickToggleAt` — click-to-toggle multi-select
- `Select.handleMouse` — click-to-select in dropdowns
- `Tabs.viewKeyed` + `Tabs.clickActivate` — click-to-switch tabs
- `Viewport.handleMouse` — scrollable read-only text panes with mouse support
- 2,630 unit, property, and integration tests

---

## Older Prereleases (pre-0.9)

No migration notes — API was unstable and undocumented.
