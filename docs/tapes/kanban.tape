Output docs/kanban.svg

Set Width 1400
Set Height 800
Set Padding 20
Set FontSize 14
Set Theme "Catppuccin Mocha"
Set LoopOffset 20%

Hide
Type "dotnet run --project samples/06-Kanban/Kanban.fsproj --no-build -c Release"
Enter
Sleep 3s
Show

# ── Column navigation ───────────────────────────────────────────────────────
Sleep 800ms
Down
Sleep 500ms
Down
Sleep 500ms
Up
Sleep 500ms

# ── Move to InProgress ──────────────────────────────────────────────────────
Right
Sleep 600ms
Down
Sleep 500ms

# ── Move to Review ──────────────────────────────────────────────────────────
Right
Sleep 600ms

# ── Grab a card and move it ─────────────────────────────────────────────────
Space
Sleep 700ms
Right
Sleep 700ms
Space
Sleep 800ms

# ── Navigate back, grab another ─────────────────────────────────────────────
Left
Sleep 500ms
Left
Sleep 500ms
Left
Sleep 500ms
Down
Sleep 400ms
Space
Sleep 700ms
Right
Sleep 700ms
Space
Sleep 800ms

# ── Final state: all 4 columns visible ──────────────────────────────────────
Sleep 1.5s

Type "q"
