Output docs/sysmon.svg

Set Width 1400
Set Height 800
Set Padding 20
Set FontSize 14
Set Theme "Catppuccin Mocha"
Set LoopOffset 10%

Hide
Type "dotnet run --project samples/09-SystemMonitor/SystemMonitor.fsproj --no-build -c Release"
Enter
Sleep 4s
Show

# ── Overview tab – let metrics update a few times ───────────────────────────
Sleep 1s
Down
Sleep 500ms
Down
Sleep 500ms
Up
Sleep 500ms
Down
Sleep 500ms

# ── Switch to Network tab ────────────────────────────────────────────────────
Right
Sleep 2s

# ── Switch to About tab ──────────────────────────────────────────────────────
Right
Sleep 1.5s

# ── Back to Overview ────────────────────────────────────────────────────────
Right
Sleep 2s

Type "q"
