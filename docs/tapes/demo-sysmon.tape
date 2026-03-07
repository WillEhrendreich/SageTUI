Output docs/demo-sysmon.gif

Set Width 110
Set Height 30
Set FontSize 13
Set Framerate 15
Set Padding 16
Set Theme "Catppuccin Mocha"
Set LoopOffset 0%

Env SAGETUI_DEMO_MODE "1"
Env SAGETUI_EXIT_AFTER_MS "12000"

Hide
Type "dotnet run --project samples/09-SystemMonitor/SystemMonitor.fsproj --no-build -c Release"
Enter
Sleep 2.5s
Show

Sleep 13.5s
