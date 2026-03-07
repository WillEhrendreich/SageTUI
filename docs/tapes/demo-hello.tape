Output docs/demo-hello.gif

Set Width 800
Set Height 480
Set FontSize 14
Set Framerate 15
Set Padding 16
Set Theme "Catppuccin Mocha"
Set LoopOffset 0%

Env SAGETUI_DEMO_MODE "1"
Env SAGETUI_EXIT_AFTER_MS "6500"

Hide
Type "dotnet run --project samples/01-HelloWorld/HelloWorld.fsproj --no-build -c Release"
Enter
Sleep 2.5s
Show

Sleep 7s
