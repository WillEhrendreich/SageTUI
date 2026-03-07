namespace SageTUI

open System
open System.Runtime.InteropServices

module Detect =
  let fromEnvironment (envReader: string -> string option) (getSize: unit -> int * int) : TerminalProfile =
    let env key = envReader key |> Option.defaultValue ""
    let envOpt key = envReader key

    let term = env "TERM"
    let colorTerm = env "COLORTERM"
    let termProgram = env "TERM_PROGRAM"
    let wtSession = envOpt "WT_SESSION"

    let color =
      match colorTerm with
      | "truecolor" | "24bit" -> ColorCapability.TrueColor
      | _ ->
        match wtSession with
        | Some _ -> ColorCapability.TrueColor
        | None ->
          match term.Contains("256color") with
          | true -> ColorCapability.Indexed256
          | false ->
            match term with
            | "dumb" -> ColorCapability.NoColor
            | _ -> ColorCapability.Basic16

    let unicode =
      let lang = env "LANG"
      match lang.Contains("UTF-8") || lang.Contains("utf-8") with
      | true -> UnicodeCapability.FullUnicode
      | false ->
        match wtSession with
        | Some _ -> UnicodeCapability.FullUnicode
        | None ->
          match lang.Contains("ISO-8859") with
          | true -> UnicodeCapability.Latin1
          | false -> UnicodeCapability.AsciiOnly

    let graphics =
      match termProgram with
      | "iTerm.app" -> GraphicsCapability.ITermInline
      | _ ->
        match term.Contains("kitty") with
        | true -> GraphicsCapability.KittyGraphics
        | false ->
          match unicode >= UnicodeCapability.UnicodeBmp with
          | true -> GraphicsCapability.HalfBlock
          | false -> GraphicsCapability.TextOnly

    let platform =
      match RuntimeInformation.IsOSPlatform(OSPlatform.Windows) with
      | true -> Windows
      | false ->
        match RuntimeInformation.IsOSPlatform(OSPlatform.OSX) with
        | true -> MacOS
        | false -> Linux

    { Color = color
      Unicode = unicode
      Graphics = graphics
      Input = InputCapability.FunctionKeys
      Output = OutputCapability.AltScreen
      Size = getSize()
      TermName = termProgram
      Platform = platform }

  let adjustForMultiplexer (envReader: string -> string option) (profile: TerminalProfile) : TerminalProfile =
    let tmux = envReader "TMUX"
    let sty = envReader "STY"
    match tmux, sty with
    | Some _, _ ->
      { profile with
          Graphics = min profile.Graphics GraphicsCapability.HalfBlock
          Output = min profile.Output OutputCapability.AltScreen }
    | _, Some _ ->
      { profile with
          Color = min profile.Color ColorCapability.Indexed256
          Graphics = GraphicsCapability.TextOnly
          Output = OutputCapability.RawMode }
    | None, None -> profile

module UserOverride =
  let apply (envReader: string -> string option) (profile: TerminalProfile) : TerminalProfile =
    // NO_COLOR (https://no-color.org): if set to any value, disable all ANSI color output.
    // Checked first; takes precedence over SAGETUI_COLOR.
    let hasNoColor = envReader "NO_COLOR" |> Option.isSome
    let color =
      match hasNoColor with
      | true -> ColorCapability.NoColor
      | false ->
        match envReader "SAGETUI_COLOR" with
        | Some "none" -> ColorCapability.NoColor
        | Some "16" -> ColorCapability.Basic16
        | Some "256" -> ColorCapability.Indexed256
        | Some "true" | Some "truecolor" -> ColorCapability.TrueColor
        | _ -> profile.Color

    let unicode =
      match envReader "SAGETUI_UNICODE" with
      | Some "ascii" -> UnicodeCapability.AsciiOnly
      | Some "latin1" -> UnicodeCapability.Latin1
      | Some "bmp" -> UnicodeCapability.UnicodeBmp
      | Some "full" -> UnicodeCapability.FullUnicode
      | _ -> profile.Unicode

    let graphics =
      match envReader "SAGETUI_GRAPHICS" with
      | Some "text" -> GraphicsCapability.TextOnly
      | Some "halfblock" -> GraphicsCapability.HalfBlock
      | Some "braille" -> GraphicsCapability.Braille
      | Some "sixel" -> GraphicsCapability.Sixel
      | Some "kitty" -> GraphicsCapability.KittyGraphics
      | _ -> profile.Graphics

    { profile with Color = color; Unicode = unicode; Graphics = graphics }

module RawMode =
  [<DllImport("kernel32.dll")>]
  extern bool GetConsoleMode(nativeint hConsoleHandle, uint32& lpMode)
  [<DllImport("kernel32.dll")>]
  extern bool SetConsoleMode(nativeint hConsoleHandle, uint32 dwMode)
  [<DllImport("kernel32.dll")>]
  extern nativeint GetStdHandle(int nStdHandle)

  let private ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004u
  let private ENABLE_PROCESSED_INPUT = 0x0001u
  let private ENABLE_LINE_INPUT = 0x0002u
  let private ENABLE_ECHO_INPUT = 0x0004u

  type SavedModes =
    { Platform: Platform
      InputMode: uint32
      OutputMode: uint32 }

  let enter (platform: Platform) : SavedModes =
    match platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      let mutable inMode = 0u
      let mutable outMode = 0u
      GetConsoleMode(stdin, &inMode) |> ignore
      GetConsoleMode(stdout, &outMode) |> ignore
      SetConsoleMode(stdin, inMode &&& ~~~(ENABLE_LINE_INPUT ||| ENABLE_ECHO_INPUT ||| ENABLE_PROCESSED_INPUT)) |> ignore
      SetConsoleMode(stdout, outMode ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING) |> ignore
      { Platform = Windows; InputMode = inMode; OutputMode = outMode }
    | MacOS | Linux ->
      let psi = System.Diagnostics.ProcessStartInfo("stty", "raw -echo")
      psi.RedirectStandardOutput <- true
      psi.UseShellExecute <- false
      System.Diagnostics.Process.Start(psi).WaitForExit()
      { Platform = platform; InputMode = 0u; OutputMode = 0u }

  let leave (saved: SavedModes) =
    match saved.Platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      SetConsoleMode(stdin, saved.InputMode) |> ignore
      SetConsoleMode(stdout, saved.OutputMode) |> ignore
    | MacOS | Linux ->
      let psi = System.Diagnostics.ProcessStartInfo("stty", "sane")
      psi.RedirectStandardOutput <- true
      psi.UseShellExecute <- false
      System.Diagnostics.Process.Start(psi).WaitForExit()

module Backend =
  let private mapKey (ki: ConsoleKeyInfo) : Key option =
    match ki.Key with
    | ConsoleKey.Escape -> Some Escape
    | ConsoleKey.Enter -> Some Enter
    | ConsoleKey.Backspace -> Some Backspace
    | ConsoleKey.Tab -> Some Tab
    | ConsoleKey.UpArrow -> Some Up
    | ConsoleKey.DownArrow -> Some Down
    | ConsoleKey.LeftArrow -> Some Left
    | ConsoleKey.RightArrow -> Some Right
    | ConsoleKey.Home -> Some Home
    | ConsoleKey.End -> Some End
    | ConsoleKey.Delete -> Some Delete
    | ConsoleKey.Insert -> Some Insert
    | ConsoleKey.PageUp -> Some PageUp
    | ConsoleKey.PageDown -> Some PageDown
    | ConsoleKey.F1 -> Some (F 1)
    | ConsoleKey.F2 -> Some (F 2)
    | ConsoleKey.F3 -> Some (F 3)
    | ConsoleKey.F4 -> Some (F 4)
    | ConsoleKey.F5 -> Some (F 5)
    | ConsoleKey.F6 -> Some (F 6)
    | ConsoleKey.F7 -> Some (F 7)
    | ConsoleKey.F8 -> Some (F 8)
    | ConsoleKey.F9 -> Some (F 9)
    | ConsoleKey.F10 -> Some (F 10)
    | ConsoleKey.F11 -> Some (F 11)
    | ConsoleKey.F12 -> Some (F 12)
    | _ ->
      let c = ki.KeyChar
      // Guard against lone surrogates and NUL bytes that some ConPTY implementations
      // inject during startup — these are not valid printable characters.
      match Char.IsHighSurrogate(c) || Char.IsLowSurrogate(c) || c = '\000' with
      | true -> None
      | false -> Some (Key.Char c)

  let private mapMods (ki: ConsoleKeyInfo) : Modifiers =
    let mutable m = Modifiers.None
    match ki.Modifiers.HasFlag(ConsoleModifiers.Shift) with
    | true -> m <- m ||| Modifiers.Shift
    | false -> ()
    match ki.Modifiers.HasFlag(ConsoleModifiers.Control) with
    | true -> m <- m ||| Modifiers.Ctrl
    | false -> ()
    match ki.Modifiers.HasFlag(ConsoleModifiers.Alt) with
    | true -> m <- m ||| Modifiers.Alt
    | false -> ()
    m

  /// Attempt to read a key event; returns None if Console.ReadKey throws or returns a
  /// sentinel ConsoleKeyInfo that indicates stdin was closed (e.g. in some ConPTY setups).
  let private tryReadKeyEvent () : TerminalEvent option =
    try
      let ki = Console.ReadKey(true)
      mapKey ki |> Option.map (fun key -> KeyPressed(key, mapMods ki))
    with _ -> None

  let create (profile: TerminalProfile) : TerminalBackend =
    let mutable savedModes = Unchecked.defaultof<RawMode.SavedModes>
    let mutable lastW = Console.WindowWidth
    let mutable lastH = Console.WindowHeight
    let checkResize () =
      let w = Console.WindowWidth
      let h = Console.WindowHeight
      match w <> lastW || h <> lastH with
      | true ->
        lastW <- w
        lastH <- h
        Some (Resized(w, h))
      | false -> None
    { Size = fun () -> (Console.WindowWidth, Console.WindowHeight)
      Write = fun s -> Console.Write(s)
      Flush = fun () -> Console.Out.Flush()
      PollEvent = fun timeoutMs ->
        match checkResize () with
        | Some r -> Some r
        | None ->
          match Console.KeyAvailable with
          | true -> tryReadKeyEvent ()
          | false ->
            match timeoutMs > 0 with
            | true ->
              Threading.Thread.Sleep(timeoutMs)
              match checkResize () with
              | Some r -> Some r
              | None ->
                match Console.KeyAvailable with
                | true -> tryReadKeyEvent ()
                | false -> None
            | false -> None
      EnterRawMode = fun () -> savedModes <- RawMode.enter profile.Platform
      LeaveRawMode = fun () -> RawMode.leave savedModes
      Profile = profile }

  /// Auto-detect terminal capabilities and create a backend. Zero ceremony.
  let auto () : TerminalBackend =
    let envReader k = System.Environment.GetEnvironmentVariable(k) |> Option.ofObj
    let sizeGetter () = System.Console.WindowWidth, System.Console.WindowHeight
    let profile =
      Detect.fromEnvironment envReader sizeGetter
      |> Detect.adjustForMultiplexer envReader
      |> UserOverride.apply envReader
    create profile
