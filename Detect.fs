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
    let color =
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

  let mutable private savedInputMode = 0u
  let mutable private savedOutputMode = 0u

  let enter (platform: Platform) =
    match platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      let mutable inMode = 0u
      let mutable outMode = 0u
      GetConsoleMode(stdin, &inMode) |> ignore
      GetConsoleMode(stdout, &outMode) |> ignore
      savedInputMode <- inMode
      savedOutputMode <- outMode
      SetConsoleMode(stdin, inMode &&& ~~~(ENABLE_LINE_INPUT ||| ENABLE_ECHO_INPUT ||| ENABLE_PROCESSED_INPUT)) |> ignore
      SetConsoleMode(stdout, outMode ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING) |> ignore
    | MacOS | Linux ->
      let psi = System.Diagnostics.ProcessStartInfo("stty", "raw -echo")
      psi.RedirectStandardOutput <- true
      psi.UseShellExecute <- false
      System.Diagnostics.Process.Start(psi).WaitForExit()

  let leave (platform: Platform) =
    match platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      SetConsoleMode(stdin, savedInputMode) |> ignore
      SetConsoleMode(stdout, savedOutputMode) |> ignore
    | MacOS | Linux ->
      let psi = System.Diagnostics.ProcessStartInfo("stty", "sane")
      psi.RedirectStandardOutput <- true
      psi.UseShellExecute <- false
      System.Diagnostics.Process.Start(psi).WaitForExit()
