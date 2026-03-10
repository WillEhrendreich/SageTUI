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

    // Detect input capability based on terminal identity.
    // Modern terminals (Windows Terminal, iTerm2, xterm*, kitty) all support SGR
    // mouse reporting (?1000h+?1006h). Fall back to FunctionKeys for unknown terminals.
    let input =
      match termProgram with
      | "iTerm.app" -> InputCapability.MouseSgr
      | _ ->
        match wtSession with
        | Some _ -> InputCapability.MouseSgr
        | None ->
          match term with
          | t when t.Contains("xterm") || t.Contains("kitty") -> InputCapability.MouseSgr
          | _ -> InputCapability.FunctionKeys

    { Color = color
      Unicode = unicode
      Graphics = graphics
      Input = input
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

  // termios constants (POSIX)
  [<DllImport("libc", EntryPoint = "tcgetattr")>]
  extern int tcgetattr(int fd, nativeint termios)
  [<DllImport("libc", EntryPoint = "tcsetattr")>]
  extern int tcsetattr(int fd, int action, nativeint termios)

  let private TCSANOW = 0
  let private ICANON = 0x100u   // line-buffered input
  let private ECHO   = 0x8u     // echo input characters
  let private ISIG   = 0x80u    // enable signals (Ctrl-C etc) — keep enabled
  // Termios struct is 60 bytes on Linux/macOS (enough for both ABI layouts)
  let private termiosSize = 60

  let private ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004u
  let private ENABLE_PROCESSED_INPUT = 0x0001u
  let private ENABLE_LINE_INPUT = 0x0002u
  let private ENABLE_ECHO_INPUT = 0x0004u
  // Mouse input and VT sequence passthrough for Windows Terminal / ConPTY
  let private ENABLE_MOUSE_INPUT = 0x0010u
  let private ENABLE_VIRTUAL_TERMINAL_INPUT = 0x0200u

  type SavedModes =
    { Platform: Platform
      InputMode: uint32
      OutputMode: uint32
      /// Saved termios bytes for Unix platforms (60 bytes, zero on Windows).
      TermiosSnapshot: byte array }

  let enter (platform: Platform) : SavedModes =
    match platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      let mutable inMode = 0u
      let mutable outMode = 0u
      GetConsoleMode(stdin, &inMode) |> ignore
      GetConsoleMode(stdout, &outMode) |> ignore
      SetConsoleMode(stdin, (inMode &&& ~~~(ENABLE_LINE_INPUT ||| ENABLE_ECHO_INPUT ||| ENABLE_PROCESSED_INPUT)) ||| ENABLE_MOUSE_INPUT ||| ENABLE_VIRTUAL_TERMINAL_INPUT) |> ignore
      SetConsoleMode(stdout, outMode ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING) |> ignore
      { Platform = Windows; InputMode = inMode; OutputMode = outMode; TermiosSnapshot = Array.empty }
    | MacOS | Linux ->
      let snapshot = Array.zeroCreate<byte> termiosSize
      let handle = Runtime.InteropServices.Marshal.AllocHGlobal(termiosSize)
      try
        tcgetattr(0, handle) |> ignore
        Runtime.InteropServices.Marshal.Copy(handle, snapshot, 0, termiosSize)
        // Clear ICANON and ECHO; leave ISIG so Ctrl-C still works
        let iflagsOffset = 0  // c_iflag is first field on both Linux and macOS
        let lflagsOffset =
          match platform with
          | Linux -> 12   // offsetof(termios, c_lflag) on Linux = 12
          | _     -> 8    // offsetof(termios, c_lflag) on macOS = 8
        let lflags = BitConverter.ToUInt32(snapshot, lflagsOffset)
        let newLflags = lflags &&& ~~~(ICANON ||| ECHO)
        let newBytes = BitConverter.GetBytes(newLflags)
        Runtime.InteropServices.Marshal.Copy(newBytes, 0, handle + nativeint lflagsOffset, 4)
        tcsetattr(0, TCSANOW, handle) |> ignore
      finally
        Runtime.InteropServices.Marshal.FreeHGlobal(handle)
      { Platform = platform; InputMode = 0u; OutputMode = 0u; TermiosSnapshot = snapshot }

  let leave (saved: SavedModes) =
    match saved.Platform with
    | Windows ->
      let stdin = GetStdHandle(-10)
      let stdout = GetStdHandle(-11)
      SetConsoleMode(stdin, saved.InputMode) |> ignore
      SetConsoleMode(stdout, saved.OutputMode) |> ignore
    | MacOS | Linux ->
      match saved.TermiosSnapshot.Length with
      | 0 -> ()
      | _ ->
        let handle = Runtime.InteropServices.Marshal.AllocHGlobal(termiosSize)
        try
          Runtime.InteropServices.Marshal.Copy(saved.TermiosSnapshot, 0, handle, termiosSize)
          tcsetattr(0, TCSANOW, handle) |> ignore
        finally
          Runtime.InteropServices.Marshal.FreeHGlobal(handle)

module Backend =
  let private tryGetConsoleSize () =
    try
      Some (Console.WindowWidth, Console.WindowHeight)
    with _ ->
      None

  let private tryParsePositiveInt (value: string option) =
    match value with
    | Some text ->
      match Int32.TryParse text with
      | true, parsed when parsed > 0 -> Some parsed
      | _ -> None
    | None -> None

  let private safeInitialSize (envReader: string -> string option) =
    match tryGetConsoleSize () with
    | Some size -> size
    | None ->
      match tryParsePositiveInt (envReader "COLUMNS"), tryParsePositiveInt (envReader "LINES") with
      | Some width, Some height -> width, height
      | _ -> 80, 25

  /// Start the background stdin reader. Reads raw chars from Console.In and parses
  /// ANSI/VT escape sequences (including SGR mouse) into TerminalEvents.
  ///
  /// Architecture:
  ///   rawThread:   Console.In.Read() → charQueue (one char per enqueue, blocks until available)
  ///   parseThread: charQueue → eventQueue (accumulates escape seqs, emits TerminalEvents)
  ///
  /// Escape sequence timeout: if no follow-up char arrives within 50ms after ESC,
  /// emits a bare Escape key. Otherwise accumulates until isCompleteEscSeq returns true.
  let private startInputReader (eventQueue: Collections.Concurrent.ConcurrentQueue<TerminalEvent>) =
    // Channel<int> replaces ConcurrentQueue + Thread.Sleep spin-wait.
    // The raw reader writes into a bounded Channel; tryReadChar does a proper blocking
    // read with cancellation — zero jitter, no wasted CPU cycles.
    let charChannel =
      System.Threading.Channels.Channel.CreateBounded<int>(
        System.Threading.Channels.BoundedChannelOptions(
          4096,
          SingleWriter = true,
          SingleReader = true,
          FullMode = System.Threading.Channels.BoundedChannelFullMode.Wait))
    // Semaphore signals PollEvent the instant an event is enqueued,
    // eliminating the fixed-sleep worst-case latency.
    let eventSignal = new Threading.SemaphoreSlim(0)

    // Raw reader: purely drains Console.In into the channel. Daemon thread; never stops.
    let rawThread = Threading.Thread(fun () ->
      let reader = Console.In
      let writer = charChannel.Writer
      let mutable running = true
      while running do
        try
          let ch = reader.Read()
          writer.WriteAsync(ch).AsTask().Wait()
        with _ -> running <- false)
    rawThread.IsBackground <- true
    rawThread.Name <- "SageTUI.InputRaw"
    rawThread.Start()

    // Try to read a raw char within timeoutMs (-1 = wait indefinitely)
    let tryReadChar (timeoutMs: int) : int =
      let reader = charChannel.Reader
      match timeoutMs with
      | -1 ->
        // Blocking read — wait until a char is available
        try reader.ReadAsync().AsTask().Result
        with _ -> -1
      | 0 ->
        // Non-blocking poll
        let mutable ch = 0
        match reader.TryRead(&ch) with
        | true -> ch
        | false -> -1
      | ms ->
        // Timeout read using CancellationToken
        use cts = new Threading.CancellationTokenSource(ms)
        try reader.ReadAsync(cts.Token).AsTask().Result
        with _ -> -1

    let enqueueEvent (ev: TerminalEvent) =
      eventQueue.Enqueue(ev)
      eventSignal.Release() |> ignore

    // Parse thread: accumulates chars into escape sequences, emits TerminalEvents.
    let parseThread = Threading.Thread(fun () ->
      let buf = Text.StringBuilder()
      let pasteAccum = Text.StringBuilder()
      let mutable inPaste = false
      let escTimeoutMs = 50

      let emitEscape () =
        let seq = buf.ToString()
        buf.Clear() |> ignore
        match seq with
        | "[200~" ->
          // Bracketed paste start — switch to paste accumulation mode
          inPaste <- true
          pasteAccum.Clear() |> ignore
        | "[201~" ->
          // Bracketed paste end — emit collected text
          inPaste <- false
          enqueueEvent (Pasted (pasteAccum.ToString()))
          pasteAccum.Clear() |> ignore
        | _ ->
          AnsiParser.parseEscape seq
          |> Option.defaultValue (KeyPressed(Key.Escape, Modifiers.None))
          |> enqueueEvent

      while true do
        let raw = tryReadChar -1  // block until char arrives
        match raw with
        | -1 | 0 -> ()  // EOF / no-op
        | 27 ->  // ESC — start sequence accumulation
          buf.Clear() |> ignore
          let mutable seqDone = false
          while not seqDone do
            let next = tryReadChar escTimeoutMs
            match next with
            | -1 ->
              // Timeout: bare Escape key (or raw Escape during paste)
              match inPaste with
              | true -> pasteAccum.Append('\x1b') |> ignore
              | false -> enqueueEvent (KeyPressed(Key.Escape, Modifiers.None))
              seqDone <- true
            | c ->
              buf.Append(char c) |> ignore
              let s = buf.ToString()
              match AnsiParser.isCompleteEscSeq s with
              | true ->
                emitEscape()
                seqDone <- true
              | false ->
                // Guard against malformed/overlong sequences
                match s.Length > 64 with
                | true ->
                  enqueueEvent (KeyPressed(Key.Escape, Modifiers.None))
                  seqDone <- true
                | false -> ()
        | c ->
          match inPaste with
          | true ->
            // In paste mode — accumulate raw chars without key routing
            pasteAccum.Append(char c) |> ignore
          | false ->
            let ch = char c
            let event =
              match ch with
              | '\r' -> KeyPressed(Key.Enter,     Modifiers.None)
              | '\b' | '\x7F' -> KeyPressed(Key.Backspace, Modifiers.None)
              | '\t' -> KeyPressed(Key.Tab,       Modifiers.None)
              | c when int c >= 1 && int c <= 26 ->
                // Ctrl-A (1) through Ctrl-Z (26): map to Char 'a'..'z' + Ctrl modifier
                KeyPressed(Key.Char (Text.Rune (char (int c + int 'a' - 1))), Modifiers.Ctrl)
              | c when Char.IsHighSurrogate(c) ->
                // UTF-16 surrogate pair: read the low surrogate and reassemble into a Rune
                let next = tryReadChar escTimeoutMs
                match next > 0 && Char.IsLowSurrogate(char next) with
                | true ->
                  let mutable rune = Unchecked.defaultof<Text.Rune>
                  match Text.Rune.TryCreate(c, char next, &rune) with
                  | true  -> KeyPressed(Key.Char rune, Modifiers.None)  // correct Rune (may be supplementary)
                  | false -> KeyPressed(Key.Char (Text.Rune ' '), Modifiers.None)  // malformed pair
                | false -> KeyPressed(Key.Char (Text.Rune ' '), Modifiers.None)  // lone surrogate
              | c when Char.IsLowSurrogate(c) ->
                // Orphaned low surrogate — should not appear without a prior high surrogate
                KeyPressed(Key.Char (Text.Rune ' '), Modifiers.None)
              | c -> KeyPressed(Key.Char (Text.Rune c), Modifiers.None)
            enqueueEvent event)
    parseThread.IsBackground <- true
    parseThread.Name <- "SageTUI.InputParse"
    parseThread.Start()
    eventSignal

  let create (profile: TerminalProfile) : TerminalBackend =
    let mutable savedModes = Unchecked.defaultof<RawMode.SavedModes>
    let mutable inputStarted = false
    let eventQueue = Collections.Concurrent.ConcurrentQueue<TerminalEvent>()
    // Option-wrapped semaphore: None until EnterRawMode starts the reader thread.
    // Guards PollEvent from a null-deref if called before the backend is initialised.
    let mutable eventSignal : Threading.SemaphoreSlim option = None
    let mutable lastW, lastH = profile.Size
    let currentSize () =
      match tryGetConsoleSize () with
      | Some size -> size
      | None -> lastW, lastH
    let checkResize () =
      let w, h = currentSize ()
      match w <> lastW || h <> lastH with
      | true ->
        lastW <- w
        lastH <- h
        Some (Resized(w, h))
      | false -> None
    { Size = fun () -> currentSize ()
      Write = fun s -> Console.Write(s)
      Flush = fun () -> Console.Out.Flush()
      PollEvent = fun timeoutMs ->
        match checkResize () with
        | Some r -> Some r
        | None ->
          let mutable evt = Unchecked.defaultof<TerminalEvent>
          match eventQueue.TryDequeue(&evt) with
          | true -> Some evt
          | false ->
            match timeoutMs > 0, eventSignal with
            | true, Some sem ->
              // Wait on the semaphore: returns immediately when parse thread enqueues an event,
              // or after timeoutMs if nothing arrives — no fixed-sleep worst-case latency.
              sem.Wait(timeoutMs) |> ignore
              match checkResize () with
              | Some r -> Some r
              | None ->
                match eventQueue.TryDequeue(&evt) with
                | true -> Some evt
                | false -> None
            | true, None ->
              Threading.Thread.Sleep(timeoutMs)
              match checkResize () with
              | Some r -> Some r
              | None ->
                match eventQueue.TryDequeue(&evt) with
                | true -> Some evt
                | false -> None
            | false, _ -> None
      EnterRawMode = fun () ->
        savedModes <- RawMode.enter profile.Platform
        match inputStarted with
        | false ->
          eventSignal <- Some (startInputReader eventQueue)
          inputStarted <- true
        | true -> ()
        // Enable terminal features:
        //   ?1000h — normal mouse button tracking (press/release)
        //   ?1002h — button-event motion tracking (required for DragSub)
        //   ?1006h — SGR extended coordinates (required for columns >223)
        //   ?1004h — OS-level focus in/out events (ESC [ I / ESC [ O)
        //   ?2004h — bracketed paste mode (ESC [ 200~ ... ESC [ 201~)
        Console.Write("\x1b[?1000h\x1b[?1002h\x1b[?1006h\x1b[?1004h\x1b[?2004h")
        // Kitty keyboard protocol: push enhanced mode (flags=1: disambiguate escape codes)
        // Only negotiate with terminals that support it (Kitty, WezTerm, Ghostty, foot, etc.)
        match profile.Input >= InputCapability.KittyKeyboard with
        | true -> Console.Write("\x1b[>1u")
        | false -> ()
        Console.Out.Flush()
      LeaveRawMode = fun () ->
        // Kitty keyboard protocol: pop enhanced mode before disabling other features
        match profile.Input >= InputCapability.KittyKeyboard with
        | true -> Console.Write("\x1b[<u")
        | false -> ()
        Console.Write("\x1b[?2004l\x1b[?1004l\x1b[?1006l\x1b[?1002l\x1b[?1000l")
        Console.Out.Flush()
        RawMode.leave savedModes
      Profile = profile }

  /// Auto-detect terminal capabilities and create a backend. Zero ceremony.
  let auto () : TerminalBackend =
    let envReader k = System.Environment.GetEnvironmentVariable(k) |> Option.ofObj
    let sizeGetter () = safeInitialSize envReader
    let profile =
      Detect.fromEnvironment envReader sizeGetter
      |> Detect.adjustForMultiplexer envReader
      |> UserOverride.apply envReader
    create profile
