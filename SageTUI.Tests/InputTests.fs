module SageTUI.InputTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ─────────────────────────────────────────────────────────────────────────────
// AnsiParser.parseSgrMouse — press, release, motion, modifiers, phase
// ─────────────────────────────────────────────────────────────────────────────

let parseSgrMouseTests = testList "AnsiParser.parseSgrMouse" [

  testCase "left button press" <| fun () ->
    AnsiParser.parseSgrMouse "<0;10;5M"
    |> Expect.equal "left press" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed })

  testCase "left button release has Released phase" <| fun () ->
    AnsiParser.parseSgrMouse "<0;10;5m"
    |> Expect.equal "left release" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Released })

  testCase "press and release are distinguishable" <| fun () ->
    let press   = AnsiParser.parseSgrMouse "<0;5;5M" |> Option.map (fun e -> e.Phase)
    let release = AnsiParser.parseSgrMouse "<0;5;5m" |> Option.map (fun e -> e.Phase)
    press   |> Expect.equal "press phase"   (Some Pressed)
    release |> Expect.equal "release phase" (Some Released)

  testCase "motion event (btn=32) has Motion phase with LeftButton" <| fun () ->
    // btn=32 = bit 5 set (motion flag); button bits (0-1) = 0 → LeftButton held
    AnsiParser.parseSgrMouse "<32;10;5M"
    |> Expect.equal "left drag"
         (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Motion })

  testCase "motion event (btn=33) has Motion phase with MiddleButton" <| fun () ->
    AnsiParser.parseSgrMouse "<33;10;5M"
    |> Expect.equal "middle drag"
         (Some { Button = MiddleButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Motion })

  testCase "motion event (btn=34) has Motion phase with RightButton" <| fun () ->
    AnsiParser.parseSgrMouse "<34;10;5M"
    |> Expect.equal "right drag"
         (Some { Button = RightButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Motion })

  testCase "middle button" <| fun () ->
    AnsiParser.parseSgrMouse "<1;10;5M"
    |> Expect.equal "middle" (Some { Button = MiddleButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed })

  testCase "right button" <| fun () ->
    AnsiParser.parseSgrMouse "<2;10;5M"
    |> Expect.equal "right" (Some { Button = RightButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed })

  testCase "scroll up (btn=64)" <| fun () ->
    AnsiParser.parseSgrMouse "<64;10;5M"
    |> Expect.equal "scroll up" (Some { Button = ScrollUp; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed })

  testCase "scroll down (btn=65)" <| fun () ->
    AnsiParser.parseSgrMouse "<65;10;5M"
    |> Expect.equal "scroll down" (Some { Button = ScrollDown; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed })

  testCase "shift modifier (btn=4)" <| fun () ->
    AnsiParser.parseSgrMouse "<4;10;5M"
    |> Expect.equal "shift+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Shift; Phase = Pressed })

  testCase "alt modifier (btn=8)" <| fun () ->
    AnsiParser.parseSgrMouse "<8;10;5M"
    |> Expect.equal "alt+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Alt; Phase = Pressed })

  testCase "ctrl modifier (btn=16)" <| fun () ->
    AnsiParser.parseSgrMouse "<16;10;5M"
    |> Expect.equal "ctrl+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Ctrl; Phase = Pressed })

  testCase "shift+ctrl (btn=20)" <| fun () ->
    AnsiParser.parseSgrMouse "<20;10;5M"
    |> Expect.equal "shift+ctrl+left"
         (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Shift ||| Modifiers.Ctrl; Phase = Pressed })

  testCase "scroll up + shift (btn=68)" <| fun () ->
    // 64 + 4 = 68: scroll flag + shift
    AnsiParser.parseSgrMouse "<68;5;5M"
    |> Option.map (fun e -> e.Button, e.Modifiers)
    |> Expect.equal "scroll+shift" (Some (ScrollUp, Modifiers.Shift))

  testCase "scroll down + ctrl (btn=81)" <| fun () ->
    // 64 + 16 + 1 = 81: scroll flag + ctrl + direction-bit
    AnsiParser.parseSgrMouse "<81;5;5M"
    |> Option.map (fun e -> e.Button, e.Modifiers)
    |> Expect.equal "scroll+ctrl" (Some (ScrollDown, Modifiers.Ctrl))

  testCase "minimum position (1,1) → (0,0)" <| fun () ->
    AnsiParser.parseSgrMouse "<0;1;1M"
    |> Option.map (fun e -> e.X, e.Y)
    |> Expect.equal "min pos" (Some (0, 0))

  testCase "large coordinates" <| fun () ->
    AnsiParser.parseSgrMouse "<0;220;50M"
    |> Option.map (fun e -> e.X, e.Y)
    |> Expect.equal "large coords" (Some (219, 49))

  testCase "missing '<' returns None" <| fun () ->
    AnsiParser.parseSgrMouse "0;10;5M"
    |> Expect.isNone "missing <"

  testCase "garbage input returns None" <| fun () ->
    AnsiParser.parseSgrMouse "garbage"
    |> Expect.isNone "garbage"

  testCase "empty string returns None" <| fun () ->
    AnsiParser.parseSgrMouse ""
    |> Expect.isNone "empty"

  testCase "non-mouse terminator returns None" <| fun () ->
    AnsiParser.parseSgrMouse "<0;10;5A"
    |> Expect.isNone "wrong terminator"

  testProperty "coordinates are 1-indexed in SGR, 0-indexed in MouseEvent" <| fun (x: PositiveInt) (y: PositiveInt) ->
    let x1 = (x.Get % 220) + 1  // 1..220
    let y1 = (y.Get % 50) + 1   // 1..50
    let body = sprintf "<0;%d;%dM" x1 y1
    AnsiParser.parseSgrMouse body
    |> Option.map (fun e -> e.X, e.Y)
    |> Expect.equal "0-indexed" (Some (x1 - 1, y1 - 1))

  testProperty "scroll wheel: btn&64 selects scroll, btn&1 picks direction, modifiers independent" <|
    fun (isDown: bool) (shift: bool) (ctrl: bool) (x: PositiveInt) (y: PositiveInt) ->
      let dirBit = if isDown then 1 else 0
      let modBits = (if shift then 4 else 0) ||| (if ctrl then 16 else 0)
      let btn = 64 + dirBit + modBits
      let x1 = (x.Get % 200) + 1
      let y1 = (y.Get % 50) + 1
      let expectedBtn = if isDown then ScrollDown else ScrollUp
      let expectedMods =
        (if shift then Modifiers.Shift else Modifiers.None) |||
        (if ctrl  then Modifiers.Ctrl  else Modifiers.None)
      match AnsiParser.parseSgrMouse (sprintf "<%d;%d;%dM" btn x1 y1) with
      | None -> false  // parse failure = test failure
      | Some e -> e.Button = expectedBtn && e.Modifiers = expectedMods

  testProperty "motion bit (32) sets Phase=Motion, doesn't change button identity" <|
    fun (btnBase: int) (x: PositiveInt) (y: PositiveInt) ->
      // Force 0..2 using positive modulo (F# % can return negative for negative inputs)
      let b = ((btnBase % 3) + 3) % 3
      let plain  = sprintf "<%d;%d;%dM" b        ((x.Get % 200) + 1) ((y.Get % 50) + 1)
      let motion = sprintf "<%d;%d;%dM" (b + 32) ((x.Get % 200) + 1) ((y.Get % 50) + 1)
      match AnsiParser.parseSgrMouse plain, AnsiParser.parseSgrMouse motion with
      | Some p, Some m -> p.Button = m.Button && p.Phase = Pressed && m.Phase = Motion
      | _ -> false
]

// ─────────────────────────────────────────────────────────────────────────────
// AnsiParser.isCompleteEscSeq
// ─────────────────────────────────────────────────────────────────────────────

let isCompleteEscSeqTests = testList "AnsiParser.isCompleteEscSeq" [

  testCase "empty is incomplete" <| fun () ->
    AnsiParser.isCompleteEscSeq "" |> Expect.isFalse "empty"

  testCase "[A (cursor up) is complete" <| fun () ->
    AnsiParser.isCompleteEscSeq "[A" |> Expect.isTrue "[A"

  testCase "[2~ (insert) is complete" <| fun () ->
    AnsiParser.isCompleteEscSeq "[2~" |> Expect.isTrue "[2~"

  testCase "[<0;10;5M (SGR mouse) is complete" <| fun () ->
    AnsiParser.isCompleteEscSeq "[<0;10;5M" |> Expect.isTrue "SGR mouse"

  testCase "[ alone is incomplete" <| fun () ->
    AnsiParser.isCompleteEscSeq "[" |> Expect.isFalse "[ alone"

  testCase "[<0;10 is incomplete" <| fun () ->
    AnsiParser.isCompleteEscSeq "[<0;10" |> Expect.isFalse "partial SGR"

  testCase "OP (F1 SS3) is complete" <| fun () ->
    AnsiParser.isCompleteEscSeq "OP" |> Expect.isTrue "OP"

  testCase "O alone is incomplete" <| fun () ->
    AnsiParser.isCompleteEscSeq "O" |> Expect.isFalse "O alone"

  testCase "single char Alt+key is complete" <| fun () ->
    AnsiParser.isCompleteEscSeq "a" |> Expect.isTrue "alt+a"
]

// ─────────────────────────────────────────────────────────────────────────────
// AnsiParser.parseEscape — including Alt+key and via-defaultValue path
// ─────────────────────────────────────────────────────────────────────────────

let parseEscapeTests = testList "AnsiParser.parseEscape" [

  testCase "arrow keys" <| fun () ->
    AnsiParser.parseEscape "[A" |> Expect.equal "up"    (Some (KeyPressed(Key.Up,    Modifiers.None)))
    AnsiParser.parseEscape "[B" |> Expect.equal "down"  (Some (KeyPressed(Key.Down,  Modifiers.None)))
    AnsiParser.parseEscape "[C" |> Expect.equal "right" (Some (KeyPressed(Key.Right, Modifiers.None)))
    AnsiParser.parseEscape "[D" |> Expect.equal "left"  (Some (KeyPressed(Key.Left,  Modifiers.None)))

  testCase "home variants" <| fun () ->
    AnsiParser.parseEscape "[H"  |> Expect.equal "[H"  (Some (KeyPressed(Key.Home, Modifiers.None)))
    AnsiParser.parseEscape "[1~" |> Expect.equal "[1~" (Some (KeyPressed(Key.Home, Modifiers.None)))

  testCase "end variants" <| fun () ->
    AnsiParser.parseEscape "[F"  |> Expect.equal "[F"  (Some (KeyPressed(Key.End, Modifiers.None)))
    AnsiParser.parseEscape "[4~" |> Expect.equal "[4~" (Some (KeyPressed(Key.End, Modifiers.None)))

  testCase "navigation keys" <| fun () ->
    AnsiParser.parseEscape "[2~" |> Expect.equal "ins"  (Some (KeyPressed(Key.Insert,  Modifiers.None)))
    AnsiParser.parseEscape "[3~" |> Expect.equal "del"  (Some (KeyPressed(Key.Delete,  Modifiers.None)))
    AnsiParser.parseEscape "[5~" |> Expect.equal "pgup" (Some (KeyPressed(Key.PageUp,  Modifiers.None)))
    AnsiParser.parseEscape "[6~" |> Expect.equal "pgdn" (Some (KeyPressed(Key.PageDown,Modifiers.None)))

  testCase "function keys F1-F4 (SS3)" <| fun () ->
    AnsiParser.parseEscape "OP" |> Expect.equal "F1" (Some (KeyPressed(Key.F 1, Modifiers.None)))
    AnsiParser.parseEscape "OQ" |> Expect.equal "F2" (Some (KeyPressed(Key.F 2, Modifiers.None)))
    AnsiParser.parseEscape "OR" |> Expect.equal "F3" (Some (KeyPressed(Key.F 3, Modifiers.None)))
    AnsiParser.parseEscape "OS" |> Expect.equal "F4" (Some (KeyPressed(Key.F 4, Modifiers.None)))

  testCase "function keys F5-F12 (CSI)" <| fun () ->
    AnsiParser.parseEscape "[15~" |> Expect.equal "F5"  (Some (KeyPressed(Key.F 5,  Modifiers.None)))
    AnsiParser.parseEscape "[17~" |> Expect.equal "F6"  (Some (KeyPressed(Key.F 6,  Modifiers.None)))
    AnsiParser.parseEscape "[24~" |> Expect.equal "F12" (Some (KeyPressed(Key.F 12, Modifiers.None)))

  // ── Alt+key path: the P0 that was broken (ESC+'a' → bare Escape) ──────────

  testCase "Alt+a (single char after ESC) produces Alt modifier" <| fun () ->
    AnsiParser.parseEscape "a"
    |> Expect.equal "alt+a" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Alt)))

  testCase "Alt+b produces Alt modifier" <| fun () ->
    AnsiParser.parseEscape "b"
    |> Expect.equal "alt+b" (Some (KeyPressed(Key.Char (System.Text.Rune 'b'), Modifiers.Alt)))

  testCase "Alt+f produces Alt modifier (readline forward-word)" <| fun () ->
    AnsiParser.parseEscape "f"
    |> Expect.equal "alt+f" (Some (KeyPressed(Key.Char (System.Text.Rune 'f'), Modifiers.Alt)))

  testCase "Alt+key via defaultValue does NOT produce bare Escape" <| fun () ->
    // This is the exact emitEscape→parseEscape→defaultValue path that was broken.
    // parseEscape "a" must return Some, so defaultValue never fires.
    let result =
      AnsiParser.parseEscape "a"
      |> Option.defaultValue (KeyPressed(Key.Escape, Modifiers.None))
    result |> Expect.equal "no bare escape" (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Alt))

  testProperty "all printable ASCII chars produce Alt+char when single" <| fun (c: char) ->
    let c = char ((int c % 94) + 33)  // printable ASCII 33-126
    match AnsiParser.parseEscape (string c) with
    | Some (KeyPressed(Key.Char ch, Modifiers.Alt)) -> ch = System.Text.Rune c
    | _ -> false

  // ── SGR mouse ─────────────────────────────────────────────────────────────

  testCase "SGR mouse press (M) in CSI body — Phase=Pressed" <| fun () ->
    AnsiParser.parseEscape "[<0;10;5M"
    |> Expect.equal "mouse press"
         (Some (MouseInput { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Pressed }))

  testCase "SGR mouse release (m) in CSI body — Phase=Released" <| fun () ->
    AnsiParser.parseEscape "[<0;10;5m"
    |> Expect.equal "mouse release"
         (Some (MouseInput { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None; Phase = Released }))

  testCase "SGR scroll up via parseEscape" <| fun () ->
    AnsiParser.parseEscape "[<64;1;1M"
    |> Option.map (function MouseInput e -> e.Button | _ -> ScrollDown)
    |> Expect.equal "scroll up" (Some ScrollUp)

  testCase "SGR drag (btn=32) via parseEscape — Phase=Motion" <| fun () ->
    AnsiParser.parseEscape "[<32;5;5M"
    |> Option.map (function MouseInput e -> e.Phase | _ -> Pressed)
    |> Expect.equal "drag phase" (Some Motion)

  testCase "unknown sequence returns None" <| fun () ->
    AnsiParser.parseEscape "[Z" |> Expect.isNone "unknown [Z"

  testCase "empty returns None" <| fun () ->
    AnsiParser.parseEscape "" |> Expect.isNone "empty"
]

// ─────────────────────────────────────────────────────────────────────────────
// Mouse dispatch via TestBackend — App.fs wiring
// ─────────────────────────────────────────────────────────────────────────────

let mouseDispatchTests = testList "Mouse dispatch" [

  testCase "MouseSub receives MouseInput from PollEvent" <| fun () ->
    let mouseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let events = [ MouseInput mouseEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable received : MouseEvent list = []
    let program : Program<unit, MouseEvent option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some me -> received <- me :: received
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ MouseSub (fun me -> Some (Some me))
            KeySub (fun (k, _) ->
              match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    received |> List.rev
    |> Expect.equal "received events" [ mouseEvent ]

  testCase "ClickSub receives MouseInput with hit key from PollEvent" <| fun () ->
    let mouseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let events = [ MouseInput mouseEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable received : (MouseEvent * string option) list = []
    let program : Program<unit, (MouseEvent * string option) option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some pair -> received <- pair :: received
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ ClickSub (fun (me, hitKey) -> Some (Some (me, hitKey)))
            KeySub (fun (k, _) ->
              match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    match received with
    | (me, _) :: _ -> me |> Expect.equal "mouse event" mouseEvent
    | [] -> failtest "expected mouse event"

  testCase "MouseSub not called for KeyPressed events" <| fun () ->
    let events = [ KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None); KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable mouseCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> mouseCount <- mouseCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ MouseSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    mouseCount |> Expect.equal "no mouse events" 0
]

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 32: Mouse drag coalescing and DragSub
// ─────────────────────────────────────────────────────────────────────────────

let dragSubTests = testList "Sprint 32: DragSub and motion coalescing" [

  testCase "DragSub fires for Motion phase" <| fun () ->
    let motionEvent = { Button = LeftButton; X = 10; Y = 5; Modifiers = Modifiers.None; Phase = Motion }
    let events = [ MouseInput motionEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable dragCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> dragCount <- dragCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ DragSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    dragCount |> Expect.equal "drag fired for motion" 1

  testCase "DragSub does NOT fire for Pressed phase" <| fun () ->
    let pressEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let events = [ MouseInput pressEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable dragCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> dragCount <- dragCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ DragSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    dragCount |> Expect.equal "drag not fired for press" 0

  testCase "Multiple consecutive Motion events coalesced to last position" <| fun () ->
    let motion x y = MouseInput { Button = LeftButton; X = x; Y = y; Modifiers = Modifiers.None; Phase = Motion }
    let events = [ motion 1 1; motion 2 2; motion 3 3; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable positions : (int * int) list = []
    let program : Program<unit, (int * int) option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some(x, y) -> positions <- (x, y) :: positions
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ DragSub (fun me -> Some (Some (me.X, me.Y)))
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    // After coalescing, only the last Motion position should arrive
    positions |> List.rev |> Expect.equal "only last motion retained" [(3, 3)]

  testCase "Motion events from different buttons not coalesced" <| fun () ->
    let motionL = MouseInput { Button = LeftButton;  X = 1; Y = 1; Modifiers = Modifiers.None; Phase = Motion }
    let motionR = MouseInput { Button = RightButton; X = 2; Y = 2; Modifiers = Modifiers.None; Phase = Motion }
    let events = [ motionL; motionR; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable dragCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> dragCount <- dragCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ DragSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    dragCount |> Expect.equal "both buttons fire" 2
]



let bracketedPasteTests = testList "Sprint 32: bracketed paste" [

  testCase "Pasted event dispatched to PasteSub" <| fun () ->
    let events = [ Pasted "hello world"; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable received : string list = []
    let program : Program<unit, unit> =
      { Init = fun () -> (), NoCmd
        Update = fun () () -> (), Quit 0
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ PasteSub (fun text -> received <- text :: received; None)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some () | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    received |> Expect.equal "paste received" ["hello world"]

  testCase "PasteSub does not fire for key events" <| fun () ->
    let events = [ KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None); KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable pasteCount = 0
    let program : Program<unit, unit> =
      { Init = fun () -> (), NoCmd
        Update = fun () () -> (), Quit 0
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ PasteSub (fun _ -> pasteCount <- pasteCount + 1; None)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some () | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    pasteCount |> Expect.equal "no paste for key event" 0

  testCase "Sub.map transforms PasteSub messages" <| fun () ->
    let inner : Sub<string> = PasteSub (fun text -> Some text)
    let outer : Sub<int> = Sub.map String.length inner
    match outer with
    | PasteSub handler ->
      handler "hello" |> Expect.equal "length mapped" (Some 5)
    | _ -> failtest "expected PasteSub"
]

// ─────────────────────────────────────────────────────────────────────────────
// Sprint 30: Phase filtering, DragSub, TerminalFocusSub, focus parse
// ─────────────────────────────────────────────────────────────────────────────

let sprintThirtyTests = testList "Sprint 30: phase filtering and new subs" [

  testCase "parseEscape [I → FocusGained" <| fun () ->
    AnsiParser.parseEscape "[I" |> Expect.equal "focus gained" (Some FocusGained)

  testCase "parseEscape [O → FocusLost" <| fun () ->
    AnsiParser.parseEscape "[O" |> Expect.equal "focus lost" (Some FocusLost)

  testCase "ClickSub does NOT fire for Released phase" <| fun () ->
    let releaseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Released }
    let events = [ MouseInput releaseEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable clickCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> clickCount <- clickCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ ClickSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    clickCount |> Expect.equal "click not fired for release" 0

  testCase "ClickSub fires for Pressed phase" <| fun () ->
    let pressEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let events = [ MouseInput pressEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable clickCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> clickCount <- clickCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ ClickSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    clickCount |> Expect.equal "click fired for press" 1

  testCase "MouseSub does NOT fire for Motion phase" <| fun () ->
    let motionEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Motion }
    let events = [ MouseInput motionEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable mouseCount = 0
    let program : Program<unit, bool> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | true -> mouseCount <- mouseCount + 1
          | false -> ()
          (), if not msg then Quit 0 else NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ MouseSub (fun _ -> Some true)
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    mouseCount |> Expect.equal "MouseSub not fired for motion" 0

  testCase "MouseSub fires for Pressed and Released" <| fun () ->
    let pressEvent   = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let releaseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Released }
    let events = [ MouseInput pressEvent; MouseInput releaseEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable phases : MousePhase list = []
    let program : Program<unit, MousePhase option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some p -> phases <- p :: phases
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ MouseSub (fun me -> Some (Some me.Phase))
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    phases |> List.rev |> Expect.equal "press then release" [ Pressed; Released ]

  testCase "DragSub fires only for Motion phase" <| fun () ->
    let pressEvent  = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None; Phase = Pressed }
    let motionEvent = { Button = LeftButton; X = 6; Y = 3; Modifiers = Modifiers.None; Phase = Motion }
    let events = [ MouseInput pressEvent; MouseInput motionEvent; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable positions : (int * int) list = []
    let program : Program<unit, (int * int) option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some xy -> positions <- xy :: positions
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ DragSub (fun me -> Some (Some (me.X, me.Y)))
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    positions |> List.rev |> Expect.equal "only motion event" [ (6, 3) ]

  testCase "TerminalFocusSub receives FocusGained and FocusLost" <| fun () ->
    let events = [ FocusGained; FocusLost; KeyPressed(Key.Escape, Modifiers.None) ]
    let backend, _ = TestBackend.create 80 24 events
    let mutable focusStates : bool list = []
    let program : Program<unit, bool option> =
      { Init = fun () -> (), NoCmd
        Update = fun msg () ->
          match msg with
          | Some b -> focusStates <- b :: focusStates
          | None -> ()
          (), match msg with None -> Quit 0 | _ -> NoCmd
        View = fun () -> El.empty
        Subscribe = fun _ ->
          [ TerminalFocusSub (fun gained -> Some (Some gained))
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ]
        OnError = CrashOnError }
    App.runWithBackend backend program
    focusStates |> List.rev |> Expect.equal "gained then lost" [ true; false ]
]


let parseEscapeModifierTests = testList "AnsiParser.parseEscape modifier keys" [
  // CSI modifier format: "[1;<mod>A" where mod = 2=Shift, 3=Alt, 5=Ctrl, 7=Alt+Ctrl
  // Arrows with modifiers
  testCase "Ctrl+Up = [1;5A" <| fun () ->
    AnsiParser.parseEscape "[1;5A" |> Expect.equal "ctrl+up" (Some (KeyPressed(Key.Up, Modifiers.Ctrl)))
  testCase "Shift+Up = [1;2A" <| fun () ->
    AnsiParser.parseEscape "[1;2A" |> Expect.equal "shift+up" (Some (KeyPressed(Key.Up, Modifiers.Shift)))
  testCase "Alt+Up = [1;3A" <| fun () ->
    AnsiParser.parseEscape "[1;3A" |> Expect.equal "alt+up" (Some (KeyPressed(Key.Up, Modifiers.Alt)))
  testCase "Ctrl+Down = [1;5B" <| fun () ->
    AnsiParser.parseEscape "[1;5B" |> Expect.equal "ctrl+down" (Some (KeyPressed(Key.Down, Modifiers.Ctrl)))
  testCase "Ctrl+Left = [1;5D" <| fun () ->
    AnsiParser.parseEscape "[1;5D" |> Expect.equal "ctrl+left" (Some (KeyPressed(Key.Left, Modifiers.Ctrl)))
  testCase "Ctrl+Right = [1;5C" <| fun () ->
    AnsiParser.parseEscape "[1;5C" |> Expect.equal "ctrl+right" (Some (KeyPressed(Key.Right, Modifiers.Ctrl)))
  testCase "Shift+Ctrl+Right = [1;6C" <| fun () ->
    AnsiParser.parseEscape "[1;6C" |> Expect.equal "shift+ctrl+right" (Some (KeyPressed(Key.Right, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Shift+Left = [1;2D" <| fun () ->
    AnsiParser.parseEscape "[1;2D" |> Expect.equal "shift+left" (Some (KeyPressed(Key.Left, Modifiers.Shift)))
  testCase "Ctrl+Home = [1;5H" <| fun () ->
    AnsiParser.parseEscape "[1;5H" |> Expect.equal "ctrl+home" (Some (KeyPressed(Key.Home, Modifiers.Ctrl)))
  testCase "Ctrl+End = [1;5F" <| fun () ->
    AnsiParser.parseEscape "[1;5F" |> Expect.equal "ctrl+end" (Some (KeyPressed(Key.End, Modifiers.Ctrl)))
  // Function keys with modifiers
  testCase "Ctrl+F5 = [15;5~" <| fun () ->
    AnsiParser.parseEscape "[15;5~" |> Expect.equal "ctrl+f5" (Some (KeyPressed(Key.F 5, Modifiers.Ctrl)))
  testCase "Shift+F6 = [17;2~" <| fun () ->
    AnsiParser.parseEscape "[17;2~" |> Expect.equal "shift+f6" (Some (KeyPressed(Key.F 6, Modifiers.Shift)))
  testCase "Ctrl+Delete = [3;5~" <| fun () ->
    AnsiParser.parseEscape "[3;5~" |> Expect.equal "ctrl+del" (Some (KeyPressed(Key.Delete, Modifiers.Ctrl)))
  testCase "Shift+PageUp = [5;2~" <| fun () ->
    AnsiParser.parseEscape "[5;2~" |> Expect.equal "shift+pgup" (Some (KeyPressed(Key.PageUp, Modifiers.Shift)))
  // Existing bare sequences still work (no regression)
  testCase "bare [A still works" <| fun () ->
    AnsiParser.parseEscape "[A" |> Expect.equal "up" (Some (KeyPressed(Key.Up, Modifiers.None)))
  testCase "bare [D still works" <| fun () ->
    AnsiParser.parseEscape "[D" |> Expect.equal "left" (Some (KeyPressed(Key.Left, Modifiers.None)))

  // ── Complete modifier × direction matrix (all 7 modifier codes × 4 arrows) ──
  // Missing entries from the original set: Shift+Down, Alt+Down, Alt+Left,
  // Alt+Right, Shift+Right, and all Shift+Ctrl / Alt+Ctrl combos.

  testCase "Shift+Down = [1;2B" <| fun () ->
    AnsiParser.parseEscape "[1;2B" |> Expect.equal "shift+down" (Some (KeyPressed(Key.Down, Modifiers.Shift)))
  testCase "Alt+Down = [1;3B" <| fun () ->
    AnsiParser.parseEscape "[1;3B" |> Expect.equal "alt+down" (Some (KeyPressed(Key.Down, Modifiers.Alt)))
  testCase "Shift+Alt+Down = [1;4B" <| fun () ->
    AnsiParser.parseEscape "[1;4B" |> Expect.equal "shift+alt+down" (Some (KeyPressed(Key.Down, Modifiers.Shift ||| Modifiers.Alt)))
  testCase "Shift+Ctrl+Down = [1;6B" <| fun () ->
    AnsiParser.parseEscape "[1;6B" |> Expect.equal "shift+ctrl+down" (Some (KeyPressed(Key.Down, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Alt+Ctrl+Down = [1;7B" <| fun () ->
    AnsiParser.parseEscape "[1;7B" |> Expect.equal "alt+ctrl+down" (Some (KeyPressed(Key.Down, Modifiers.Alt ||| Modifiers.Ctrl)))
  testCase "Shift+Alt+Ctrl+Down = [1;8B" <| fun () ->
    AnsiParser.parseEscape "[1;8B" |> Expect.equal "shift+alt+ctrl+down" (Some (KeyPressed(Key.Down, Modifiers.Shift ||| Modifiers.Alt ||| Modifiers.Ctrl)))

  testCase "Alt+Left = [1;3D" <| fun () ->
    AnsiParser.parseEscape "[1;3D" |> Expect.equal "alt+left" (Some (KeyPressed(Key.Left, Modifiers.Alt)))
  testCase "Shift+Alt+Left = [1;4D" <| fun () ->
    AnsiParser.parseEscape "[1;4D" |> Expect.equal "shift+alt+left" (Some (KeyPressed(Key.Left, Modifiers.Shift ||| Modifiers.Alt)))
  testCase "Shift+Ctrl+Left = [1;6D" <| fun () ->
    AnsiParser.parseEscape "[1;6D" |> Expect.equal "shift+ctrl+left" (Some (KeyPressed(Key.Left, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Alt+Ctrl+Left = [1;7D" <| fun () ->
    AnsiParser.parseEscape "[1;7D" |> Expect.equal "alt+ctrl+left" (Some (KeyPressed(Key.Left, Modifiers.Alt ||| Modifiers.Ctrl)))

  testCase "Shift+Right = [1;2C" <| fun () ->
    AnsiParser.parseEscape "[1;2C" |> Expect.equal "shift+right" (Some (KeyPressed(Key.Right, Modifiers.Shift)))
  testCase "Alt+Right = [1;3C" <| fun () ->
    AnsiParser.parseEscape "[1;3C" |> Expect.equal "alt+right" (Some (KeyPressed(Key.Right, Modifiers.Alt)))
  testCase "Shift+Alt+Right = [1;4C" <| fun () ->
    AnsiParser.parseEscape "[1;4C" |> Expect.equal "shift+alt+right" (Some (KeyPressed(Key.Right, Modifiers.Shift ||| Modifiers.Alt)))
  testCase "Shift+Ctrl+Up = [1;6A" <| fun () ->
    AnsiParser.parseEscape "[1;6A" |> Expect.equal "shift+ctrl+up" (Some (KeyPressed(Key.Up, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Alt+Ctrl+Up = [1;7A" <| fun () ->
    AnsiParser.parseEscape "[1;7A" |> Expect.equal "alt+ctrl+up" (Some (KeyPressed(Key.Up, Modifiers.Alt ||| Modifiers.Ctrl)))
  testCase "Alt+Ctrl+Right = [1;7C" <| fun () ->
    AnsiParser.parseEscape "[1;7C" |> Expect.equal "alt+ctrl+right" (Some (KeyPressed(Key.Right, Modifiers.Alt ||| Modifiers.Ctrl)))

  // ── Home / End with modifiers ─────────────────────────────────────────────
  testCase "Shift+Home = [1;2H" <| fun () ->
    AnsiParser.parseEscape "[1;2H" |> Expect.equal "shift+home" (Some (KeyPressed(Key.Home, Modifiers.Shift)))
  testCase "Alt+Home = [1;3H" <| fun () ->
    AnsiParser.parseEscape "[1;3H" |> Expect.equal "alt+home" (Some (KeyPressed(Key.Home, Modifiers.Alt)))
  testCase "Shift+Ctrl+Home = [1;6H" <| fun () ->
    AnsiParser.parseEscape "[1;6H" |> Expect.equal "shift+ctrl+home" (Some (KeyPressed(Key.Home, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Shift+End = [1;2F" <| fun () ->
    AnsiParser.parseEscape "[1;2F" |> Expect.equal "shift+end" (Some (KeyPressed(Key.End, Modifiers.Shift)))
  testCase "Alt+End = [1;3F" <| fun () ->
    AnsiParser.parseEscape "[1;3F" |> Expect.equal "alt+end" (Some (KeyPressed(Key.End, Modifiers.Alt)))
  testCase "Shift+Ctrl+End = [1;6F" <| fun () ->
    AnsiParser.parseEscape "[1;6F" |> Expect.equal "shift+ctrl+end" (Some (KeyPressed(Key.End, Modifiers.Shift ||| Modifiers.Ctrl)))

  // ── F1-F4 with modifiers (SS3 modifier form [1;nP/Q/R/S) ─────────────────
  testCase "Ctrl+F1 = [1;5P" <| fun () ->
    AnsiParser.parseEscape "[1;5P" |> Expect.equal "ctrl+f1" (Some (KeyPressed(Key.F 1, Modifiers.Ctrl)))
  testCase "Shift+F2 = [1;2Q" <| fun () ->
    AnsiParser.parseEscape "[1;2Q" |> Expect.equal "shift+f2" (Some (KeyPressed(Key.F 2, Modifiers.Shift)))
  testCase "Alt+Ctrl+F3 = [1;7R" <| fun () ->
    AnsiParser.parseEscape "[1;7R" |> Expect.equal "alt+ctrl+f3" (Some (KeyPressed(Key.F 3, Modifiers.Alt ||| Modifiers.Ctrl)))
  testCase "Shift+F4 = [1;2S" <| fun () ->
    AnsiParser.parseEscape "[1;2S" |> Expect.equal "shift+f4" (Some (KeyPressed(Key.F 4, Modifiers.Shift)))

  // ── F7-F12 with modifiers ─────────────────────────────────────────────────
  testCase "Ctrl+F7 = [18;5~" <| fun () ->
    AnsiParser.parseEscape "[18;5~" |> Expect.equal "ctrl+f7" (Some (KeyPressed(Key.F 7, Modifiers.Ctrl)))
  testCase "Shift+F8 = [19;2~" <| fun () ->
    AnsiParser.parseEscape "[19;2~" |> Expect.equal "shift+f8" (Some (KeyPressed(Key.F 8, Modifiers.Shift)))
  testCase "Alt+F9 = [20;3~" <| fun () ->
    AnsiParser.parseEscape "[20;3~" |> Expect.equal "alt+f9" (Some (KeyPressed(Key.F 9, Modifiers.Alt)))
  testCase "Ctrl+F10 = [21;5~" <| fun () ->
    AnsiParser.parseEscape "[21;5~" |> Expect.equal "ctrl+f10" (Some (KeyPressed(Key.F 10, Modifiers.Ctrl)))
  testCase "Shift+F11 = [23;2~" <| fun () ->
    AnsiParser.parseEscape "[23;2~" |> Expect.equal "shift+f11" (Some (KeyPressed(Key.F 11, Modifiers.Shift)))
  testCase "Ctrl+F12 = [24;5~" <| fun () ->
    AnsiParser.parseEscape "[24;5~" |> Expect.equal "ctrl+f12" (Some (KeyPressed(Key.F 12, Modifiers.Ctrl)))

  // ── Insert / Delete / PageUp / PageDown with modifiers ────────────────────
  testCase "Shift+Insert = [2;2~" <| fun () ->
    AnsiParser.parseEscape "[2;2~" |> Expect.equal "shift+insert" (Some (KeyPressed(Key.Insert, Modifiers.Shift)))
  testCase "Ctrl+Insert = [2;5~" <| fun () ->
    AnsiParser.parseEscape "[2;5~" |> Expect.equal "ctrl+insert" (Some (KeyPressed(Key.Insert, Modifiers.Ctrl)))
  testCase "Shift+Ctrl+Delete = [3;6~" <| fun () ->
    AnsiParser.parseEscape "[3;6~" |> Expect.equal "shift+ctrl+del" (Some (KeyPressed(Key.Delete, Modifiers.Shift ||| Modifiers.Ctrl)))
  testCase "Alt+PageDown = [6;3~" <| fun () ->
    AnsiParser.parseEscape "[6;3~" |> Expect.equal "alt+pgdown" (Some (KeyPressed(Key.PageDown, Modifiers.Alt)))
  testCase "Shift+Ctrl+PageUp = [5;6~" <| fun () ->
    AnsiParser.parseEscape "[5;6~" |> Expect.equal "shift+ctrl+pgup" (Some (KeyPressed(Key.PageUp, Modifiers.Shift ||| Modifiers.Ctrl)))

  // Unknown modifier-qualified sequence returns None (not bare Escape)
  testCase "unknown base with modifier returns None" <| fun () ->
    AnsiParser.parseEscape "[9;5~" |> Expect.isNone "unknown [9;5~"
]

/// Kitty keyboard protocol CSI-u format: ESC [ <codepoint> ; <modifiers> : <event-type> u
/// Full spec: https://sw.kovidgoyal.net/kitty/keyboard-protocol/
/// Modifier encoding: bits from (value - 1): bit0=Shift, bit1=Alt, bit2=Ctrl, bit3=Meta
/// Event types: 1=press, 2=repeat, 3=release
let kittyKeyTests = testList "Sprint 33: Kitty keyboard protocol" [

  // ── Basic CSI-u sequences ────────────────────────────────────────────────────

  testCase "CSI 97 u → Char 'a' Press" <| fun () ->
    AnsiParser.parseEscape "[97u"
    |> Expect.equal "plain 'a'" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None)))

  testCase "CSI 13 u → Enter Press" <| fun () ->
    AnsiParser.parseEscape "[13u"
    |> Expect.equal "Enter" (Some (KeyPressed(Key.Enter, Modifiers.None)))

  testCase "CSI 27 u → Escape Press" <| fun () ->
    AnsiParser.parseEscape "[27u"
    |> Expect.equal "Escape" (Some (KeyPressed(Key.Escape, Modifiers.None)))

  testCase "CSI 127 u → Backspace Press" <| fun () ->
    AnsiParser.parseEscape "[127u"
    |> Expect.equal "Backspace" (Some (KeyPressed(Key.Backspace, Modifiers.None)))

  testCase "CSI 9 u → Tab Press" <| fun () ->
    AnsiParser.parseEscape "[9u"
    |> Expect.equal "Tab" (Some (KeyPressed(Key.Tab, Modifiers.None)))

  // ── Modified keys ────────────────────────────────────────────────────────────

  testCase "CSI 97;2u → Shift+a" <| fun () ->
    AnsiParser.parseEscape "[97;2u"
    |> Expect.equal "Shift+a" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Shift)))

  testCase "CSI 97;5u → Ctrl+a" <| fun () ->
    AnsiParser.parseEscape "[97;5u"
    |> Expect.equal "Ctrl+a" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Ctrl)))

  testCase "CSI 97;3u → Alt+a" <| fun () ->
    AnsiParser.parseEscape "[97;3u"
    |> Expect.equal "Alt+a" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.Alt)))

  testCase "CSI 13;2u → Shift+Enter (AI chat send key)" <| fun () ->
    AnsiParser.parseEscape "[13;2u"
    |> Expect.equal "Shift+Enter" (Some (KeyPressed(Key.Enter, Modifiers.Shift)))

  testCase "CSI 32;5u → Ctrl+Space" <| fun () ->
    AnsiParser.parseEscape "[32;5u"
    |> Expect.equal "Ctrl+Space" (Some (KeyPressed(Key.Char (System.Text.Rune ' '), Modifiers.Ctrl)))

  testCase "CSI 13;6u → Shift+Ctrl+Enter" <| fun () ->
    AnsiParser.parseEscape "[13;6u"
    |> Expect.equal "Shift+Ctrl+Enter" (Some (KeyPressed(Key.Enter, Modifiers.Shift ||| Modifiers.Ctrl)))

  // ── Event types (press / repeat / release) ───────────────────────────────────

  testCase "CSI 97;1:1u → 'a' press event (explicit event-type=1)" <| fun () ->
    AnsiParser.parseEscape "[97;1:1u"
    |> Expect.equal "press" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None)))

  testCase "CSI 97;1:2u → 'a' repeat event dispatches as KeyPressed" <| fun () ->
    AnsiParser.parseEscape "[97;1:2u"
    |> Expect.equal "repeat dispatches as KeyPressed" (Some (KeyPressed(Key.Char (System.Text.Rune 'a'), Modifiers.None)))

  testCase "CSI 97;1:3u → 'a' release event returns None (not dispatched)" <| fun () ->
    AnsiParser.parseEscape "[97;1:3u"
    |> Expect.isNone "release events are not dispatched via KeyPressed"

  testCase "CSI 57443;5u → Ctrl+F6 via Kitty numpad encoding" <| fun () ->
    // 57443 = 0xE063 = Kitty's F6 codepoint in extended range
    // Just testing that extended codepoints parse without error (we can't map all)
    let result = AnsiParser.parseEscape "[57443;5u"
    result |> Expect.isSome "extended codepoint parses without error"

  // ── Regression: existing sequences still work ────────────────────────────────

  testCase "legacy [A (Up arrow) still parses after Kitty support added" <| fun () ->
    AnsiParser.parseEscape "[A"
    |> Expect.equal "Up still works" (Some (KeyPressed(Key.Up, Modifiers.None)))

  testCase "legacy Shift+Up [1;2A still parses after Kitty support added" <| fun () ->
    AnsiParser.parseEscape "[1;2A"
    |> Expect.equal "Shift+Up still works" (Some (KeyPressed(Key.Up, Modifiers.Shift)))

]

let subPrefixTests = testList "Sprint 33: Sub.prefix" [

  testCase "Sub.prefix renames TimerSub id" <| fun () ->
    let sub = TimerSub("refresh", System.TimeSpan.FromSeconds 1.0, fun () -> ())
    match Sub.prefix "mycomp" sub with
    | TimerSub(id, _, _) -> id |> Expect.equal "prefixed" "mycomp/refresh"
    | _ -> failwith "expected TimerSub"

  testCase "Sub.prefix renames CustomSub id" <| fun () ->
    let sub = CustomSub("worker", fun _ _ -> async { () })
    match Sub.prefix "mycomp" sub with
    | CustomSub(id, _) -> id |> Expect.equal "prefixed" "mycomp/worker"
    | _ -> failwith "expected CustomSub"

  testCase "Sub.prefix on KeySub is identity (no id to prefix)" <| fun () ->
    let sub = KeySub(fun _ -> None)
    match Sub.prefix "ns" sub with
    | KeySub _ -> ()
    | _ -> failwith "expected KeySub unchanged"

  testCase "Sub.prefix prevents id collision between two components" <| fun () ->
    let sub1 = TimerSub("refresh", System.TimeSpan.FromSeconds 1.0, fun () -> ())
    let sub2 = TimerSub("refresh", System.TimeSpan.FromSeconds 2.0, fun () -> ())
    let prefixed1 = Sub.prefix "comp1" sub1
    let prefixed2 = Sub.prefix "comp2" sub2
    match prefixed1, prefixed2 with
    | TimerSub(id1, _, _), TimerSub(id2, _, _) ->
      id1 |> Expect.equal "comp1 id" "comp1/refresh"
      id2 |> Expect.equal "comp2 id" "comp2/refresh"
      id1 |> Expect.notEqual "ids differ" id2
    | _ -> failwith "expected TimerSubs"

  testCase "Sub.prefix on PasteSub is identity" <| fun () ->
    let sub = PasteSub(fun _ -> None)
    match Sub.prefix "ns" sub with
    | PasteSub _ -> ()
    | _ -> failwith "expected PasteSub unchanged"

  testCase "Sub.prefixAll prefixes all ID-bearing subs in a list" <| fun () ->
    let subs = [
      TimerSub("refresh", System.TimeSpan.FromSeconds 1.0, fun () -> ())
      CustomSub("worker", fun _ _ -> async { () })
      KeySub(fun _ -> None)
    ]
    let prefixed = Sub.prefixAll "sidebar" subs
    match prefixed with
    | [ TimerSub(id1, _, _); CustomSub(id2, _); KeySub _ ] ->
      id1 |> Expect.equal "timer prefixed" "sidebar/refresh"
      id2 |> Expect.equal "custom prefixed" "sidebar/worker"
    | _ -> failwith "expected 3 subs with correct types"

  testCase "Sub.prefixAll on empty list returns empty list" <| fun () ->
    Sub.prefixAll "ns" []
    |> Expect.isEmpty "empty in, empty out"

]

[<Tests>]
let inputTests =
  testList "Phase 11: Input / AnsiParser" [
    parseSgrMouseTests
    isCompleteEscSeqTests
    parseEscapeTests
    parseEscapeModifierTests
    mouseDispatchTests
    sprintThirtyTests
    dragSubTests
    bracketedPasteTests
    kittyKeyTests
    subPrefixTests
  ]
