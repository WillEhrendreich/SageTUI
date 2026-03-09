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
              match k with Key.Escape -> Some None | _ -> None) ] }
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
              match k with Key.Escape -> Some None | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ] }
    App.runWithBackend backend program
    mouseCount |> Expect.equal "no mouse events" 0
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some false | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ] }
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
            KeySub (fun (k, _) -> match k with Key.Escape -> Some None | _ -> None) ] }
    App.runWithBackend backend program
    focusStates |> List.rev |> Expect.equal "gained then lost" [ true; false ]
]


[<Tests>]
let inputTests =
  testList "Phase 11: Input / AnsiParser" [
    parseSgrMouseTests
    isCompleteEscSeqTests
    parseEscapeTests
    mouseDispatchTests
    sprintThirtyTests
  ]
