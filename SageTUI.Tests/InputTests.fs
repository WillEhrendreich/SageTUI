module SageTUI.InputTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ─────────────────────────────────────────────────────────────────────────────
// AnsiParser.parseSgrMouse
// ─────────────────────────────────────────────────────────────────────────────

let parseSgrMouseTests = testList "AnsiParser.parseSgrMouse" [

  testCase "left button press" <| fun () ->
    AnsiParser.parseSgrMouse "<0;10;5M"
    |> Expect.equal "left press" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "left button release" <| fun () ->
    AnsiParser.parseSgrMouse "<0;10;5m"
    |> Expect.equal "left release" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "middle button" <| fun () ->
    AnsiParser.parseSgrMouse "<1;10;5M"
    |> Expect.equal "middle" (Some { Button = MiddleButton; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "right button" <| fun () ->
    AnsiParser.parseSgrMouse "<2;10;5M"
    |> Expect.equal "right" (Some { Button = RightButton; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "scroll up (btn=64)" <| fun () ->
    AnsiParser.parseSgrMouse "<64;10;5M"
    |> Expect.equal "scroll up" (Some { Button = ScrollUp; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "scroll down (btn=65)" <| fun () ->
    AnsiParser.parseSgrMouse "<65;10;5M"
    |> Expect.equal "scroll down" (Some { Button = ScrollDown; X = 9; Y = 4; Modifiers = Modifiers.None })

  testCase "shift modifier (btn=4)" <| fun () ->
    AnsiParser.parseSgrMouse "<4;10;5M"
    |> Expect.equal "shift+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Shift })

  testCase "alt modifier (btn=8)" <| fun () ->
    AnsiParser.parseSgrMouse "<8;10;5M"
    |> Expect.equal "alt+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Alt })

  testCase "ctrl modifier (btn=16)" <| fun () ->
    AnsiParser.parseSgrMouse "<16;10;5M"
    |> Expect.equal "ctrl+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Ctrl })

  testCase "shift+ctrl (btn=20)" <| fun () ->
    AnsiParser.parseSgrMouse "<20;10;5M"
    |> Expect.equal "shift+ctrl+left" (Some { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.Shift ||| Modifiers.Ctrl })

  testCase "minimum position (1,1) → (0,0)" <| fun () ->
    AnsiParser.parseSgrMouse "<0;1;1M"
    |> Expect.equal "min pos" (Some { Button = LeftButton; X = 0; Y = 0; Modifiers = Modifiers.None })

  testCase "large coordinates" <| fun () ->
    AnsiParser.parseSgrMouse "<0;220;50M"
    |> Expect.equal "large coords" (Some { Button = LeftButton; X = 219; Y = 49; Modifiers = Modifiers.None })

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

  testProperty "scroll wheel: btn&64 selects scroll, btn&1 picks up/down" <| fun (isDown: bool) (x: PositiveInt) (y: PositiveInt) ->
    let btn = if isDown then 65 else 64
    let x1 = (x.Get % 200) + 1
    let y1 = (y.Get % 50) + 1
    let expected = if isDown then ScrollDown else ScrollUp
    AnsiParser.parseSgrMouse (sprintf "<%d;%d;%dM" btn x1 y1)
    |> Option.map (fun e -> e.Button)
    |> Expect.equal "scroll direction" (Some expected)
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
// AnsiParser.parseEscape
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

  testCase "SGR mouse press in CSI body" <| fun () ->
    AnsiParser.parseEscape "[<0;10;5M"
    |> Expect.equal "mouse press" (Some (MouseInput { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None }))

  testCase "SGR mouse release in CSI body" <| fun () ->
    AnsiParser.parseEscape "[<0;10;5m"
    |> Expect.equal "mouse release" (Some (MouseInput { Button = LeftButton; X = 9; Y = 4; Modifiers = Modifiers.None }))

  testCase "SGR scroll up via parseEscape" <| fun () ->
    AnsiParser.parseEscape "[<64;1;1M"
    |> Expect.equal "scroll up" (Some (MouseInput { Button = ScrollUp; X = 0; Y = 0; Modifiers = Modifiers.None }))

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
    let mouseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None }
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
    let mouseEvent = { Button = LeftButton; X = 5; Y = 3; Modifiers = Modifiers.None }
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
    let events = [ KeyPressed(Key.Char 'a', Modifiers.None); KeyPressed(Key.Escape, Modifiers.None) ]
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


[<Tests>]
let inputTests =
  testList "Phase 11: Input / AnsiParser" [
    parseSgrMouseTests
    isCompleteEscSeqTests
    parseEscapeTests
    mouseDispatchTests
  ]
