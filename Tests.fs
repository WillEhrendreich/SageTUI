module SageTUI.Tests

open Expecto
open Expecto.Flip
open SageTUI

let mkS fg bg attrs : Style =
  { Fg = fg; Bg = bg; Attrs = attrs }

let colorTests = testList "Color" [
  testCase "Default" <| fun () ->
    Default |> Expect.equal "d" Default
  testCase "Named" <| fun () ->
    Named(Red, Bright) |> Expect.equal "r" (Named(Red, Bright))
  testCase "Ansi256" <| fun () ->
    Ansi256 42uy |> Expect.equal "a" (Ansi256 42uy)
  testCase "Rgb" <| fun () ->
    Rgb(1uy,2uy,3uy) |> Expect.equal "rgb" (Rgb(1uy,2uy,3uy))
  testCase "8 base colors" <| fun () ->
    [Black;Red;Green;Yellow;Blue;Magenta;Cyan;White]
    |> List.length |> Expect.equal "8" 8
  testCase "2 intensities" <| fun () ->
    [Normal;Bright] |> List.length |> Expect.equal "2" 2
]

let textAttrsTests = testList "TextAttrs" [
  testCase "none is zero" <| fun () ->
    TextAttrs.none.Value |> Expect.equal "" 0us
  testCase "bold bit" <| fun () ->
    TextAttrs.bold.Value |> Expect.equal "" 1us
  testCase "combine bold+italic" <| fun () ->
    let c = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    c.Value |> Expect.equal "" 5us
  testCase "has bold" <| fun () ->
    let c = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    c |> TextAttrs.has TextAttrs.bold |> Expect.isTrue ""
  testCase "not has dim" <| fun () ->
    TextAttrs.bold
    |> TextAttrs.has TextAttrs.dim
    |> Expect.isFalse ""
  testCase "remove" <| fun () ->
    let both = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    TextAttrs.remove TextAttrs.bold both
    |> TextAttrs.has TextAttrs.bold
    |> Expect.isFalse ""
  testCase "remove keeps other" <| fun () ->
    let both = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    TextAttrs.remove TextAttrs.bold both
    |> TextAttrs.has TextAttrs.italic
    |> Expect.isTrue ""
  testCase "combine commutative" <| fun () ->
    let ab = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    let ba = TextAttrs.combine TextAttrs.italic TextAttrs.bold
    ab.Value |> Expect.equal "" ba.Value
  testCase "combine associative" <| fun () ->
    let a = TextAttrs.bold
    let b = TextAttrs.italic
    let c = TextAttrs.underline
    let lhs = TextAttrs.combine (TextAttrs.combine a b) c
    let rhs = TextAttrs.combine a (TextAttrs.combine b c)
    lhs.Value |> Expect.equal "" rhs.Value
  testCase "none is identity" <| fun () ->
    let r = TextAttrs.combine TextAttrs.bold TextAttrs.none
    r.Value |> Expect.equal "" TextAttrs.bold.Value
  testCase "8 distinct attrs" <| fun () ->
    [ TextAttrs.bold; TextAttrs.dim; TextAttrs.italic
      TextAttrs.underline; TextAttrs.blink; TextAttrs.reverse
      TextAttrs.hidden; TextAttrs.strikethrough ]
    |> List.map (fun a -> a.Value)
    |> List.distinct
    |> List.length
    |> Expect.equal "" 8
  testCase "all bits powers of 2" <| fun () ->
    [ TextAttrs.bold; TextAttrs.dim; TextAttrs.italic
      TextAttrs.underline; TextAttrs.blink; TextAttrs.reverse
      TextAttrs.hidden; TextAttrs.strikethrough ]
    |> List.forall (fun a ->
      a.Value > 0us && (a.Value &&& (a.Value - 1us)) = 0us)
    |> Expect.isTrue ""
  testProperty "combine commutative prop" <|
    fun (a: byte) (b: byte) ->
      let x = { Value = uint16 a }
      let y = { Value = uint16 b }
      (TextAttrs.combine x y).Value = (TextAttrs.combine y x).Value
  testProperty "combine associative prop" <|
    fun (a: byte) (b: byte) (c: byte) ->
      let x = { Value = uint16 a }
      let y = { Value = uint16 b }
      let z = { Value = uint16 c }
      let lhs = TextAttrs.combine (TextAttrs.combine x y) z
      let rhs = TextAttrs.combine x (TextAttrs.combine y z)
      lhs.Value = rhs.Value
  testProperty "has/remove roundtrip" <|
    fun (a: byte) (b: byte) ->
      let flag = { Value = uint16 (b ||| 1uy) }
      let combined = TextAttrs.combine { Value = uint16 a } flag
      TextAttrs.has flag combined
  testProperty "remove then not has" <|
    fun (a: byte) (b: byte) ->
      let flag = { Value = uint16 (b ||| 1uy) }
      let combined = TextAttrs.combine { Value = uint16 a } flag
      let removed = TextAttrs.remove flag combined
      TextAttrs.has flag removed |> not
  testProperty "none is identity prop" <|
    fun (a: byte) ->
      let x = { Value = uint16 a }
      let lhs = (TextAttrs.combine x TextAttrs.none).Value = x.Value
      let rhs = (TextAttrs.combine TextAttrs.none x).Value = x.Value
      lhs && rhs
]

let styleTests = testList "Style" [
  testCase "empty" <| fun () ->
    Style.empty
    |> Expect.equal "" { Fg = None; Bg = None; Attrs = TextAttrs.none }
  testCase "merge right identity" <| fun () ->
    let s = mkS (Some Default) None TextAttrs.bold
    Style.merge s Style.empty |> Expect.equal "" s
  testCase "merge left identity" <| fun () ->
    let s = mkS (Some Default) None TextAttrs.bold
    Style.merge Style.empty s |> Expect.equal "" s
  testCase "merge fg overlay wins" <| fun () ->
    let a = mkS (Some (Named(Red,Normal))) None TextAttrs.none
    let b = mkS (Some (Named(Blue,Bright))) None TextAttrs.none
    (Style.merge a b).Fg
    |> Expect.equal "" (Some (Named(Blue,Bright)))
  testCase "merge fg inherits" <| fun () ->
    let a = mkS (Some (Named(Red,Normal))) None TextAttrs.none
    let b = mkS None None TextAttrs.none
    (Style.merge a b).Fg
    |> Expect.equal "" (Some (Named(Red,Normal)))
  testCase "merge attrs combine" <| fun () ->
    let a = mkS None None TextAttrs.bold
    let b = mkS None None TextAttrs.italic
    let merged = Style.merge a b
    merged.Attrs |> TextAttrs.has TextAttrs.bold |> Expect.isTrue ""
    merged.Attrs |> TextAttrs.has TextAttrs.italic |> Expect.isTrue ""
  testCase "withFg" <| fun () ->
    (Style.withFg Default Style.empty).Fg
    |> Expect.equal "" (Some Default)
  testCase "withBg" <| fun () ->
    (Style.withBg Default Style.empty).Bg
    |> Expect.equal "" (Some Default)
  testCase "withBold" <| fun () ->
    (Style.withBold Style.empty).Attrs
    |> TextAttrs.has TextAttrs.bold
    |> Expect.isTrue ""
  testCase "withItalic" <| fun () ->
    (Style.withItalic Style.empty).Attrs
    |> TextAttrs.has TextAttrs.italic
    |> Expect.isTrue ""
  testProperty "left identity" <|
    fun (f: byte option) (b: byte option) (a: byte) ->
      let fg = f |> Option.map Ansi256
      let bg = b |> Option.map Ansi256
      let s = mkS fg bg { Value = uint16 a }
      Style.merge Style.empty s = s
  testProperty "right identity" <|
    fun (f: byte option) (b: byte option) (a: byte) ->
      let fg = f |> Option.map Ansi256
      let bg = b |> Option.map Ansi256
      let s = mkS fg bg { Value = uint16 a }
      Style.merge s Style.empty = s
  testProperty "associative" <|
    fun (x: byte option * byte option * byte)
        (y: byte option * byte option * byte)
        (z: byte option * byte option * byte) ->
      let toS (f, b, a) =
        mkS (f |> Option.map Ansi256) (b |> Option.map Ansi256) { Value = uint16 a }
      let sx = toS x
      let sy = toS y
      let sz = toS z
      Style.merge (Style.merge sx sy) sz = Style.merge sx (Style.merge sy sz)
]

let packedColorTests = testList "PackedColor" [
  testCase "Default roundtrip" <| fun () ->
    PackedColor.pack Default
    |> PackedColor.unpack
    |> Expect.equal "" Default
  testCase "Named roundtrip" <| fun () ->
    let c = Named(Blue, Bright)
    PackedColor.pack c |> PackedColor.unpack |> Expect.equal "" c
  testCase "Ansi256 roundtrip" <| fun () ->
    let c = Ansi256 200uy
    PackedColor.pack c |> PackedColor.unpack |> Expect.equal "" c
  testCase "Rgb roundtrip" <| fun () ->
    let c = Rgb(10uy,20uy,30uy)
    PackedColor.pack c |> PackedColor.unpack |> Expect.equal "" c
  testCase "Default packs to 0" <| fun () ->
    PackedColor.pack Default |> Expect.equal "" 0
  testCase "distinct tags" <| fun () ->
    let tags =
      [ Default; Named(Red,Normal); Ansi256 0uy; Rgb(0uy,0uy,0uy) ]
      |> List.map (fun c -> PackedColor.pack c &&& 0x3)
    tags |> List.distinct |> List.length |> Expect.equal "" 4
  testProperty "all Named roundtrip" <| fun () ->
    let bcs = [Black;Red;Green;Yellow;Blue;Magenta;Cyan;White]
    let ins = [Normal;Bright]
    bcs |> List.forall (fun bc ->
      ins |> List.forall (fun i ->
        let c = Named(bc,i)
        PackedColor.pack c |> PackedColor.unpack = c))
  testProperty "Ansi256 roundtrip prop" <| fun (b: byte) ->
    let c = Ansi256 b
    PackedColor.pack c |> PackedColor.unpack = c
  testProperty "Rgb roundtrip prop" <| fun (r: byte) (g: byte) (b: byte) ->
    let c = Rgb(r,g,b)
    PackedColor.pack c |> PackedColor.unpack = c
]

let areaTests = testList "Area" [
  testCase "fields" <| fun () ->
    let a : Area = { X = 5; Y = 10; Width = 80; Height = 24 }
    a.X |> Expect.equal "" 5
    a.Width |> Expect.equal "" 80
]

let paddingTests = testList "Padding" [
  testCase "zero" <| fun () ->
    Padding.zero
    |> Expect.equal "" { Top = 0; Right = 0; Bottom = 0; Left = 0 }
  testCase "all" <| fun () ->
    let p = Padding.all 3
    p.Top |> Expect.equal "" 3
    p.Left |> Expect.equal "" 3
  testCase "hv" <| fun () ->
    let p = Padding.hv 2 1
    p.Left |> Expect.equal "" 2
    p.Top |> Expect.equal "" 1
  testCase "horiz" <| fun () ->
    Padding.horizontal (Padding.hv 5 3) |> Expect.equal "" 10
  testCase "vert" <| fun () ->
    Padding.vertical (Padding.hv 5 3) |> Expect.equal "" 6
  testProperty "all symmetric" <| fun (n: byte) ->
    let p = Padding.all (int n)
    p.Top = p.Right && p.Right = p.Bottom && p.Bottom = p.Left
]

let constraintTests = testList "Constraint" [
  testCase "6 variants" <| fun () ->
    [ Fixed 10; Min 5; Max 20; Percentage 50; Fill; Ratio(1,3) ]
    |> List.length
    |> Expect.equal "" 6
]

let elementTests = testList "Element" [
  testCase "text" <| fun () ->
    match El.text "hi" with
    | Text(s, st) ->
      s |> Expect.equal "" "hi"
      st |> Expect.equal "" Style.empty
    | _ -> failwith "expected Text"
  testCase "bold" <| fun () ->
    match El.text "x" |> El.bold with
    | Styled(s, Text("x", _)) ->
      TextAttrs.has TextAttrs.bold s.Attrs |> Expect.isTrue ""
    | _ -> failwith "expected Styled"
  testCase "fg" <| fun () ->
    match El.text "x" |> El.fg (Named(Red,Normal)) with
    | Styled(s, Text("x", _)) ->
      s.Fg |> Expect.equal "" (Some(Named(Red,Normal)))
    | _ -> failwith "expected Styled"
  testCase "border" <| fun () ->
    match El.text "x" |> El.border with
    | Bordered(Light, Text("x",_)) -> ()
    | _ -> failwith "expected Bordered"
  testCase "pad" <| fun () ->
    match El.text "x" |> El.padAll 2 with
    | Padded(p, Text("x",_)) ->
      p |> Expect.equal "" (Padding.all 2)
    | _ -> failwith "expected Padded"
  testCase "width" <| fun () ->
    match El.text "x" |> El.width 10 with
    | Constrained(Fixed 10, Text("x",_)) -> ()
    | _ -> failwith "expected Constrained"
  testCase "keyed" <| fun () ->
    match El.text "x" |> El.viewTransition "p" with
    | Keyed("p", ColorMorph d, Fade f, Text("x",_)) ->
      d |> Expect.equal "" 200<ms>
      f |> Expect.equal "" 0<ms>
    | _ -> failwith "expected Keyed"
  testCase "chain" <| fun () ->
    let elem =
      El.text "x"
      |> El.fg (Named(Red,Normal))
      |> El.bold
      |> El.border
    match elem with
    | Bordered(Light, Styled(s2, Styled(s1, Text("x",_)))) ->
      TextAttrs.has TextAttrs.bold s2.Attrs |> Expect.isTrue ""
      s1.Fg |> Expect.equal "" (Some(Named(Red,Normal)))
    | _ -> failwith "expected chain"
  testCase "row" <| fun () ->
    match El.row [El.text "a"; El.text "b"] with
    | Row [Text("a",_); Text("b",_)] -> ()
    | _ -> failwith "expected Row"
  testCase "column" <| fun () ->
    match El.column [El.text "a"] with
    | Column [Text("a",_)] -> ()
    | _ -> failwith "expected Column"
  testCase "overlay" <| fun () ->
    match El.overlay [El.text "a"] with
    | Overlay [Text("a",_)] -> ()
    | _ -> failwith "expected Overlay"
]

let easingTests = testList "Easing" [
  testCase "lin0" <| fun () ->
    Ease.linear 0.0 |> Expect.equal "" 0.0
  testCase "lin1" <| fun () ->
    Ease.linear 1.0 |> Expect.equal "" 1.0
  testCase "qo0" <| fun () ->
    Ease.quadOut 0.0 |> Expect.equal "" 0.0
  testCase "qo1" <| fun () ->
    Ease.quadOut 1.0 |> Expect.equal "" 1.0
  testCase "qi0" <| fun () ->
    Ease.quadIn 0.0 |> Expect.equal "" 0.0
  testCase "qi1" <| fun () ->
    Ease.quadIn 1.0 |> Expect.equal "" 1.0
  testCase "cio0" <| fun () ->
    Ease.cubicInOut 0.0 |> Expect.equal "" 0.0
  testCase "cio1" <| fun () ->
    Ease.cubicInOut 1.0 |> Expect.equal "" 1.0
  testCase "qo>lin" <| fun () ->
    (Ease.quadOut 0.5 > Ease.linear 0.5)
    |> Expect.isTrue ""
  testProperty "boundaries" <| fun () ->
    [ Ease.linear; Ease.quadOut; Ease.quadIn; Ease.cubicInOut ]
    |> List.forall (fun e ->
      abs (e 0.0) < 1e-10 && abs (e 1.0 - 1.0) < 1e-10)
  testProperty "in [0,1]" <| fun (n: byte) ->
    let t = float n / 255.0
    [ Ease.linear; Ease.quadOut; Ease.quadIn; Ease.cubicInOut ]
    |> List.forall (fun e ->
      let v = e t
      v >= -0.01 && v <= 1.01)
]

let brailleTests = testList "Braille" [
  testCase "base" <| fun () ->
    Braille.brailleBase |> Expect.equal "" 0x2800
  testCase "empty" <| fun () ->
    Braille.toChar 0 |> Expect.equal "" '\u2800'
  testCase "d00" <| fun () ->
    Braille.dotAt 0 0 |> Expect.equal "" 0x01
  testCase "d13" <| fun () ->
    Braille.dotAt 1 3 |> Expect.equal "" 0x80
  testCase "oob" <| fun () ->
    Braille.dotAt 2 0 |> Expect.equal "" 0
  testCase "all8" <| fun () ->
    let allBits =
      [0..1] |> List.collect (fun c ->
        [0..3] |> List.map (fun r -> Braille.dotAt c r))
    allBits |> List.fold (|||) 0 |> Expect.equal "" 0xFF
  testProperty "in-range non-zero" <|
    fun (c: int) (r: int) ->
      Braille.dotAt (abs c % 2) (abs r % 4) > 0
  testProperty "toChar in braille" <| fun (bits: byte) ->
    let ch = Braille.toChar (int bits &&& 0xFF)
    int ch >= 0x2800 && int ch <= 0x28FF
]

let alphaTests = testList "Alpha" [
  testCase "t0=bg" <| fun () ->
    Alpha.blendColor 0.0 (Rgb(255uy,0uy,0uy)) (Rgb(0uy,255uy,0uy))
    |> Expect.equal "" (Rgb(255uy,0uy,0uy))
  testCase "t1=fg" <| fun () ->
    Alpha.blendColor 1.0 (Rgb(255uy,0uy,0uy)) (Rgb(0uy,255uy,0uy))
    |> Expect.equal "" (Rgb(0uy,255uy,0uy))
  testCase "midpt" <| fun () ->
    match Alpha.blendColor 0.5 (Rgb(0uy,0uy,0uy)) (Rgb(254uy,254uy,254uy)) with
    | Rgb(r,_,_) ->
      r |> Expect.equal "" 127uy
    | _ -> failwith "expected rgb"
  testCase "named<0.5" <| fun () ->
    Alpha.blendColor 0.3 (Named(Red,Normal)) (Named(Blue,Bright))
    |> Expect.equal "" (Named(Red,Normal))
  testCase "named>=0.5" <| fun () ->
    Alpha.blendColor 0.5 (Named(Red,Normal)) (Named(Blue,Bright))
    |> Expect.equal "" (Named(Blue,Bright))
  testProperty "lerp t=0" <| fun (a: byte) (b: byte) ->
    Alpha.lerpByte 0.0 a b = a
  testProperty "lerp t=1" <| fun (a: byte) (b: byte) ->
    Alpha.lerpByte 1.0 a b = b
  testProperty "lerp monotone" <| fun (a: byte) (b: byte) ->
    let lo = Alpha.lerpByte 0.25 a b
    let hi = Alpha.lerpByte 0.75 a b
    match a <= b with
    | true -> lo <= hi
    | false -> lo >= hi
]

let terminalCapabilityTests = testList "TerminalCapabilities" [
  testCase "color ordering" <| fun () ->
    (ColorCapability.TrueColor >= ColorCapability.NoColor)
    |> Expect.isTrue ""
  testCase "unicode ordering" <| fun () ->
    (UnicodeCapability.FullUnicode >= UnicodeCapability.AsciiOnly)
    |> Expect.isTrue ""
  testCase "graphics ordering" <| fun () ->
    (GraphicsCapability.KittyGraphics >= GraphicsCapability.TextOnly)
    |> Expect.isTrue ""
  testCase "input ordering" <| fun () ->
    (InputCapability.KittyKeyboard >= InputCapability.BasicKeys)
    |> Expect.isTrue ""
  testCase "output ordering" <| fun () ->
    (OutputCapability.SynchronizedOutput >= OutputCapability.LineBuffered)
    |> Expect.isTrue ""
  testCase "profile" <| fun () ->
    let p : TerminalProfile = {
      Color = ColorCapability.TrueColor
      Unicode = UnicodeCapability.FullUnicode
      Graphics = GraphicsCapability.Braille
      Input = InputCapability.MouseSgr
      Output = OutputCapability.AltScreen
      Size = (80, 24)
      TermName = "xterm-256color"
      Platform = Linux
    }
    p.Color |> Expect.equal "" ColorCapability.TrueColor
    p.Platform |> Expect.equal "" Linux
  testCase "3 platforms" <| fun () ->
    [Windows; MacOS; Linux]
    |> List.length
    |> Expect.equal "" 3
  testCase "color enum values" <| fun () ->
    int ColorCapability.NoColor |> Expect.equal "" 0
    int ColorCapability.TrueColor |> Expect.equal "" 3
  testCase "graceful downgrade" <| fun () ->
    let hasTrueColor cap = cap >= ColorCapability.TrueColor
    hasTrueColor ColorCapability.TrueColor |> Expect.isTrue ""
    hasTrueColor ColorCapability.Basic16 |> Expect.isFalse ""
]

let blurTests = testList "Blur" [
  testCase "desaturate 0 is identity for Rgb" <| fun () ->
    let c = Rgb(100uy, 150uy, 200uy)
    Blur.desaturate 0.0 c |> Expect.equal "" c
  testCase "desaturate 1 is gray" <| fun () ->
    match Blur.desaturate 1.0 (Rgb(100uy, 150uy, 200uy)) with
    | Rgb(r, g, b) ->
      r |> Expect.equal "" g
      g |> Expect.equal "" b
    | _ -> failwith "expected Rgb"
  testCase "desaturate non-Rgb passthrough" <| fun () ->
    Blur.desaturate 0.5 (Named(Red, Normal))
    |> Expect.equal "" (Named(Red, Normal))
  testCase "degradeChar >0.8 is space" <| fun () ->
    Blur.degradeChar 0.9 (int 'X') |> Expect.equal "" (int ' ')
  testCase "degradeChar <0.4 is identity" <| fun () ->
    Blur.degradeChar 0.1 (int 'X') |> Expect.equal "" (int 'X')
  testCase "degradeChar 0.5 is medium shade" <| fun () ->
    Blur.degradeChar 0.5 (int 'X') |> Expect.equal "" (int '▒')
  testCase "degradeChar 0.7 is light shade" <| fun () ->
    Blur.degradeChar 0.7 (int 'X') |> Expect.equal "" (int '░')
  testProperty "desaturate clamped" <| fun (n: byte) ->
    let c = Rgb(n, n, n)
    match Blur.desaturate 0.5 c with
    | Rgb(r, _, _) -> r >= 0uy && r <= 255uy
    | _ -> false
]

[<Tests>]
let allTests = testList "Phase 0" [
  colorTests
  textAttrsTests
  styleTests
  packedColorTests
  areaTests
  paddingTests
  constraintTests
  elementTests
  easingTests
  brailleTests
  alphaTests
  terminalCapabilityTests
  blurTests
]
