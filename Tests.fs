module SageTUI.Tests

open System.Text
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

// ============================================================
// Phase 1 Tests: PackedCell + Buffer + SIMD Diff
// ============================================================

let packedCellTests = testList "PackedCell" [
  testCase "size is 16 bytes" <| fun () ->
    sizeof<PackedCell>
    |> Expect.equal "sizeof<PackedCell> should be 16" 16

  testCase "empty has space rune" <| fun () ->
    PackedCell.empty.Rune
    |> Expect.equal "rune" (int (Rune ' ').Value)

  testCase "empty has zero fg/bg" <| fun () ->
    PackedCell.empty.Fg |> Expect.equal "fg" 0
    PackedCell.empty.Bg |> Expect.equal "bg" 0

  testCase "empty has zero attrs" <| fun () ->
    PackedCell.empty.Attrs |> Expect.equal "attrs" 0us

  testCase "create preserves all fields" <| fun () ->
    let c = PackedCell.create 65 100 200 7us
    c.Rune |> Expect.equal "rune" 65
    c.Fg |> Expect.equal "fg" 100
    c.Bg |> Expect.equal "bg" 200
    c.Attrs |> Expect.equal "attrs" 7us
    c._pad |> Expect.equal "pad" 0us

  testCase "PackedColor roundtrip in cell" <| fun () ->
    let fg = PackedColor.pack (Named(Red, Bright))
    let bg = PackedColor.pack (Rgb(10uy, 20uy, 30uy))
    let c = PackedCell.create (int (Rune 'X').Value) fg bg 0us
    PackedColor.unpack c.Fg
    |> Expect.equal "fg color" (Named(Red, Bright))
    PackedColor.unpack c.Bg
    |> Expect.equal "bg color" (Rgb(10uy, 20uy, 30uy))

  testCase "TextAttrs roundtrip in cell" <| fun () ->
    let attrs = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    let c = PackedCell.create 0 0 0 attrs.Value
    TextAttrs.has TextAttrs.bold { Value = c.Attrs }
    |> Expect.isTrue "bold"
    TextAttrs.has TextAttrs.italic { Value = c.Attrs }
    |> Expect.isTrue "italic"
    TextAttrs.has TextAttrs.underline { Value = c.Attrs }
    |> Expect.isFalse "not underline"
]

let runeWidthTests = testList "RuneWidth" [
  testCase "ASCII letter is 1" <| fun () ->
    RuneWidth.getColumnWidth (Rune 'A') |> Expect.equal "" 1

  testCase "space is 1" <| fun () ->
    RuneWidth.getColumnWidth (Rune ' ') |> Expect.equal "" 1

  testCase "digit is 1" <| fun () ->
    RuneWidth.getColumnWidth (Rune '0') |> Expect.equal "" 1

  testCase "CJK ideograph is 2" <| fun () ->
    RuneWidth.getColumnWidth (Rune 0x4E2D) |> Expect.equal "U+4E2D" 2

  testCase "Hangul syllable is 2" <| fun () ->
    RuneWidth.getColumnWidth (Rune 0xAC00) |> Expect.equal "U+AC00" 2

  testCase "null is 0" <| fun () ->
    RuneWidth.getColumnWidth (Rune 0x00) |> Expect.equal "NUL" 0

  testCase "tab is 0" <| fun () ->
    RuneWidth.getColumnWidth (Rune 0x09) |> Expect.equal "TAB" 0
]

let bufferBasicTests = testList "Buffer basics" [
  testCase "create dimensions" <| fun () ->
    let buf = Buffer.create 80 24
    buf.Width |> Expect.equal "width" 80
    buf.Height |> Expect.equal "height" 24
    buf.Cells.Length |> Expect.equal "cells" (80 * 24)

  testCase "create all empty" <| fun () ->
    let buf = Buffer.create 10 5
    buf.Cells |> Array.forall ((=) Buffer.emptyCell)
    |> Expect.isTrue "all empty"

  testCase "get valid" <| fun () ->
    let buf = Buffer.create 5 5
    let cell = PackedCell.create 65 1 2 3us
    Buffer.set 2 3 cell buf
    Buffer.get 2 3 buf |> Expect.equal "roundtrip" cell

  testCase "get out-of-bounds returns empty" <| fun () ->
    let buf = Buffer.create 5 5
    Buffer.get -1 0 buf |> Expect.equal "neg x" Buffer.emptyCell
    Buffer.get 0 -1 buf |> Expect.equal "neg y" Buffer.emptyCell
    Buffer.get 5 0 buf |> Expect.equal "over x" Buffer.emptyCell
    Buffer.get 0 5 buf |> Expect.equal "over y" Buffer.emptyCell

  testCase "set out-of-bounds is no-op" <| fun () ->
    let buf = Buffer.create 5 5
    let before = buf.Cells |> Array.copy
    Buffer.set -1 0 (PackedCell.create 1 2 3 4us) buf
    Buffer.set 5 0 (PackedCell.create 1 2 3 4us) buf
    Buffer.set 0 -1 (PackedCell.create 1 2 3 4us) buf
    Buffer.set 0 5 (PackedCell.create 1 2 3 4us) buf
    (buf.Cells = before) |> Expect.isTrue "unchanged"

  testCase "clear resets all cells" <| fun () ->
    let buf = Buffer.create 5 5
    Buffer.set 2 2 (PackedCell.create 65 1 2 3us) buf
    Buffer.clear buf
    buf.Cells |> Array.forall ((=) Buffer.emptyCell)
    |> Expect.isTrue "all empty after clear"

  testProperty "set-get roundtrip" <| fun (xb: byte) (yb: byte) (r: int32) (fg: int32) (bg: int32) (a: uint16) ->
    let w, h = 20, 20
    let x = int xb % w
    let y = int yb % h
    let buf = Buffer.create w h
    let cell = PackedCell.create r fg bg a
    Buffer.set x y cell buf
    Buffer.get x y buf = cell

  testProperty "out-of-bounds get always returns emptyCell" <| fun (xb: int) (yb: int) ->
    let buf = Buffer.create 10 10
    let x = if xb >= 0 then xb + 10 else xb
    let y = if yb >= 0 then yb + 10 else yb
    Buffer.get x y buf = Buffer.emptyCell
]

let bufferWriteTests = testList "Buffer writeString" [
  testCase "write Hello" <| fun () ->
    let buf = Buffer.create 10 1
    Buffer.writeString 0 0 0 0 0us "Hello" buf
    Buffer.toString buf |> Expect.stringStarts "starts with Hello" "Hello"

  testCase "write preserves style" <| fun () ->
    let fg = PackedColor.pack (Named(Red, Normal))
    let bg = PackedColor.pack (Rgb(1uy, 2uy, 3uy))
    let attrs = TextAttrs.bold.Value
    let buf = Buffer.create 10 1
    Buffer.writeString 0 0 fg bg attrs "AB" buf
    let c = Buffer.get 0 0 buf
    c.Fg |> Expect.equal "fg" fg
    c.Bg |> Expect.equal "bg" bg
    c.Attrs |> Expect.equal "attrs" attrs

  testCase "write clips at right edge" <| fun () ->
    let buf = Buffer.create 3 1
    Buffer.writeString 0 0 0 0 0us "ABCDE" buf
    Buffer.toString buf |> Expect.equal "clipped" "ABC"

  testCase "write at offset" <| fun () ->
    let buf = Buffer.create 10 1
    Buffer.writeString 3 0 0 0 0us "Hi" buf
    Buffer.toString buf |> Expect.equal "offset" "   Hi     "

  testCase "write empty string is no-op" <| fun () ->
    let buf = Buffer.create 5 1
    Buffer.writeString 0 0 0 0 0us "" buf
    buf.Cells |> Array.forall ((=) Buffer.emptyCell)
    |> Expect.isTrue "unchanged"

  testCase "write on second row" <| fun () ->
    let buf = Buffer.create 5 3
    Buffer.writeString 0 1 0 0 0us "Row1" buf
    let c = Buffer.get 0 1 buf
    c.Rune |> Expect.equal "R" (int (Rune 'R').Value)
    Buffer.get 0 0 buf |> Expect.equal "row 0 empty" Buffer.emptyCell

  testCase "write out-of-bounds y is no-op" <| fun () ->
    let buf = Buffer.create 5 2
    let before = buf.Cells |> Array.copy
    Buffer.writeString 0 5 0 0 0us "Hello" buf
    (buf.Cells = before) |> Expect.isTrue "unchanged"
]

let bufferDiffTests = testList "Buffer diff" [
  testCase "identical buffers yield empty diff" <| fun () ->
    let a = Buffer.create 20 10
    let b = Buffer.create 20 10
    Buffer.diff a b
    |> (fun d -> d.Count) |> Expect.equal "count" 0

  testCase "single change yields 1 index" <| fun () ->
    let a = Buffer.create 20 10
    let b = Buffer.create 20 10
    Buffer.set 5 5 (PackedCell.create 65 1 0 0us) b
    let d = Buffer.diff a b
    d.Count |> Expect.equal "count" 1
    d[0] |> Expect.equal "idx" (5 * 20 + 5)

  testCase "multiple scattered changes" <| fun () ->
    let a = Buffer.create 20 10
    let b = Buffer.create 20 10
    Buffer.set 0 0 (PackedCell.create 1 0 0 0us) b
    Buffer.set 19 9 (PackedCell.create 2 0 0 0us) b
    Buffer.set 10 5 (PackedCell.create 3 0 0 0us) b
    Buffer.diff a b |> (fun d -> d.Count) |> Expect.equal "count" 3

  testCase "diff indices are sorted ascending" <| fun () ->
    let a = Buffer.create 20 10
    let b = Buffer.create 20 10
    Buffer.set 19 9 (PackedCell.create 1 0 0 0us) b
    Buffer.set 0 0 (PackedCell.create 2 0 0 0us) b
    Buffer.set 10 5 (PackedCell.create 3 0 0 0us) b
    let d = Buffer.diff a b |> Seq.toList
    d |> Expect.equal "sorted" (d |> List.sort)

  testCase "all cells changed" <| fun () ->
    let a = Buffer.create 10 10
    let b = Buffer.create 10 10
    for i in 0 .. b.Cells.Length - 1 do
      b.Cells[i] <- PackedCell.create (i + 1) 1 0 0us
    Buffer.diff a b |> (fun d -> d.Count) |> Expect.equal "all" 100

  testCase "large buffer exercises chunk-skip" <| fun () ->
    let a = Buffer.create 80 25
    let b = Buffer.create 80 25
    b.Cells[500] <- PackedCell.create 42 0 0 0us
    let d = Buffer.diff a b
    d.Count |> Expect.equal "count" 1
    d[0] |> Expect.equal "idx" 500

  testCase "change in remainder cells" <| fun () ->
    let a = Buffer.create 17 1
    let b = Buffer.create 17 1
    b.Cells[16] <- PackedCell.create 99 0 0 0us
    let d = Buffer.diff a b
    d.Count |> Expect.equal "count" 1
    d[0] |> Expect.equal "idx" 16

  testCase "change in first and last cell" <| fun () ->
    let a = Buffer.create 50 1
    let b = Buffer.create 50 1
    b.Cells[0] <- PackedCell.create 1 0 0 0us
    b.Cells[49] <- PackedCell.create 2 0 0 0us
    let d = Buffer.diff a b
    d.Count |> Expect.equal "count" 2
    d[0] |> Expect.equal "first" 0
    d[1] |> Expect.equal "last" 49

  testProperty "self-diff is always empty" <| fun (wb: byte) (hb: byte) ->
    let w = max 1 (int wb % 50)
    let h = max 1 (int hb % 50)
    let buf = Buffer.create w h
    Buffer.diff buf buf |> (fun d -> d.Count) = 0

  testProperty "set then diff detects the change" <| fun (xb: byte) (yb: byte) (rv: int32) ->
    let w, h = 20, 20
    let x = int xb % w
    let y = int yb % h
    let a = Buffer.create w h
    let b = Buffer.create w h
    let cell = PackedCell.create (abs rv ||| 1) 42 0 0us
    Buffer.set x y cell b
    let d = Buffer.diff a b
    d |> Seq.contains (y * w + x)
]

let bufferToStringTests = testList "Buffer toString" [
  testCase "empty buffer is all spaces" <| fun () ->
    let buf = Buffer.create 3 2
    Buffer.toString buf |> Expect.equal "spaces" "   \n   "

  testCase "write then toString" <| fun () ->
    let buf = Buffer.create 5 1
    Buffer.writeString 0 0 0 0 0us "Hi" buf
    Buffer.toString buf |> Expect.equal "str" "Hi   "

  testCase "multi-line content" <| fun () ->
    let buf = Buffer.create 3 2
    Buffer.writeString 0 0 0 0 0us "ABC" buf
    Buffer.writeString 0 1 0 0 0us "DEF" buf
    Buffer.toString buf |> Expect.equal "str" "ABC\nDEF"

  testCase "no trailing newline" <| fun () ->
    let buf = Buffer.create 2 3
    let s = Buffer.toString buf
    s.EndsWith('\n') |> Expect.isFalse "no trailing newline"

  testCase "1x1 buffer" <| fun () ->
    let buf = Buffer.create 1 1
    Buffer.writeString 0 0 0 0 0us "X" buf
    Buffer.toString buf |> Expect.equal "str" "X"
]

// ============================================================
// Phase 2: Frame Arena Tests
// ============================================================

let arenaInfraTests = testList "FrameArena infra" [
  testCase "create initial state" <| fun () ->
    let a = FrameArena.create 100 1000 500
    a.NodeCount |> Expect.equal "nodes" 0
    a.TextPos |> Expect.equal "text" 0
    a.LayoutPos |> Expect.equal "layout" 0
    a.Generation |> Expect.equal "gen" 0
    a.Nodes.Length |> Expect.equal "capacity" 100

  testCase "allocNode increments" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h0 = FrameArena.allocNode a
    NodeHandle.value h0 |> Expect.equal "h0" 0
    a.NodeCount |> Expect.equal "count1" 1
    let h1 = FrameArena.allocNode a
    NodeHandle.value h1 |> Expect.equal "h1" 1
    a.NodeCount |> Expect.equal "count2" 2

  testCase "allocNode overflow throws" <| fun () ->
    let a = FrameArena.create 2 100 50
    FrameArena.allocNode a |> ignore
    FrameArena.allocNode a |> ignore
    let threw =
      try FrameArena.allocNode a |> ignore; false
      with _ -> true
    threw |> Expect.isTrue "should throw on overflow"

  testCase "allocText copies text" <| fun () ->
    let a = FrameArena.create 10 100 50
    let (s, l) = FrameArena.allocText "Hello" a
    s |> Expect.equal "start" 0
    l |> Expect.equal "len" 5
    a.TextPos |> Expect.equal "pos" 5
    System.String(a.TextBuf, s, l) |> Expect.equal "text" "Hello"

  testCase "allocText sequential" <| fun () ->
    let a = FrameArena.create 10 100 50
    let (s1, _) = FrameArena.allocText "AB" a
    let (s2, l2) = FrameArena.allocText "CD" a
    s1 |> Expect.equal "s1" 0
    s2 |> Expect.equal "s2" 2
    System.String(a.TextBuf, s2, l2) |> Expect.equal "t2" "CD"

  testCase "reset clears and bumps gen" <| fun () ->
    let a = FrameArena.create 10 100 50
    FrameArena.allocNode a |> ignore
    FrameArena.allocText "Hi" a |> ignore
    FrameArena.reset a
    a.NodeCount |> Expect.equal "nodes" 0
    a.TextPos |> Expect.equal "text" 0
    a.LayoutPos |> Expect.equal "layout" 0
    a.Generation |> Expect.equal "gen" 1

  testCase "reset preserves capacity" <| fun () ->
    let a = FrameArena.create 10 100 50
    FrameArena.allocNode a |> ignore
    FrameArena.reset a
    a.Nodes.Length |> Expect.equal "cap" 10
    a.TextBuf.Length |> Expect.equal "text cap" 100

  testCase "multiple resets increment gen" <| fun () ->
    let a = FrameArena.create 10 100 50
    FrameArena.reset a
    FrameArena.reset a
    FrameArena.reset a
    a.Generation |> Expect.equal "gen" 3
]

let arenaLowerTests = testList "Arena.lower" [
  testCase "lower Empty" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a Empty
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 0uy
    n.FirstChild |> Expect.equal "child" -1
    n.NextSibling |> Expect.equal "sib" -1

  testCase "lower Text" <| fun () ->
    let a = FrameArena.create 10 100 50
    let style = { Fg = Some (Named(Red, Bright)); Bg = None; Attrs = TextAttrs.bold }
    let h = Arena.lower a (Text("hi", style))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 1uy
    n.DataLen |> Expect.equal "len" 2
    System.String(a.TextBuf, n.DataStart, n.DataLen) |> Expect.equal "text" "hi"
    n.AttrsPacked |> Expect.equal "attrs" TextAttrs.bold.Value

  testCase "lower Row links 3 children" <| fun () ->
    let a = FrameArena.create 20 200 100
    let h = Arena.lower a (Row [
      Text("A", Style.empty); Text("B", Style.empty); Text("C", Style.empty)
    ])
    let root = FrameArena.getNode h a
    root.Kind |> Expect.equal "kind" 2uy
    (root.FirstChild >= 0) |> Expect.isTrue "has child"
    let c0 = a.Nodes.[root.FirstChild]
    c0.Kind |> Expect.equal "c0" 1uy
    (c0.NextSibling >= 0) |> Expect.isTrue "c0 next"
    let c1 = a.Nodes.[c0.NextSibling]
    c1.Kind |> Expect.equal "c1" 1uy
    (c1.NextSibling >= 0) |> Expect.isTrue "c1 next"
    let c2 = a.Nodes.[c1.NextSibling]
    c2.Kind |> Expect.equal "c2" 1uy
    c2.NextSibling |> Expect.equal "c2 end" -1

  testCase "lower Column" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Column [Text("X", Style.empty)])
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 3uy
    (n.FirstChild >= 0) |> Expect.isTrue "has child"

  testCase "lower Overlay" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Overlay [Empty; Empty])
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 4uy
    (n.FirstChild >= 0) |> Expect.isTrue "has children"

  testCase "lower Styled packs color" <| fun () ->
    let a = FrameArena.create 10 100 50
    let style = Style.empty |> Style.withFg (Rgb(255uy, 0uy, 0uy))
    let h = Arena.lower a (Styled(style, Empty))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 5uy
    (n.FirstChild >= 0) |> Expect.isTrue "has child"
    let fg = Arena.unpackStyleFg n.StylePacked
    PackedColor.unpack fg |> Expect.equal "fg" (Rgb(255uy, 0uy, 0uy))

  testCase "lower Constrained Fixed" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Constrained(Fixed 42, Empty))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 6uy
    n.ConstraintKind |> Expect.equal "ck" 0uy
    n.ConstraintVal |> Expect.equal "cv" 42s

  testCase "lower Constrained Fill" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Constrained(Fill, Empty))
    let n = FrameArena.getNode h a
    n.ConstraintKind |> Expect.equal "ck" 4uy
    n.ConstraintVal |> Expect.equal "cv" 0s

  testCase "lower Bordered Rounded" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Bordered(Rounded, Empty))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 7uy
    n.ConstraintKind |> Expect.equal "border" 3uy

  testCase "lower Padded" <| fun () ->
    let a = FrameArena.create 10 100 50
    let p = { Top = 1; Right = 2; Bottom = 3; Left = 4 }
    let h = Arena.lower a (Padded(p, Empty))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 8uy
    (n.DataStart >>> 16) |> Expect.equal "top" 1
    (n.DataStart &&& 0xFFFF) |> Expect.equal "right" 2
    (n.DataLen >>> 16) |> Expect.equal "bottom" 3
    (n.DataLen &&& 0xFFFF) |> Expect.equal "left" 4

  testCase "lower Keyed stores key" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Keyed("item-1", Fade 200<ms>, Fade 0<ms>, Empty))
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 9uy
    System.String(a.TextBuf, n.DataStart, n.DataLen) |> Expect.equal "key" "item-1"
    n.ConstraintKind |> Expect.equal "enter" 0uy
    int16 n.ConstraintVal |> Expect.equal "exit" 0s
    (n.FirstChild >= 0) |> Expect.isTrue "has child"

  testCase "lower Canvas" <| fun () ->
    let a = FrameArena.create 10 100 50
    let cfg = { Draw = (fun _ _ -> { Width = 0; Height = 0; Pixels = [||] })
                Mode = HalfBlock; Fallback = Some Braille }
    let h = Arena.lower a (Canvas cfg)
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 10uy
    n.ConstraintKind |> Expect.equal "mode" 1uy

  testCase "node count matches element count" <| fun () ->
    let a = FrameArena.create 100 1000 500
    Arena.lower a (Row [
      Text("A", Style.empty)
      Column [Text("B", Style.empty); Text("C", Style.empty)]
      Empty
    ]) |> ignore
    a.NodeCount |> Expect.equal "total" 6

  testCase "deeply nested structure" <| fun () ->
    let a = FrameArena.create 100 1000 500
    let tree = Styled(Style.empty, Bordered(Light, Padded(Padding.zero, Text("deep", Style.empty))))
    let h = Arena.lower a tree
    let root = FrameArena.getNode h a
    root.Kind |> Expect.equal "styled" 5uy
    let c1 = a.Nodes.[root.FirstChild]
    c1.Kind |> Expect.equal "bordered" 7uy
    let c2 = a.Nodes.[c1.FirstChild]
    c2.Kind |> Expect.equal "padded" 8uy
    let leaf = a.Nodes.[c2.FirstChild]
    leaf.Kind |> Expect.equal "text" 1uy
    a.NodeCount |> Expect.equal "nodes" 4

  testCase "reset then reuse arena" <| fun () ->
    let a = FrameArena.create 100 1000 500
    Arena.lower a (Text("frame1", Style.empty)) |> ignore
    a.NodeCount |> Expect.equal "f1" 1
    FrameArena.reset a
    Arena.lower a (Row [Text("a", Style.empty); Text("b", Style.empty)]) |> ignore
    a.NodeCount |> Expect.equal "f2" 3
    a.Generation |> Expect.equal "gen" 1

  testCase "empty children list" <| fun () ->
    let a = FrameArena.create 10 100 50
    let h = Arena.lower a (Row [])
    let n = FrameArena.getNode h a
    n.Kind |> Expect.equal "kind" 2uy
    n.FirstChild |> Expect.equal "no children" -1
]

let packStyleTests = testList "packStylePair" [
  testCase "empty style packs to zero" <| fun () ->
    Arena.packStylePair Style.empty |> Expect.equal "zero" 0UL

  testCase "fg in upper 32 bits" <| fun () ->
    let style = { Style.empty with Fg = Some (Named(Red, Normal)) }
    let packed = Arena.packStylePair style
    PackedColor.unpack (Arena.unpackStyleFg packed)
    |> Expect.equal "fg" (Named(Red, Normal))
    Arena.unpackStyleBg packed |> Expect.equal "bg zero" 0

  testCase "bg in lower 32 bits" <| fun () ->
    let style = { Style.empty with Bg = Some (Rgb(1uy, 2uy, 3uy)) }
    let packed = Arena.packStylePair style
    Arena.unpackStyleFg packed |> Expect.equal "fg zero" 0
    PackedColor.unpack (Arena.unpackStyleBg packed)
    |> Expect.equal "bg" (Rgb(1uy, 2uy, 3uy))

  testCase "both fg and bg" <| fun () ->
    let style =
      { Fg = Some (Named(Blue, Bright))
        Bg = Some (Ansi256 42uy)
        Attrs = TextAttrs.none }
    let packed = Arena.packStylePair style
    PackedColor.unpack (Arena.unpackStyleFg packed)
    |> Expect.equal "fg" (Named(Blue, Bright))
    PackedColor.unpack (Arena.unpackStyleBg packed)
    |> Expect.equal "bg" (Ansi256 42uy)

  testProperty "Rgb roundtrip through packStylePair" <| fun (r: byte) (g: byte) (b: byte) ->
    let c = Rgb(r, g, b)
    let style = { Fg = Some c; Bg = Some c; Attrs = TextAttrs.none }
    let packed = Arena.packStylePair style
    let fg = PackedColor.unpack (Arena.unpackStyleFg packed)
    let bg = PackedColor.unpack (Arena.unpackStyleBg packed)
    fg = c && bg = c
]

let constraintPackTests = testList "packConstraint" [
  testCase "Fixed" <| fun () ->
    let (k, v) = Arena.packConstraint (Fixed 42)
    k |> Expect.equal "k" 0uy
    v |> Expect.equal "v" 42s

  testCase "Min" <| fun () ->
    Arena.packConstraint (Min 10) |> fst |> Expect.equal "k" 1uy

  testCase "Max" <| fun () ->
    Arena.packConstraint (Max 100) |> fst |> Expect.equal "k" 2uy

  testCase "Percentage" <| fun () ->
    let (k, v) = Arena.packConstraint (Percentage 50)
    k |> Expect.equal "k" 3uy
    v |> Expect.equal "v" 50s

  testCase "Fill" <| fun () ->
    let (k, v) = Arena.packConstraint Fill
    k |> Expect.equal "k" 4uy
    v |> Expect.equal "v" 0s

  testCase "Ratio" <| fun () ->
    let (k, v) = Arena.packConstraint (Ratio(1, 3))
    k |> Expect.equal "k" 5uy
    v |> Expect.equal "v" 259s
]

// ============================================================
// Phase 3: Terminal Backend Tests
// ============================================================

let e = "\x1b["

let ansiCursorTests = testList "Ansi cursor/screen" [
  testCase "moveCursor 0 0" <| fun () ->
    Ansi.moveCursor 0 0 |> Expect.equal "origin" (sprintf "%s1;1H" e)

  testCase "moveCursor 5 10" <| fun () ->
    Ansi.moveCursor 5 10 |> Expect.equal "5,10" (sprintf "%s6;11H" e)

  testCase "hideCursor" <| fun () ->
    Ansi.hideCursor |> Expect.stringContains "esc" "?25l"

  testCase "showCursor" <| fun () ->
    Ansi.showCursor |> Expect.stringContains "esc" "?25h"

  testCase "enterAltScreen" <| fun () ->
    Ansi.enterAltScreen |> Expect.stringContains "esc" "?1049h"

  testCase "leaveAltScreen" <| fun () ->
    Ansi.leaveAltScreen |> Expect.stringContains "esc" "?1049l"

  testCase "resetStyle" <| fun () ->
    Ansi.resetStyle |> Expect.equal "reset" (sprintf "%s0m" e)
]

let ansiFgTests = testList "Ansi fgColor" [
  testCase "Default" <| fun () ->
    Ansi.fgColor Default |> Expect.equal "default" (sprintf "%s39m" e)

  testCase "Named Red Normal" <| fun () ->
    Ansi.fgColor (Named(Red, Normal)) |> Expect.equal "red" (sprintf "%s31m" e)

  testCase "Named Red Bright" <| fun () ->
    Ansi.fgColor (Named(Red, Bright)) |> Expect.equal "bright red" (sprintf "%s91m" e)

  testCase "Named Black Normal" <| fun () ->
    Ansi.fgColor (Named(Black, Normal)) |> Expect.equal "black" (sprintf "%s30m" e)

  testCase "Named White Bright" <| fun () ->
    Ansi.fgColor (Named(White, Bright)) |> Expect.equal "bright white" (sprintf "%s97m" e)

  testCase "Ansi256 42" <| fun () ->
    Ansi.fgColor (Ansi256 42uy) |> Expect.equal "256" (sprintf "%s38;5;42m" e)

  testCase "Rgb 255 128 0" <| fun () ->
    Ansi.fgColor (Rgb(255uy, 128uy, 0uy)) |> Expect.equal "rgb" (sprintf "%s38;2;255;128;0m" e)
]

let ansiBgTests = testList "Ansi bgColor" [
  testCase "Default" <| fun () ->
    Ansi.bgColor Default |> Expect.equal "default" (sprintf "%s49m" e)

  testCase "Named Blue Normal" <| fun () ->
    Ansi.bgColor (Named(Blue, Normal)) |> Expect.equal "blue" (sprintf "%s44m" e)

  testCase "Named Blue Bright" <| fun () ->
    Ansi.bgColor (Named(Blue, Bright)) |> Expect.equal "bright blue" (sprintf "%s104m" e)

  testCase "Ansi256 100" <| fun () ->
    Ansi.bgColor (Ansi256 100uy) |> Expect.equal "256" (sprintf "%s48;5;100m" e)

  testCase "Rgb 0 255 0" <| fun () ->
    Ansi.bgColor (Rgb(0uy, 255uy, 0uy)) |> Expect.equal "rgb" (sprintf "%s48;2;0;255;0m" e)
]

let ansiAttrsTests = testList "Ansi textAttrs" [
  testCase "none produces empty" <| fun () ->
    Ansi.textAttrs TextAttrs.none |> Expect.equal "empty" ""

  testCase "bold" <| fun () ->
    Ansi.textAttrs TextAttrs.bold |> Expect.equal "bold" (sprintf "%s1m" e)

  testCase "dim" <| fun () ->
    Ansi.textAttrs TextAttrs.dim |> Expect.equal "dim" (sprintf "%s2m" e)

  testCase "italic" <| fun () ->
    Ansi.textAttrs TextAttrs.italic |> Expect.equal "italic" (sprintf "%s3m" e)

  testCase "underline" <| fun () ->
    Ansi.textAttrs TextAttrs.underline |> Expect.equal "underline" (sprintf "%s4m" e)

  testCase "bold+italic combined" <| fun () ->
    let combined = TextAttrs.combine TextAttrs.bold TextAttrs.italic
    let s = Ansi.textAttrs combined
    s |> Expect.stringContains "has bold" (sprintf "%s1m" e)
    s |> Expect.stringContains "has italic" (sprintf "%s3m" e)

  testCase "packed roundtrip" <| fun () ->
    Ansi.textAttrsPacked TextAttrs.bold.Value
    |> Expect.equal "bold" (sprintf "%s1m" e)
]

let ansiAllBaseColorsTests = testList "Ansi all 8 base colors" [
  testCase "all fg normal codes" <| fun () ->
    let bases = [Black; Red; Green; Yellow; Blue; Magenta; Cyan; White]
    let codes = bases |> List.map (fun b -> Ansi.fgColor (Named(b, Normal)))
    codes |> List.length |> Expect.equal "count" 8
    codes |> List.forall (fun s -> s.StartsWith(e)) |> Expect.isTrue "all start with esc"

  testCase "all bg bright codes" <| fun () ->
    let bases = [Black; Red; Green; Yellow; Blue; Magenta; Cyan; White]
    let codes = bases |> List.map (fun b -> Ansi.bgColor (Named(b, Bright)))
    codes |> List.length |> Expect.equal "count" 8
    codes |> List.forall (fun s -> s.Contains("10") || s.Contains("100") || s.Contains("101") || s.Contains("102") || s.Contains("103") || s.Contains("104") || s.Contains("105") || s.Contains("106") || s.Contains("107"))
    |> Expect.isTrue "all bright bg codes"
]

let presenterTests = testList "Presenter" [
  testCase "empty changes produces empty string" <| fun () ->
    let buf = Buffer.create 10 5
    Presenter.present (ResizeArray<int>()) buf |> Expect.equal "empty" ""

  testCase "single change includes cursor move and char" <| fun () ->
    let buf = Buffer.create 10 5
    let fg = PackedColor.pack (Named(Red, Normal))
    buf.Cells[23] <- PackedCell.create (int (System.Text.Rune 'X').Value) fg 0 0us
    let changes = ResizeArray<int>([23])
    let s = Presenter.present changes buf
    s |> Expect.stringContains "has cursor move" (Ansi.moveCursor 2 3)
    s |> Expect.stringContains "has char" "X"

  testCase "contiguous cells skip cursor move" <| fun () ->
    let buf = Buffer.create 10 5
    buf.Cells[0] <- PackedCell.create (int (System.Text.Rune 'A').Value) 0 0 0us
    buf.Cells[1] <- PackedCell.create (int (System.Text.Rune 'B').Value) 0 0 0us
    let changes = ResizeArray<int>([0; 1])
    let s = Presenter.present changes buf
    s |> Expect.stringContains "has AB" "AB"

  testCase "non-contiguous cells emit multiple moves" <| fun () ->
    let buf = Buffer.create 10 5
    buf.Cells[0] <- PackedCell.create (int (System.Text.Rune 'A').Value) 0 0 0us
    buf.Cells[5] <- PackedCell.create (int (System.Text.Rune 'B').Value) 0 0 0us
    let changes = ResizeArray<int>([0; 5])
    let s = Presenter.present changes buf
    s |> Expect.stringContains "has first move" (Ansi.moveCursor 0 0)
    s |> Expect.stringContains "has second move" (Ansi.moveCursor 0 5)

  testCase "style change emits reset + style codes" <| fun () ->
    let buf = Buffer.create 10 5
    let fg1 = PackedColor.pack (Named(Red, Normal))
    let fg2 = PackedColor.pack (Named(Blue, Normal))
    buf.Cells[0] <- PackedCell.create (int (System.Text.Rune 'R').Value) fg1 0 0us
    buf.Cells[1] <- PackedCell.create (int (System.Text.Rune 'B').Value) fg2 0 0us
    let changes = ResizeArray<int>([0; 1])
    let s = Presenter.present changes buf
    s |> Expect.stringContains "has R" "R"
    s |> Expect.stringContains "has B" "B"
    let resetCount = s.Split(Ansi.resetStyle).Length - 1
    (resetCount >= 2) |> Expect.isTrue "style changes emit resets"

  testCase "same style across contiguous cells: no redundant style" <| fun () ->
    let buf = Buffer.create 10 5
    let fg = PackedColor.pack (Named(Red, Normal))
    buf.Cells[0] <- PackedCell.create (int (System.Text.Rune 'A').Value) fg 0 0us
    buf.Cells[1] <- PackedCell.create (int (System.Text.Rune 'B').Value) fg 0 0us
    let changes = ResizeArray<int>([0; 1])
    let s = Presenter.present changes buf
    let resetCount = s.Split(Ansi.resetStyle).Length - 1
    resetCount |> Expect.equal "one style set" 1
]

let mkProfile color =
  { Color = color; Unicode = UnicodeCapability.FullUnicode
    Graphics = GraphicsCapability.TextOnly; Input = InputCapability.BasicKeys
    Output = OutputCapability.RawMode; Size = (80, 25); TermName = "xterm"; Platform = Linux }

let colorFallbackTests = testList "ColorFallback" [
  testCase "TrueColor passes through Rgb" <| fun () ->
    ColorFallback.resolve (mkProfile ColorCapability.TrueColor) (Rgb(100uy, 200uy, 50uy))
    |> Expect.equal "pass through" (Rgb(100uy, 200uy, 50uy))

  testCase "Indexed256 converts Rgb to Ansi256" <| fun () ->
    let result = ColorFallback.resolve (mkProfile ColorCapability.Indexed256) (Rgb(255uy, 0uy, 0uy))
    match result with
    | Ansi256 _ -> ()
    | other -> failwith (sprintf "expected Ansi256, got %A" other)

  testCase "Basic16 converts Rgb to Named" <| fun () ->
    let result = ColorFallback.resolve (mkProfile ColorCapability.Basic16) (Rgb(255uy, 0uy, 0uy))
    match result with
    | Named(Red, Bright) -> ()
    | other -> failwith (sprintf "expected Named Red Bright, got %A" other)

  testCase "NoColor converts Rgb to Default" <| fun () ->
    ColorFallback.resolve (mkProfile ColorCapability.NoColor) (Rgb(100uy, 200uy, 50uy))
    |> Expect.equal "default" Default

  testCase "NoColor converts Ansi256 to Default" <| fun () ->
    ColorFallback.resolve (mkProfile ColorCapability.NoColor) (Ansi256 42uy)
    |> Expect.equal "default" Default

  testCase "Named passes through on Basic16" <| fun () ->
    ColorFallback.resolve (mkProfile ColorCapability.Basic16) (Named(Red, Normal))
    |> Expect.equal "passthrough" (Named(Red, Normal))

  testCase "Default always passes through" <| fun () ->
    ColorFallback.resolve (mkProfile ColorCapability.NoColor) Default
    |> Expect.equal "default" Default

  testCase "redmeanDistance identical colors is zero" <| fun () ->
    ColorFallback.redmeanDistance (100uy, 100uy, 100uy) (100uy, 100uy, 100uy)
    |> Expect.equal "zero" 0.0

  testCase "redmeanDistance black-white is large" <| fun () ->
    let d = ColorFallback.redmeanDistance (0uy, 0uy, 0uy) (255uy, 255uy, 255uy)
    (d > 500.0) |> Expect.isTrue "large distance"

  testCase "toBasic16 pure white maps to White Bright" <| fun () ->
    ColorFallback.toBasic16 (255uy, 255uy, 255uy)
    |> Expect.equal "white" (White, Bright)

  testCase "toBasic16 pure black maps to Black Normal" <| fun () ->
    ColorFallback.toBasic16 (0uy, 0uy, 0uy)
    |> Expect.equal "black" (Black, Normal)
]

let syncOutputTests = testList "SynchronizedOutput" [
  testCase "wraps with sync when capable" <| fun () ->
    let profile =
      { Color = ColorCapability.TrueColor; Unicode = UnicodeCapability.FullUnicode
        Graphics = GraphicsCapability.TextOnly; Input = InputCapability.BasicKeys
        Output = OutputCapability.SynchronizedOutput; Size = (80, 25); TermName = "wt"; Platform = Windows }
    let result = SynchronizedOutput.wrap profile "content"
    result |> Expect.stringStarts "begins with sync" SynchronizedOutput.beginSync
    result |> Expect.stringEnds "ends with sync" SynchronizedOutput.endSync
    result |> Expect.stringContains "has content" "content"

  testCase "no wrap when not supported" <| fun () ->
    let profile =
      { Color = ColorCapability.TrueColor; Unicode = UnicodeCapability.FullUnicode
        Graphics = GraphicsCapability.TextOnly; Input = InputCapability.BasicKeys
        Output = OutputCapability.RawMode; Size = (80, 25); TermName = "xterm"; Platform = Linux }
    SynchronizedOutput.wrap profile "content" |> Expect.equal "passthrough" "content"
]

// ============================================================
// Phase 4: Layout Engine + Renderer Tests
// ============================================================

let area4 w h = { X = 0; Y = 0; Width = w; Height = h }
let areaAt x y w h = { X = x; Y = y; Width = w; Height = h }

let layoutSolveTests = testList "Layout.solve" [
  testCase "single Fixed" <| fun () ->
    Layout.solve 100 [Fixed 30]
    |> Expect.equal "one fixed" [(0, 30)]

  testCase "two Fixed" <| fun () ->
    Layout.solve 100 [Fixed 30; Fixed 40]
    |> Expect.equal "two fixed" [(0, 30); (30, 40)]

  testCase "single Fill takes all" <| fun () ->
    Layout.solve 80 [Fill]
    |> Expect.equal "fill" [(0, 80)]

  testCase "two Fill splits evenly" <| fun () ->
    Layout.solve 80 [Fill; Fill]
    |> Expect.equal "even" [(0, 40); (40, 40)]

  testCase "three Fill with remainder" <| fun () ->
    Layout.solve 100 [Fill; Fill; Fill]
    |> Expect.equal "3 fill" [(0, 34); (34, 33); (67, 33)]

  testCase "Fixed + Fill" <| fun () ->
    Layout.solve 100 [Fixed 20; Fill]
    |> Expect.equal "fixed+fill" [(0, 20); (20, 80)]

  testCase "Percentage 50" <| fun () ->
    Layout.solve 100 [Percentage 50]
    |> Expect.equal "50%" [(0, 50)]

  testCase "Ratio 1/3" <| fun () ->
    Layout.solve 90 [Ratio(1, 3)]
    |> Expect.equal "1/3" [(0, 30)]

  testCase "Min sets floor" <| fun () ->
    Layout.solve 100 [Min 20]
    |> Expect.equal "min" [(0, 20)]

  testCase "Max caps" <| fun () ->
    Layout.solve 100 [Max 50]
    |> Expect.equal "max" [(0, 50)]

  testCase "Fixed exceeds available" <| fun () ->
    Layout.solve 10 [Fixed 30]
    |> Expect.equal "capped" [(0, 10)]

  testCase "empty constraints" <| fun () ->
    Layout.solve 100 []
    |> Expect.equal "empty" []

  testCase "complex: Fixed + Fill + Fixed" <| fun () ->
    Layout.solve 100 [Fixed 20; Fill; Fixed 20]
    |> Expect.equal "sidebar" [(0, 20); (20, 60); (80, 20)]

  testCase "Percentage + Fill" <| fun () ->
    Layout.solve 100 [Percentage 30; Fill]
    |> Expect.equal "pct+fill" [(0, 30); (30, 70)]
]

let layoutSplitTests = testList "Layout.splitH/splitV" [
  testCase "splitH even" <| fun () ->
    Layout.splitH [Fill; Fill] (area4 80 24)
    |> Expect.equal "h split" [areaAt 0 0 40 24; areaAt 40 0 40 24]

  testCase "splitV even" <| fun () ->
    Layout.splitV [Fill; Fill] (area4 80 24)
    |> Expect.equal "v split" [areaAt 0 0 80 12; areaAt 0 12 80 12]

  testCase "splitH with offset area" <| fun () ->
    Layout.splitH [Fixed 10; Fill] (areaAt 5 3 80 24)
    |> Expect.equal "offset" [areaAt 5 3 10 24; areaAt 15 3 70 24]

  testCase "splitV with offset area" <| fun () ->
    Layout.splitV [Fixed 5; Fill] (areaAt 5 3 80 24)
    |> Expect.equal "offset" [areaAt 5 3 80 5; areaAt 5 8 80 19]
]

let renderTextTests = testList "Render text" [
  testCase "Empty renders nothing" <| fun () ->
    let buf = Buffer.create 10 3
    Render.render (area4 10 3) Style.empty buf Empty
    Buffer.toString buf |> Expect.equal "blank" (String.replicate 10 " " + "\n" + String.replicate 10 " " + "\n" + String.replicate 10 " ")

  testCase "Text renders at position" <| fun () ->
    let buf = Buffer.create 10 1
    Render.render (area4 10 1) Style.empty buf (El.text "Hello")
    let s = Buffer.toString buf
    s |> Expect.stringStarts "starts with Hello" "Hello"

  testCase "Text with style applies fg" <| fun () ->
    let buf = Buffer.create 10 1
    let style = { Style.empty with Fg = Some (Named(Red, Normal)) }
    Render.render (area4 10 1) Style.empty buf (Text("Hi", style))
    let cell = Buffer.get 0 0 buf
    let expectedFg = PackedColor.pack (Named(Red, Normal))
    cell.Fg |> Expect.equal "fg red" expectedFg

  testCase "Styled wraps fg" <| fun () ->
    let buf = Buffer.create 10 1
    let elem = El.text "X" |> El.fg (Named(Blue, Bright))
    Render.render (area4 10 1) Style.empty buf elem
    let cell = Buffer.get 0 0 buf
    cell.Fg |> Expect.equal "fg blue" (PackedColor.pack (Named(Blue, Bright)))

  testCase "zero area renders nothing" <| fun () ->
    let buf = Buffer.create 10 1
    Render.render { X = 0; Y = 0; Width = 0; Height = 1 } Style.empty buf (El.text "X")
    Buffer.toString buf |> Expect.equal "blank" (String.replicate 10 " ")
]

let renderRowColTests = testList "Render Row/Column" [
  testCase "Row splits evenly" <| fun () ->
    let buf = Buffer.create 10 1
    let elem = Row [El.text "AB"; El.text "CD"]
    Render.render (area4 10 1) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "A" 'A'
    s.[1] |> Expect.equal "B" 'B'
    s.[5] |> Expect.equal "C" 'C'
    s.[6] |> Expect.equal "D" 'D'

  testCase "Column splits evenly" <| fun () ->
    let buf = Buffer.create 5 4
    let elem = Column [El.text "Top"; El.text "Bot"]
    Render.render (area4 5 4) Style.empty buf elem
    let cell00 = Buffer.get 0 0 buf
    cell00.Rune |> Expect.equal "T" (int (System.Text.Rune 'T').Value)
    let cell02 = Buffer.get 0 2 buf
    cell02.Rune |> Expect.equal "B" (int (System.Text.Rune 'B').Value)

  testCase "Row with three Fill children" <| fun () ->
    let buf = Buffer.create 9 1
    let elem = Row [El.text "A"; El.text "B"; El.text "C"]
    Render.render (area4 9 1) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "A" 'A'
    s.[3] |> Expect.equal "B" 'B'
    s.[6] |> Expect.equal "C" 'C'

  testCase "nested Row in Column" <| fun () ->
    let buf = Buffer.create 6 2
    let elem = Column [Row [El.text "AB"; El.text "CD"]; El.text "EF"]
    Render.render (area4 6 2) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "A" 'A'
    s.[3] |> Expect.equal "C" 'C'
    let cell01 = Buffer.get 0 1 buf
    cell01.Rune |> Expect.equal "E" (int (System.Text.Rune 'E').Value)
]

let renderOverlayTests = testList "Render Overlay" [
  testCase "Overlay last wins" <| fun () ->
    let buf = Buffer.create 5 1
    let elem = Overlay [El.text "AAAAA"; El.text "BB"]
    Render.render (area4 5 1) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "B overwrites A" 'B'
    s.[1] |> Expect.equal "B overwrites A" 'B'
    s.[2] |> Expect.equal "A remains" 'A'
]

let renderConstrainedTests = testList "Render Constrained" [
  testCase "Fixed width constrains child" <| fun () ->
    let buf = Buffer.create 10 1
    let elem = Constrained(Fixed 3, El.text "Hello")
    Render.render (area4 10 1) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "H" 'H'
    s.[1] |> Expect.equal "e" 'e'
    s.[2] |> Expect.equal "l" 'l'
    s.[3] |> Expect.equal "space" ' '

  testCase "Max width caps" <| fun () ->
    let buf = Buffer.create 10 1
    let elem = Constrained(Max 2, El.text "Hello")
    Render.render (area4 10 1) Style.empty buf elem
    let s = Buffer.toString buf
    s.[0] |> Expect.equal "H" 'H'
    s.[1] |> Expect.equal "e" 'e'
    s.[2] |> Expect.equal "space" ' '
]

let renderBorderTests = testList "Render Border" [
  testCase "Light border corners" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Light, Empty))
    let tl = Buffer.get 0 0 buf
    tl.Rune |> Expect.equal "top-left" (int (System.Text.Rune '\u250C').Value)
    let tr = Buffer.get 4 0 buf
    tr.Rune |> Expect.equal "top-right" (int (System.Text.Rune '\u2510').Value)
    let bl = Buffer.get 0 2 buf
    bl.Rune |> Expect.equal "bot-left" (int (System.Text.Rune '\u2514').Value)
    let br = Buffer.get 4 2 buf
    br.Rune |> Expect.equal "bot-right" (int (System.Text.Rune '\u2518').Value)

  testCase "Light border horizontal bars" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Light, Empty))
    let top = Buffer.get 2 0 buf
    top.Rune |> Expect.equal "h bar" (int (System.Text.Rune '\u2500').Value)

  testCase "Light border vertical bars" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Light, Empty))
    let left = Buffer.get 0 1 buf
    left.Rune |> Expect.equal "v bar" (int (System.Text.Rune '\u2502').Value)

  testCase "Bordered child renders inside" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Light, El.text "X"))
    let inner = Buffer.get 1 1 buf
    inner.Rune |> Expect.equal "X inside" (int (System.Text.Rune 'X').Value)

  testCase "Ascii border" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Ascii, Empty))
    let tl = Buffer.get 0 0 buf
    tl.Rune |> Expect.equal "+" (int (System.Text.Rune '+').Value)
    let h = Buffer.get 2 0 buf
    h.Rune |> Expect.equal "-" (int (System.Text.Rune '-').Value)

  testCase "Heavy border corners" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Heavy, Empty))
    let tl = Buffer.get 0 0 buf
    tl.Rune |> Expect.equal "heavy tl" (int (System.Text.Rune '\u250F').Value)

  testCase "Rounded border corners" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Rounded, Empty))
    let tl = Buffer.get 0 0 buf
    tl.Rune |> Expect.equal "rounded tl" (int (System.Text.Rune '\u256D').Value)

  testCase "Double border corners" <| fun () ->
    let buf = Buffer.create 5 3
    Render.render (area4 5 3) Style.empty buf (Bordered(Double, Empty))
    let tl = Buffer.get 0 0 buf
    tl.Rune |> Expect.equal "double tl" (int (System.Text.Rune '\u2554').Value)
]

let renderPaddedTests = testList "Render Padded" [
  testCase "padding offsets child" <| fun () ->
    let buf = Buffer.create 10 5
    let pad = { Top = 1; Right = 1; Bottom = 1; Left = 2 }
    let elem = Padded(pad, El.text "X")
    Render.render (area4 10 5) Style.empty buf elem
    let cell = Buffer.get 2 1 buf
    cell.Rune |> Expect.equal "X at (2,1)" (int (System.Text.Rune 'X').Value)
    let empty = Buffer.get 0 0 buf
    empty.Rune |> Expect.equal "space at origin" (int (System.Text.Rune ' ').Value)

  testCase "padAll wraps evenly" <| fun () ->
    let buf = Buffer.create 10 5
    let elem = El.text "Y" |> El.padAll 2
    Render.render (area4 10 5) Style.empty buf elem
    let cell = Buffer.get 2 2 buf
    cell.Rune |> Expect.equal "Y at (2,2)" (int (System.Text.Rune 'Y').Value)
]

let renderKeyedTests = testList "Render Keyed" [
  testCase "Keyed renders child transparently" <| fun () ->
    let buf = Buffer.create 5 1
    let elem = El.text "K" |> El.keyed "k1"
    Render.render (area4 5 1) Style.empty buf elem
    let cell = Buffer.get 0 0 buf
    cell.Rune |> Expect.equal "K" (int (System.Text.Rune 'K').Value)
]

let renderInheritedStyleTests = testList "Render inherited style" [
  testCase "inherited fg applies to child text" <| fun () ->
    let buf = Buffer.create 5 1
    let style = { Style.empty with Fg = Some (Named(Green, Bright)) }
    Render.render (area4 5 1) style buf (El.text "G")
    let cell = Buffer.get 0 0 buf
    cell.Fg |> Expect.equal "green" (PackedColor.pack (Named(Green, Bright)))

  testCase "local style overrides inherited" <| fun () ->
    let buf = Buffer.create 5 1
    let inherited = { Style.empty with Fg = Some (Named(Green, Normal)) }
    let local = { Style.empty with Fg = Some (Named(Red, Normal)) }
    Render.render (area4 5 1) inherited buf (Text("R", local))
    let cell = Buffer.get 0 0 buf
    cell.Fg |> Expect.equal "red overrides" (PackedColor.pack (Named(Red, Normal)))

  testCase "Styled merges with inherited" <| fun () ->
    let buf = Buffer.create 5 1
    let inherited = { Style.empty with Fg = Some (Named(Green, Normal)) }
    let overlay = { Style.empty with Attrs = TextAttrs.bold }
    let elem = Styled(overlay, El.text "B")
    Render.render (area4 5 1) inherited buf elem
    let cell = Buffer.get 0 0 buf
    cell.Fg |> Expect.equal "inherited fg" (PackedColor.pack (Named(Green, Normal)))
    cell.Attrs |> Expect.equal "bold" TextAttrs.bold.Value
]

[<Tests>]
let allTests = testList "All" [
  testList "Phase 0" [
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
  testList "Phase 1" [
    packedCellTests
    runeWidthTests
    bufferBasicTests
    bufferWriteTests
    bufferDiffTests
    bufferToStringTests
  ]
  testList "Phase 2" [
    arenaInfraTests
    arenaLowerTests
    packStyleTests
    constraintPackTests
  ]
  testList "Phase 3" [
    ansiCursorTests
    ansiFgTests
    ansiBgTests
    ansiAttrsTests
    ansiAllBaseColorsTests
    presenterTests
    colorFallbackTests
    syncOutputTests
  ]
  testList "Phase 4" [
    layoutSolveTests
    layoutSplitTests
    renderTextTests
    renderRowColTests
    renderOverlayTests
    renderConstrainedTests
    renderBorderTests
    renderPaddedTests
    renderKeyedTests
    renderInheritedStyleTests
  ]
]
