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
]
