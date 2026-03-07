module ColorPalette

// Showcase the full color system: 16 named, 256 palette, RGB gradients, text attributes.
// Demonstrates: Color DU, TextAttrs, Gradient.rainbow, Gradient.horizontal, Spinner, TimerSub.

open System
open SageTUI

type Tab = Base16 | Palette256 | TrueColor | TextStyles

type Model = { Tab: Tab; Elapsed: int64 }

type Msg =
  | SwitchTab of Tab
  | Tick
  | Quit

let init () = { Tab = Base16; Elapsed = 0L }, Cmd.none

let update msg model =
  match msg with
  | SwitchTab t -> { model with Tab = t }, Cmd.none
  | Tick -> { model with Elapsed = model.Elapsed + 200L }, Cmd.none
  | Quit -> model, Cmd.quit

let tabBar (active: Tab) =
  let tab label t =
    match t = active with
    | true ->
      El.text (sprintf " %s " label)
        |> El.bold
        |> El.fg (Color.Named(Black, Normal))
        |> El.bg (Color.Named(Cyan, Bright))
    | false ->
      El.text (sprintf " %s " label)
        |> El.fg (Color.Named(Cyan, Normal))
  El.row [
    tab "1: Base-16" Base16
    El.text " │ " |> El.dim
    tab "2: 256-Color" Palette256
    El.text " │ " |> El.dim
    tab "3: TrueColor" TrueColor
    El.text " │ " |> El.dim
    tab "4: Text Styles" TextStyles
  ]

let base16View =
  let colorBlock (name: string) (bc: BaseColor) =
    El.row [
      El.text "  "
      El.text "████" |> El.fg (Color.Named(bc, Normal))
      El.text " "
      El.text "████" |> El.fg (Color.Named(bc, Bright))
      El.text (sprintf "  %-10s" name)
    ]
  El.column [
    El.text " Base-16 Terminal Colors" |> El.bold |> El.fg (Color.Named(Yellow, Bright))
    El.text ""
    El.row [
      El.text (sprintf "  %-4s %-7s %-11s" "" "Normal" "Bright") |> El.dim
    ]
    colorBlock "Black" Black
    colorBlock "Red" Red
    colorBlock "Green" Green
    colorBlock "Yellow" Yellow
    colorBlock "Blue" Blue
    colorBlock "Magenta" Magenta
    colorBlock "Cyan" Cyan
    colorBlock "White" White
    El.text ""
    El.text " Foreground on backgrounds:" |> El.dim
    El.row [
      El.text "  "
      El.text " Hello " |> El.fg (Color.Named(White, Bright)) |> El.bg (Color.Named(Red, Normal))
      El.text " Hello " |> El.fg (Color.Named(Black, Normal)) |> El.bg (Color.Named(Green, Bright))
      El.text " Hello " |> El.fg (Color.Named(White, Bright)) |> El.bg (Color.Named(Blue, Normal))
      El.text " Hello " |> El.fg (Color.Named(Black, Normal)) |> El.bg (Color.Named(Yellow, Bright))
      El.text " Hello " |> El.fg (Color.Named(White, Bright)) |> El.bg (Color.Named(Magenta, Normal))
      El.text " Hello " |> El.fg (Color.Named(Black, Normal)) |> El.bg (Color.Named(Cyan, Bright))
    ]
  ]

let palette256View =
  El.column [
    El.text " 256-Color Palette" |> El.bold |> El.fg (Color.Named(Yellow, Bright))
    El.text ""
    El.text " Standard (0-15):" |> El.dim
    El.row [
      El.text "  "
      yield! [ for i in 0..15 -> El.text "██" |> El.fg (Color.Ansi256(byte i)) ]
    ]
    El.text ""
    El.text " Color Cube (16-231):" |> El.dim
    yield!
      [ for row in 0..5 ->
          El.row [
            El.text "  "
            yield! [ for g in 0..5 do
                       for b in 0..5 ->
                         El.text "▐" |> El.fg (Color.Ansi256(byte (16 + row * 36 + g * 6 + b))) ]
            El.text " "
          ] ]
    El.text ""
    El.text " Grayscale (232-255):" |> El.dim
    El.row [
      El.text "  "
      yield! [ for i in 232..255 -> El.text "██" |> El.fg (Color.Ansi256(byte i)) ]
    ]
  ]

let trueColorView =
  El.column [
    El.text " TrueColor RGB Gradients" |> El.bold |> El.fg (Color.Named(Yellow, Bright))
    El.text ""
    El.text " Red gradient:" |> El.dim
    El.row [
      El.text "  "
      yield! [ for i in 0..31 ->
                 let v = byte (i * 8)
                 El.text "█" |> El.fg (Color.Rgb(v, 0uy, 0uy)) ]
    ]
    El.text " Green gradient:" |> El.dim
    El.row [
      El.text "  "
      yield! [ for i in 0..31 ->
                 let v = byte (i * 8)
                 El.text "█" |> El.fg (Color.Rgb(0uy, v, 0uy)) ]
    ]
    El.text " Blue gradient:" |> El.dim
    El.row [
      El.text "  "
      yield! [ for i in 0..31 ->
                 let v = byte (i * 8)
                 El.text "█" |> El.fg (Color.Rgb(0uy, 0uy, v)) ]
    ]
    El.text ""
    El.text " Rainbow:" |> El.dim
    El.row [
      El.text "  "
      Gradient.rainbow 48 (String('█', 48))
    ]
    El.text ""
    El.text " Fire gradient:" |> El.dim
    El.row [
      El.text "  "
      Gradient.horizontal (40uy, 0uy, 0uy) (255uy, 255uy, 80uy) 40 (String('█', 40))
    ]
    El.text ""
    El.text " Ocean gradient:" |> El.dim
    El.row [
      El.text "  "
      Gradient.horizontal (0uy, 20uy, 80uy) (80uy, 220uy, 255uy) 40 (String('█', 40))
    ]
  ]

let textStylesView (elapsed: int64) =
  El.column [
    El.text " Text Attributes" |> El.bold |> El.fg (Color.Named(Yellow, Bright))
    El.text ""
    El.row [
      El.text "  Normal:        "
      El.text "The quick brown fox"
    ]
    El.row [
      El.text "  Bold:          " |> El.dim
      El.text "The quick brown fox" |> El.bold
    ]
    El.row [
      El.text "  Dim:           " |> El.dim
      El.text "The quick brown fox" |> El.dim
    ]
    El.row [
      El.text "  Italic:        " |> El.dim
      El.text "The quick brown fox" |> El.italic
    ]
    El.row [
      El.text "  Underline:     " |> El.dim
      El.text "The quick brown fox" |> El.underline
    ]
    El.row [
      El.text "  Strikethrough: " |> El.dim
      El.text "The quick brown fox" |> El.strikethrough
    ]
    El.row [
      El.text "  Reverse:       " |> El.dim
      El.text "The quick brown fox" |> El.reverse
    ]
    El.text ""
    El.text " Combined styles:" |> El.dim
    El.row [
      El.text "  "
      El.text "Bold+Italic"
        |> El.bold |> El.italic
        |> El.fg (Color.Named(Cyan, Bright))
      El.text "  "
      El.text "Bold+Underline"
        |> El.bold |> El.underline
        |> El.fg (Color.Named(Green, Bright))
      El.text "  "
      El.text "Dim+Italic"
        |> El.dim |> El.italic
        |> El.fg (Color.Named(Magenta, Normal))
    ]
    El.text ""
    El.text " Styled badges:" |> El.dim
    El.row [
      El.text "  "
      El.text " SUCCESS " |> El.bold |> El.fg (Color.Named(Black, Normal)) |> El.bg (Color.Named(Green, Normal))
      El.text " "
      El.text " WARNING " |> El.bold |> El.fg (Color.Named(Black, Normal)) |> El.bg (Color.Named(Yellow, Bright))
      El.text " "
      El.text " ERROR " |> El.bold |> El.fg (Color.Named(White, Bright)) |> El.bg (Color.Named(Red, Normal))
      El.text " "
      El.text " INFO " |> El.bold |> El.fg (Color.Named(White, Bright)) |> El.bg (Color.Named(Blue, Normal))
    ]
    El.text ""
    El.text " Animated spinners:" |> El.dim
    El.row [
      El.text "  Dots: "
      Spinner.dots elapsed |> El.fg (Color.Named(Cyan, Bright))
      El.text "   Line: "
      Spinner.line elapsed |> El.fg (Color.Named(Green, Bright))
      El.text "   "
      Gradient.rainbow 20 "Loading..."
        |> El.bold
    ]
  ]

let view model =
  let header =
    El.row [
      El.text " 🎨 SageTUI Color Palette"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.fill (El.text "")
    ]
    |> El.bg (Color.Named(Black, Normal))

  let content =
    match model.Tab with
    | Base16 -> base16View
    | Palette256 -> palette256View
    | TrueColor -> trueColorView
    | TextStyles -> textStylesView model.Elapsed

  El.column [
    header
    El.text ""
    tabBar model.Tab
    El.text ""
    content |> El.padHV 1 0
    El.fill (El.text "")
    El.text " [1-4] Switch Tab  [q] Quit" |> El.dim
  ]

let subscribe _model =
  [ TimerSub("anim", TimeSpan.FromMilliseconds(200.0), fun () -> Tick)
    Keys.bind [
      Key.Char '1', SwitchTab Base16
      Key.Char '2', SwitchTab Palette256
      Key.Char '3', SwitchTab TrueColor
      Key.Char '4', SwitchTab TextStyles
      Key.Char 'q', Quit
      Key.Char 'Q', Quit
      Key.Escape, Quit
    ] ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _ = App.run program; 0