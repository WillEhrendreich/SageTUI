module Transitions

// Animated transition showcase: Fade, ColorMorph, Wipe, Dissolve, SlideIn.
// Demonstrates: Keyed elements, view transitions, enter/exit animations, timers.

open System
open SageTUI

type AnimKind = FadeAnim | ColorMorphAnim | WipeRight | WipeDown | DissolveAnim | GrowAnim | SlideInRight | SequenceAnim

type Model =
  { Counter: int
    AnimKind: AnimKind
    AutoCycle: bool
    Elapsed: int64
    Width: int }

type Msg =
  | NextContent
  | PrevContent
  | CycleAnim
  | ToggleAuto
  | AutoTick
  | AnimTick
  | Resized of int * int
  | Quit

let allAnims =
  [| FadeAnim; ColorMorphAnim; WipeRight; WipeDown; DissolveAnim
     GrowAnim; SlideInRight; SequenceAnim |]

let animName anim =
  match anim with
  | FadeAnim -> "Fade"
  | ColorMorphAnim -> "ColorMorph"
  | WipeRight -> "Wipe →"
  | WipeDown -> "Wipe ↓"
  | DissolveAnim -> "Dissolve"
  | GrowAnim -> "Grow"
  | SlideInRight -> "SlideIn →"
  | SequenceAnim -> "Sequence"

let animTransition anim =
  match anim with
  | FadeAnim -> Fade 400<ms>
  | ColorMorphAnim -> ColorMorph 500<ms>
  | WipeRight -> Wipe(Direction.Right, 400<ms>)
  | WipeDown -> Wipe(Direction.Down, 400<ms>)
  | DissolveAnim -> Dissolve 600<ms>
  | GrowAnim -> Grow 400<ms>
  | SlideInRight -> SlideIn(Direction.Right, 400<ms>)
  | SequenceAnim -> Sequence [ Fade 200<ms>; ColorMorph 300<ms> ]

let contentCards = [|
  ("🚀 Performance", Color.Rgb(50uy, 180uy, 255uy))
  ("🎨 Rich Styling", Color.Rgb(255uy, 100uy, 200uy))
  ("🌐 HTML Bridge", Color.Rgb(100uy, 255uy, 150uy))
  ("⚡ Architecture", Color.Rgb(255uy, 200uy, 50uy))
  ("🧩 Layout Engine", Color.Rgb(200uy, 130uy, 255uy))
  ("✨ Animations", Color.Rgb(255uy, 150uy, 80uy))
|]

// Build a wide gradient bar for visual impact
let gradientBar w (c1: byte * byte * byte) (c2: byte * byte * byte) =
  Gradient.oklch c1 c2 w (String('█', w))

let cardContent (idx: int) (elapsed: int64) (w: int) =
  let barW = max 10 (w - 8)
  let dim = Rgb(90uy, 90uy, 110uy)
  match idx with
  | 0 -> // Performance — rocket
    let hull = Rgb(180uy, 210uy, 255uy)
    let flame = Rgb(255uy, 140uy, 50uy)
    let accent = Rgb(50uy, 180uy, 255uy)
    El.column [
      El.text ""
      El.text "              ╱╲" |> El.fg hull
      El.text "             ╱  ╲" |> El.fg hull
      El.text "            ╱    ╲" |> El.fg hull
      El.text "           │ ╱──╲ │" |> El.fg hull
      El.text "           │ ╲──╱ │" |> El.fg hull
      El.text "           │ SAGE │" |> El.bold |> El.fg accent
      El.text "           │  TUI │" |> El.bold |> El.fg accent
      El.text "          ╱│      │╲" |> El.fg hull
      El.text "         ╱ └──────┘ ╲" |> El.fg hull
      El.text "         ╲    ╱╲    ╱" |> El.fg flame
      El.text "          ╲ ╱╱╲╲ ╱" |> El.fg (Rgb(255uy, 200uy, 80uy))
      El.text "           ╲╱╲╱╲╱" |> El.fg (Rgb(255uy, 100uy, 30uy))
      El.text ""
      El.text "  ⚡ Zero-allocation rendering pipeline" |> El.fg accent |> El.bold
      El.text ""
      El.text "  Arena allocator    — no GC pressure, ever"
      El.text "  SIMD buffer diff   — skip unchanged chunks at 16B/cycle"
      El.text "  Packed 16B cells   — cache-line aligned, struct-of-arrays"
      El.text ""
      El.row [
        El.text "  Status: "
        Spinner.dots elapsed |> El.fg accent
        El.text " rendering frames..." |> El.fg dim
      ]
      El.text ""
      El.row [ El.text "  "; gradientBar barW (50uy, 180uy, 255uy) (20uy, 60uy, 120uy) ]
      El.row [ El.text "  "; gradientBar barW (20uy, 60uy, 120uy) (50uy, 180uy, 255uy) ]
    ]
  | 1 -> // Rich Styling — paintbrush
    let brush = Rgb(255uy, 100uy, 200uy)
    let wood = Rgb(200uy, 150uy, 80uy)
    El.column [
      El.text ""
      El.text "                ╭───╮" |> El.fg brush
      El.text "               ╱░░░░░╲" |> El.fg (Rgb(255uy, 80uy, 180uy))
      El.text "              ╱░░░░░░░╲" |> El.fg (Rgb(255uy, 60uy, 160uy))
      El.text "             │░░░░░░░░░│" |> El.fg (Rgb(220uy, 50uy, 140uy))
      El.text "              ╲░░░░░░░╱" |> El.fg (Rgb(180uy, 30uy, 100uy))
      El.text "               ╰─┬─┬─╯" |> El.fg brush
      El.text "                 │ │" |> El.fg wood
      El.text "                 │ │" |> El.fg wood
      El.text "                 │ │" |> El.fg wood
      El.text "                 ╰─╯" |> El.fg wood
      El.text ""
      El.text "  🎨 Full 24-bit TrueColor" |> El.fg brush |> El.bold
      El.text ""
      El.row [ El.text "  "; Gradient.rainbow barW (String('█', barW)) ]
      El.row [ El.text "  "; gradientBar barW (255uy, 50uy, 100uy) (100uy, 50uy, 255uy) ]
      El.row [ El.text "  "; gradientBar barW (50uy, 255uy, 100uy) (255uy, 200uy, 50uy) ]
      El.row [ El.text "  "; gradientBar barW (255uy, 150uy, 30uy) (30uy, 150uy, 255uy) ]
      El.row [ El.text "  "; gradientBar barW (200uy, 80uy, 255uy) (80uy, 255uy, 200uy) ]
      El.text ""
      El.row [
        El.text "  "
        El.text "Bold" |> El.bold |> El.fg (Rgb(255uy, 255uy, 100uy))
        El.text "  "
        El.text "Italic" |> El.italic |> El.fg (Rgb(100uy, 255uy, 200uy))
        El.text "  "
        El.text "Underline" |> El.underline |> El.fg (Rgb(200uy, 150uy, 255uy))
        El.text "  "
        El.text "Strike" |> El.strikethrough |> El.fg (Rgb(255uy, 150uy, 150uy))
        El.text "  "
        El.text "Dim" |> El.dim
      ]
    ]
  | 2 -> // HTML Bridge — browser window
    let chrome = Rgb(80uy, 200uy, 120uy)
    let tag = Rgb(100uy, 180uy, 255uy)
    let content = Rgb(220uy, 220uy, 240uy)
    El.column [
      El.text ""
      El.text "   ┌──────────────────────────────────────┐" |> El.fg chrome
      El.text "   │ ● ● ●   ┃ index.html               │" |> El.fg chrome
      El.text "   ├──────────────────────────────────────┤" |> El.fg chrome
      El.text "   │                                      │" |> El.fg chrome
      El.text "   │   <h1>Hello, Terminal!</h1>          │" |> El.fg tag
      El.text "   │   <p>Rendered in your shell.</p>     │" |> El.fg content
      El.text "   │   <a href=\"#\">Click me</a>           │" |> El.fg (Color.Named(Blue, Bright))
      El.text "   │   <strong>Bold text</strong>         │" |> El.fg content
      El.text "   │                                      │" |> El.fg chrome
      El.text "   └──────────────────────────────────────┘" |> El.fg chrome
      El.text ""
      El.text "  🌐 Render real HTML/CSS in the terminal" |> El.fg chrome |> El.bold
      El.text ""
      El.row [
        El.text "  "
        El.text "<h1>" |> El.fg tag
        El.text "Welcome" |> El.bold |> El.fg content
        El.text "</h1>" |> El.fg tag
        El.text "  "
        El.text "<a>" |> El.fg tag
        El.text "links" |> El.underline |> El.fg (Color.Named(Blue, Bright))
        El.text "</a>" |> El.fg tag
        El.text "  "
        El.text "<em>" |> El.fg tag
        El.text "italic" |> El.italic |> El.fg content
        El.text "</em>" |> El.fg tag
        El.text "  "
        El.text "<strong>" |> El.fg tag
        El.text "bold" |> El.bold |> El.fg content
        El.text "</strong>" |> El.fg tag
      ]
      El.text ""
      El.text "  CSS color properties · Semantic tags · Tables · Forms" |> El.fg dim
      El.text ""
      El.row [ El.text "  "; gradientBar barW (100uy, 255uy, 150uy) (50uy, 120uy, 80uy) ]
    ]
  | 3 -> // Architecture — TEA cycle diagram
    let wire = Rgb(80uy, 80uy, 120uy)
    let node = Rgb(255uy, 230uy, 130uy)
    El.column [
      El.text ""
      El.text "       ┌─────────────────────────────────────────────────┐" |> El.fg wire
      El.text "       │                                                 │" |> El.fg wire
      El.text "       ▼                                                 │" |> El.fg wire
      El.text "   ┌────────┐       ┌────────┐       ┌──────────┐       │" |> El.fg wire
      El.row [
        El.text "   │ " |> El.fg wire
        El.text "MODEL " |> El.bold |> El.fg (Color.Named(Green, Bright))
        El.text " │ ════▶ │ " |> El.fg wire
        El.text " VIEW " |> El.bold |> El.fg (Color.Named(Cyan, Bright))
        El.text " │ ════▶ │ " |> El.fg wire
        El.text " UPDATE " |> El.bold |> El.fg (Color.Named(Yellow, Bright))
        El.text " │       │" |> El.fg wire
      ]
      El.text "   └────────┘       └────────┘       └─────┬────┘       │" |> El.fg wire
      El.text "                                            │            │" |> El.fg wire
      El.text "                                            └────────────┘" |> El.fg wire
      El.text ""
      El.text "  ⚡ The Elm Architecture (TEA)" |> El.fg node |> El.bold
      El.text ""
      El.row [
        El.text "  "
        El.text "Model " |> El.bold |> El.fg (Color.Named(Green, Bright))
        El.text "Immutable state record" |> El.fg dim
        El.text "    "
        El.text "View " |> El.bold |> El.fg (Color.Named(Cyan, Bright))
        El.text "Pure function: Model → UI" |> El.fg dim
      ]
      El.row [
        El.text "  "
        El.text "Update" |> El.bold |> El.fg (Color.Named(Yellow, Bright))
        El.text " Msg → Model → Model × Cmd" |> El.fg dim
      ]
      El.text ""
      El.text "  Pure functions · Immutable state · Time-travel debugging" |> El.fg dim
      El.text ""
      El.row [ El.text "  "; gradientBar barW (255uy, 200uy, 50uy) (120uy, 80uy, 20uy) ]
    ]
  | 4 -> // Layout Engine — grid art
    let grid = Rgb(200uy, 130uy, 255uy)
    El.column [
      El.text ""
      El.text "   ┌─────────────┬──────────────────┐" |> El.fg (Rgb(100uy, 200uy, 255uy))
      El.text "   │░░░░░░░░░░░░░│▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│" |> El.fg (Rgb(100uy, 200uy, 255uy))
      El.text "   │░░░░ A ░░░░░│▓▓▓▓▓▓ B ▓▓▓▓▓▓▓▓│" |> El.fg (Rgb(100uy, 200uy, 255uy))
      El.text "   │░░░░░░░░░░░░░│▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│" |> El.fg (Rgb(100uy, 200uy, 255uy))
      El.text "   │░░░░░░░░░░░░░├────────┬─────────┤" |> El.fg (Rgb(255uy, 200uy, 100uy))
      El.text "   ├─────────────┤▒▒▒▒▒▒▒▒│█████████│" |> El.fg (Rgb(255uy, 200uy, 100uy))
      El.text "   │▓▓▓▓▓▓▓▓▓▓▓▓▓│▒▒ C ▒▒▒│███ D ███│" |> El.fg (Rgb(100uy, 255uy, 180uy))
      El.text "   │▓▓▓ E ▓▓▓▓▓▓│▒▒▒▒▒▒▒▒│█████████│" |> El.fg (Rgb(100uy, 255uy, 180uy))
      El.text "   │▓▓▓▓▓▓▓▓▓▓▓▓▓├────────┴─────────┤" |> El.fg (Rgb(255uy, 130uy, 180uy))
      El.text "   │▓▓▓▓▓▓▓▓▓▓▓▓▓│░░░░░░░░░░░░░░░░░░│" |> El.fg (Rgb(255uy, 130uy, 180uy))
      El.text "   │▓▓▓▓▓▓▓▓▓▓▓▓▓│░░░░░ F ░░░░░░░░░│" |> El.fg (Rgb(255uy, 130uy, 180uy))
      El.text "   └─────────────┴──────────────────┘" |> El.fg grid
      El.text ""
      El.text "  🧩 CSS-like flexbox layout engine" |> El.fg grid |> El.bold
      El.text ""
      El.text "  Constraints: Fixed · Min · Max · Fill · Ratio · Percentage" |> El.fg dim
      El.text "  Borders:     Light · Heavy · Double · Rounded · ASCII" |> El.fg dim
      El.text "  Padding:     Uniform · Horizontal/Vertical · Per-side" |> El.fg dim
      El.text ""
      El.row [ El.text "  "; gradientBar barW (200uy, 130uy, 255uy) (80uy, 50uy, 150uy) ]
      El.row [ El.text "  "; gradientBar barW (80uy, 50uy, 150uy) (200uy, 130uy, 255uy) ]
    ]
  | _ -> // Animations — starburst
    let star = Rgb(255uy, 200uy, 80uy)
    let glow = Rgb(255uy, 150uy, 80uy)
    El.column [
      El.text ""
      El.text "                  ·" |> El.fg star
      El.text "              ·  ╱│╲  ·" |> El.fg star
      El.text "           ·   ╱ │ ╲   ·" |> El.fg star
      El.text "         ──── ╱  │  ╲ ────" |> El.fg glow
      El.text "         ─── ╱   │   ╲ ───" |> El.fg glow
      El.text "        ────╱────┼────╲────" |> El.fg star
      El.text "         ─── ╲   │   ╱ ───" |> El.fg glow
      El.text "         ──── ╲  │  ╱ ────" |> El.fg glow
      El.text "           ·   ╲ │ ╱   ·" |> El.fg star
      El.text "              ·  ╲│╱  ·" |> El.fg star
      El.text "                  ·" |> El.fg star
      El.text ""
      El.text "  ✨ Keyed element transitions" |> El.fg glow |> El.bold
      El.text ""
      El.row [
        El.text "  "
        Spinner.dots elapsed |> El.fg (Rgb(255uy, 200uy, 100uy))
        El.text " Fade        — alpha blend in/out"
      ]
      El.row [
        El.text "  "
        Spinner.dots (elapsed + 150L) |> El.fg (Rgb(200uy, 100uy, 255uy))
        El.text " ColorMorph  — OKLCH interpolation"
      ]
      El.row [
        El.text "  "
        Spinner.dots (elapsed + 300L) |> El.fg (Rgb(100uy, 255uy, 200uy))
        El.text " Wipe        — directional reveal"
      ]
      El.row [
        El.text "  "
        Spinner.dots (elapsed + 450L) |> El.fg (Rgb(255uy, 150uy, 150uy))
        El.text " Dissolve    — random pixel scatter"
      ]
      El.row [
        El.text "  "
        Spinner.dots (elapsed + 600L) |> El.fg (Rgb(100uy, 200uy, 255uy))
        El.text " SlideIn     — directional entry"
      ]
      El.row [
        El.text "  "
        Spinner.dots (elapsed + 750L) |> El.fg (Rgb(200uy, 255uy, 100uy))
        El.text " Grow        — scale from center"
      ]
      El.row [
        El.text "  "
        Spinner.line elapsed |> El.fg (Rgb(255uy, 255uy, 100uy))
        El.text " Sequence    — chain animations"
      ]
      El.text ""
      El.row [ El.text "  "; gradientBar barW (255uy, 180uy, 50uy) (255uy, 80uy, 30uy) ]
      El.row [ El.text "  "; gradientBar barW (255uy, 80uy, 30uy) (255uy, 180uy, 50uy) ]
    ]

let init () =
  { Counter = 0; AnimKind = FadeAnim; AutoCycle = false; Elapsed = 0L; Width = 80 }, Cmd.none

let update msg model =
  match msg with
  | NextContent ->
    { model with Counter = (model.Counter + 1) % contentCards.Length }, Cmd.none
  | PrevContent ->
    { model with Counter = (model.Counter - 1 + contentCards.Length) % contentCards.Length }, Cmd.none
  | CycleAnim ->
    let idx = allAnims |> Array.findIndex ((=) model.AnimKind)
    let next = allAnims.[(idx + 1) % allAnims.Length]
    { model with AnimKind = next }, Cmd.none
  | ToggleAuto ->
    { model with AutoCycle = not model.AutoCycle }, Cmd.none
  | AutoTick ->
    match model.AutoCycle with
    | true -> { model with Counter = (model.Counter + 1) % contentCards.Length }, Cmd.none
    | false -> model, Cmd.none
  | AnimTick ->
    { model with Elapsed = model.Elapsed + 200L }, Cmd.none
  | Resized (w, _h) ->
    { model with Width = w }, Cmd.none
  | Quit -> model, Cmd.quit

let renderContentCard (idx: int) (animKind: AnimKind) (elapsed: int64) (w: int) =
  let (title, color) = contentCards.[idx]
  let transition = animTransition animKind
  let card =
    El.column [
      El.text ""
      El.text (sprintf "  %s" title)
        |> El.bold
        |> El.fg color
      cardContent idx elapsed w
    ]
    |> El.padHV 1 0
    |> El.bordered Rounded
  card
  |> El.keyed (sprintf "card-%d" idx)
  |> El.transition transition

let view model =
  let header =
    El.row [
      El.text " ✨ SageTUI Transitions"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.fill (El.text "")
    ]
    |> El.bg (Color.Named(Black, Normal))

  let animSelector =
    El.row [
      El.text "  Animation: " |> El.dim
      El.text (sprintf "[ %s ]" (animName model.AnimKind))
        |> El.bold
        |> El.fg (Color.Named(Yellow, Bright))
      El.text "  "
      El.text (sprintf "%d/%d" (model.Counter + 1) contentCards.Length)
        |> El.fg (Color.Named(Cyan, Normal))
      El.text "  "
      match model.AutoCycle with
      | true ->
        El.text "AUTO ●"
          |> El.fg (Color.Named(Green, Bright))
          |> El.bold
      | false ->
        El.text "AUTO ○" |> El.dim
    ]

  let indicators =
    El.row [
      El.text "  "
      yield!
        [0 .. contentCards.Length - 1]
        |> List.map (fun i ->
          match i = model.Counter with
          | true -> El.text " ● " |> El.fg (Color.Named(Cyan, Bright))
          | false -> El.text " ○ " |> El.dim)
    ]

  let card = renderContentCard model.Counter model.AnimKind model.Elapsed model.Width

  let footer =
    El.row [
      El.text " [←→] Slide  [a] Animation  [space] Auto  [q] Quit "
        |> El.dim
    ]

  El.column [
    header
    El.text ""
    animSelector
    El.text ""
    El.fill (
      El.column [
        card |> El.fill
        El.text ""
        indicators
      ])
    footer
  ]

let subscribe model =
  [ KeySub (fun (key, _mods) ->
      match key with
      | Key.Right | Char 'l' -> Some NextContent
      | Key.Left | Char 'h' -> Some PrevContent
      | Char 'a' -> Some CycleAnim
      | Char ' ' -> Some ToggleAuto
      | Char 'q' | Char 'Q' | Escape -> Some Quit
      | _ -> None)
    ResizeSub (fun (w, h) -> Resized(w, h))
    TimerSub("anim-tick", TimeSpan.FromMilliseconds(200.0), fun () -> AnimTick)
    match model.AutoCycle with
    | true -> TimerSub("auto", TimeSpan.FromSeconds(2.0), fun () -> AutoTick)
    | false -> () ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _argv =
  let profile =
    Detect.fromEnvironment
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
      (fun () -> System.Console.WindowWidth, System.Console.WindowHeight)
    |> Detect.adjustForMultiplexer
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
    |> UserOverride.apply
      (fun k -> System.Environment.GetEnvironmentVariable(k) |> Option.ofObj)
  let backend = Backend.create profile
  App.run backend program
  0
