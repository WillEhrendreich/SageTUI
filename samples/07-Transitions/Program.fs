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
    Elapsed: int64 }

type Msg =
  | NextContent
  | PrevContent
  | CycleAnim
  | ToggleAuto
  | AutoTick
  | AnimTick
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

// Each card shows visual demos, not just bullet text
let cardContent (idx: int) (elapsed: int64) =
  match idx with
  | 0 -> // Performance — show a live spinner
    El.column [
      El.text "  Zero-GC arena allocator"
      El.text "  SIMD-accelerated diff"
      El.row [
        El.text "  Processing "
        Spinner.dots elapsed |> El.fg (Color.Named(Cyan, Bright))
      ]
    ]
  | 1 -> // Rich Styling — show actual gradients
    El.column [
      El.text "  24-bit TrueColor:" |> El.dim
      El.row [ El.text "  "; Gradient.rainbow 30 "████████████████████████████████" ]
      El.row [ El.text "  "; Gradient.horizontal (255uy, 50uy, 50uy) (50uy, 50uy, 255uy) 30 "████████████████████████████████" ]
      El.row [
        El.text "  "
        El.text "Bold" |> El.bold
        El.text " + "
        El.text "Italic" |> El.italic
        El.text " + "
        El.text "Underline" |> El.underline
      ]
    ]
  | 2 -> // HTML Bridge — show styled HTML-like rendering
    El.column [
      El.text "  Renders real HTML:" |> El.dim
      El.row [
        El.text "  "
        El.text "<h1>" |> El.fg (Color.Named(Cyan, Normal))
        El.text "Hello" |> El.bold |> El.fg (Color.Named(White, Bright))
        El.text "</h1>" |> El.fg (Color.Named(Cyan, Normal))
      ]
      El.row [
        El.text "  "
        El.text "<a>" |> El.fg (Color.Named(Cyan, Normal))
        El.text "link" |> El.underline |> El.fg (Color.Named(Blue, Bright))
        El.text "</a>" |> El.fg (Color.Named(Cyan, Normal))
        El.text " + "
        El.text "<b>" |> El.fg (Color.Named(Cyan, Normal))
        El.text "bold" |> El.bold
        El.text "</b>" |> El.fg (Color.Named(Cyan, Normal))
      ]
    ]
  | 3 -> // Architecture — show TEA cycle
    El.column [
      El.text "  The Elm Architecture:" |> El.dim
      El.row [
        El.text "  Model"
          |> El.bold |> El.fg (Color.Named(Green, Bright))
        El.text " → "
        El.text "View"
          |> El.bold |> El.fg (Color.Named(Cyan, Bright))
        El.text " → "
        El.text "Update"
          |> El.bold |> El.fg (Color.Named(Yellow, Bright))
      ]
      El.text "  Pure functions, immutable state"
    ]
  | 4 -> // Layout Engine — show nested borders
    El.column [
      El.row [
        El.text " A " |> El.bordered Light
        El.text " B " |> El.bordered Rounded
        El.text " C " |> El.bordered Double
      ]
      El.text "  Row/Column composition"
      El.text "  Fill, ratio, constraints"
    ]
  | _ -> // Animations — show spinner
    El.column [
      El.row [
        El.text "  "
        Spinner.line elapsed |> El.fg (Color.Named(Yellow, Bright))
        El.text " Fade, Wipe, Dissolve"
      ]
      El.text "  SlideIn, Grow, Sequence"
      El.text "  View transitions by key"
    ]

let init () =
  { Counter = 0; AnimKind = FadeAnim; AutoCycle = false; Elapsed = 0L }, Cmd.none

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
  | Quit -> model, Cmd.quit

let renderContentCard (idx: int) (animKind: AnimKind) (elapsed: int64) =
  let (title, color) = contentCards.[idx]
  let transition = animTransition animKind
  let card =
    El.column [
      El.text ""
      El.text (sprintf "  %s" title)
        |> El.bold
        |> El.fg color
      El.text ""
      cardContent idx elapsed
      El.text ""
    ]
    |> El.padAll 1
    |> El.bordered Rounded
    |> El.width 40
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

  let card = renderContentCard model.Counter model.AnimKind model.Elapsed

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
        El.text ""
        El.row [
          El.fill (El.text "")
          card
          El.fill (El.text "")
        ]
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
