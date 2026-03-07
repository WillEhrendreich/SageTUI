module Transitions

// Animated transition showcase: Fade, ColorMorph, Wipe, Dissolve, SlideIn.
// Demonstrates: Keyed elements, view transitions, enter/exit animations, timers.

open System
open SageTUI

type AnimKind = FadeAnim | ColorMorphAnim | WipeRight | WipeDown | DissolveAnim | GrowAnim

type Model =
  { Counter: int
    AnimKind: AnimKind
    AutoCycle: bool }

type Msg =
  | NextContent
  | PrevContent
  | CycleAnim
  | ToggleAuto
  | AutoTick
  | Quit

let allAnims = [| FadeAnim; ColorMorphAnim; WipeRight; WipeDown; DissolveAnim; GrowAnim |]

let animName anim =
  match anim with
  | FadeAnim -> "Fade"
  | ColorMorphAnim -> "ColorMorph"
  | WipeRight -> "Wipe →"
  | WipeDown -> "Wipe ↓"
  | DissolveAnim -> "Dissolve"
  | GrowAnim -> "Grow"

let animTransition anim =
  match anim with
  | FadeAnim -> Fade 400<ms>
  | ColorMorphAnim -> ColorMorph 500<ms>
  | WipeRight -> Wipe(Direction.Right, 400<ms>)
  | WipeDown -> Wipe(Direction.Down, 400<ms>)
  | DissolveAnim -> Dissolve 600<ms>
  | GrowAnim -> Grow 400<ms>

let contentCards = [|
  ("🚀 Performance",
   Color.Rgb(50uy, 180uy, 255uy),
   [ "Zero-GC arena allocator"
     "SIMD-accelerated diff"
     "16-byte packed cells"
     "Batched ANSI output" ])

  ("🎨 Rich Styling",
   Color.Rgb(255uy, 100uy, 200uy),
   [ "TrueColor 24-bit RGB"
     "256-color palette"
     "Bold, italic, underline"
     "Background colors" ])

  ("🌐 HTML Bridge",
   Color.Rgb(100uy, 255uy, 150uy),
   [ "Render HTML in terminal"
     "CSS color support"
     "Semantic tag mapping"
     "Table/list/form rendering" ])

  ("⚡ Architecture",
   Color.Rgb(255uy, 200uy, 50uy),
   [ "The Elm Architecture (TEA)"
     "Immutable state management"
     "Subscription-based input"
     "Command side-effects" ])

  ("🧩 Layout Engine",
   Color.Rgb(200uy, 130uy, 255uy),
   [ "Row/Column composition"
     "Fill, ratio, percentage"
     "Constraint solver"
     "Nested borders & padding" ])

  ("✨ Animations",
   Color.Rgb(255uy, 150uy, 80uy),
   [ "Fade, Wipe, Dissolve"
     "ColorMorph interpolation"
     "View transitions by key"
     "Sequence compositions" ])
|]

let init () =
  { Counter = 0; AnimKind = FadeAnim; AutoCycle = false }, Cmd.none

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
  | Quit -> model, Cmd.quit

let renderContentCard (idx: int) (animKind: AnimKind) =
  let (title, color, items) = contentCards.[idx]
  let transition = animTransition animKind
  let card =
    El.column [
      El.text ""
      El.text (sprintf "  %s" title)
        |> El.bold
        |> El.fg color
      El.text ""
      yield!
        items |> List.map (fun item ->
          El.text (sprintf "    • %s" item))
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

  let card = renderContentCard model.Counter model.AnimKind

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
