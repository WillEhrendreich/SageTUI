module Kanban

// Kanban board with columns, cards, keyboard navigation.
// Demonstrates: Complex layouts, fill/ratio, borders, overlays, keyboard-driven UX.

open System
open SageTUI

type Priority = Low | Medium | High | Critical

type Card =
  { Id: int
    Title: string
    Priority: Priority
    Assignee: string }

type Stage = Todo | InProgress | Review | Done

type Model =
  { Cards: Map<Stage, Card list>
    FocusCol: Stage
    FocusRow: int
    Moving: bool
    DemoStep: int }

type Msg =
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | GrabOrDrop
  | Quit
  | NextDemoStep

// ─── Demo Mode ───────────────────────────────────────────────────────────────

let isDemoMode = Environment.GetEnvironmentVariable("SAGETUI_DEMO_MODE") = "1"

// Sequence: navigate to InProgress, browse cards, grab Critical card, fly it to Done.
// Then navigate back, grab a Todo card, push it to InProgress.
let demoSteps : (int * Msg option) list = [
  700,  None               // board renders — pause
  350,  Some MoveRight     // → InProgress column
  400,  None               // pause — InProgress focused
  300,  Some MoveDown      // → row 1 (SIMD diff engine)
  300,  Some MoveUp        // → row 0 (Arena allocator - CRITICAL)
  500,  None               // pause — hovering Critical card
  300,  Some GrabOrDrop    // GRAB — MOVING badge appears
  700,  None               // pause — let viewer see MOVING state
  400,  Some MoveRight     // → card moves to Review
  600,  None               // pause — card landed in Review
  300,  Some GrabOrDrop    // GRAB again from Review
  500,  None               // pause
  400,  Some MoveRight     // → card moves to Done!
  900,  None               // dramatic pause — DONE ✅
  350,  Some MoveLeft      // ← navigate back (card is in Done, we navigate)
  350,  Some MoveLeft      // ← InProgress
  350,  Some MoveLeft      // ← Todo
  400,  None               // pause on Todo
  300,  Some MoveDown      // → row 1 (Write MDN tests)
  300,  Some GrabOrDrop    // GRAB
  500,  None               // pause — MOVING
  400,  Some MoveRight     // → moves to InProgress
  700,  None               // pause — card is now in InProgress
  350,  Some MoveLeft      // ← back to Todo
  350,  Some MoveUp        // → row 0
  600,  None               // pause — back to start, loop
]

// ─── Data ────────────────────────────────────────────────────────────────────

let allColumns = [Todo; InProgress; Review; Done]

let columnName col =
  match col with
  | Todo -> "📋 Todo"
  | InProgress -> "🔨 In Progress"
  | Review -> "🔍 Review"
  | Done -> "✅ Done"

let priorityColor p =
  match p with
  | Low -> Color.Named(Blue, Normal)
  | Medium -> Color.Named(Yellow, Normal)
  | High -> Color.Named(Red, Normal)
  | Critical -> Color.Named(Red, Bright)

let priorityLabel p =
  match p with
  | Low -> "LOW"
  | Medium -> "MED"
  | High -> "HI"
  | Critical -> "CRIT"

let init () =
  { Cards =
      Map.ofList [
        Todo, [
          { Id = 1; Title = "Design CSS parser"; Priority = High; Assignee = "Alice" }
          { Id = 2; Title = "Write MDN tests"; Priority = Medium; Assignee = "Bob" }
          { Id = 3; Title = "Add color themes"; Priority = Low; Assignee = "Carol" }
        ]
        InProgress, [
          { Id = 4; Title = "Arena allocator"; Priority = Critical; Assignee = "Alice" }
          { Id = 5; Title = "SIMD diff engine"; Priority = High; Assignee = "Dave" }
        ]
        Review, [
          { Id = 6; Title = "HTML bridge"; Priority = Medium; Assignee = "Bob" }
        ]
        Done, [
          { Id = 7; Title = "TEA architecture"; Priority = High; Assignee = "Alice" }
          { Id = 8; Title = "Input handling"; Priority = Medium; Assignee = "Carol" }
          { Id = 9; Title = "Border rendering"; Priority = Low; Assignee = "Dave" }
        ]
      ]
    FocusCol = Todo
    FocusRow = 0
    Moving = false
    DemoStep = 0 },
  match isDemoMode with true -> Cmd.delay 500 NextDemoStep | false -> Cmd.none

let colIdx col =
  match col with Todo -> 0 | InProgress -> 1 | Review -> 2 | Done -> 3

let colAt idx =
  match idx with 0 -> Todo | 1 -> InProgress | 2 -> Review | _ -> Done

let cardsIn col model = Map.tryFind col model.Cards |> Option.defaultValue []

let clampRow model =
  let cards = cardsIn model.FocusCol model
  { model with FocusRow = min model.FocusRow (max 0 (cards.Length - 1)) }

let moveCardToColumn (deltaIdx: int) model =
  let srcCol = model.FocusCol
  let srcCards = cardsIn srcCol model
  match model.FocusRow < srcCards.Length with
  | true ->
    let card = srcCards.[model.FocusRow]
    let dstIdx = (colIdx srcCol + deltaIdx) |> max 0 |> min 3
    let dstCol = colAt dstIdx
    let newSrc = srcCards |> List.filter (fun c -> c.Id <> card.Id)
    let newDst = (cardsIn dstCol model) @ [card]
    { model with
        Cards = model.Cards |> Map.add srcCol newSrc |> Map.add dstCol newDst
        FocusCol = dstCol
        FocusRow = newDst.Length - 1
        Moving = false }, Cmd.none
  | false -> model, Cmd.none

let rec update msg model =
  match msg with
  | Quit -> model, Cmd.quit
  | MoveRight ->
    match model.Moving with
    | false ->
      let nextIdx = min 3 (colIdx model.FocusCol + 1)
      { model with FocusCol = colAt nextIdx } |> clampRow, Cmd.none
    | true -> moveCardToColumn 1 model
  | MoveLeft ->
    match model.Moving with
    | false ->
      let prevIdx = max 0 (colIdx model.FocusCol - 1)
      { model with FocusCol = colAt prevIdx } |> clampRow, Cmd.none
    | true -> moveCardToColumn -1 model
  | MoveUp ->
    { model with FocusRow = max 0 (model.FocusRow - 1) }, Cmd.none
  | MoveDown ->
    let maxRow = (cardsIn model.FocusCol model).Length - 1
    { model with FocusRow = min maxRow (model.FocusRow + 1) }, Cmd.none
  | GrabOrDrop ->
    { model with Moving = not model.Moving }, Cmd.none
  | NextDemoStep when isDemoMode ->
    let step = model.DemoStep % demoSteps.Length
    let (delayMs, actionOpt) = demoSteps[step]
    let model' = { model with DemoStep = step + 1 }
    let model'', actionCmd =
      match actionOpt with
      | Some action -> update action model'
      | None -> model', Cmd.none
    model'', Cmd.batch [actionCmd; Cmd.delay delayMs NextDemoStep]
  | NextDemoStep -> model, Cmd.none

let renderCard (card: Card) (focused: bool) (grabbed: bool) =
  let border =
    match focused, grabbed with
    | _, true -> Double
    | true, _ -> Rounded
    | false, _ -> Light
  let titleColor =
    match focused with
    | true -> Color.Named(White, Bright)
    | false -> Color.Named(White, Normal)
  El.column [
    El.row [
      El.text card.Title
        |> El.fg titleColor
        |> (match focused with true -> El.bold | false -> id)
    ]
    El.row [
      El.text (sprintf "[%s]" (priorityLabel card.Priority))
        |> El.fg (priorityColor card.Priority)
        |> El.bold
      El.text (sprintf " @%s" card.Assignee)
        |> El.dim
    ]
  ]
  |> El.padHV 1 0
  |> El.bordered border

let renderColumn (col: Stage) (cards: Card list) (isFocusCol: bool) (focusRow: int) (moving: bool) =
  let headerColor =
    match isFocusCol with
    | true -> Color.Named(Cyan, Bright)
    | false -> Color.Named(Cyan, Normal)
  let header =
    El.text (sprintf " %s (%d)" (columnName col) cards.Length)
      |> El.fg headerColor
      |> El.bold
  let cardViews =
    cards
    |> List.mapi (fun i card ->
      let focused = isFocusCol && i = focusRow
      let grabbed = focused && moving
      renderCard card focused grabbed)
  El.column [
    header
    El.text ""
    yield! cardViews
    match cards.IsEmpty with
    | true ->
      El.text "  (empty)" |> El.dim
    | false -> El.empty
  ]
  |> El.bordered (match isFocusCol with true -> Rounded | false -> Light)

let view model =
  let header =
    El.row [
      El.text " ◆ SageTUI Kanban"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.fill (El.text "")
      match model.Moving with
      | true ->
        El.text " ✋ MOVING "
          |> El.bold
          |> El.fg (Color.Named(Black, Normal))
          |> El.bg (Color.Named(Yellow, Bright))
      | false -> El.empty
      El.text " "
    ]
    |> El.bg (Color.Named(Black, Normal))

  let board =
    El.row [
      yield!
        allColumns
        |> List.map (fun col ->
          let cards = cardsIn col model
          let isFocus = col = model.FocusCol
          renderColumn col cards isFocus model.FocusRow model.Moving
          |> El.fill)
    ]

  let footer =
    El.row [
      El.text " [←→] Column  [↑↓] Card  [Space] Grab/Move  [q] Quit "
        |> El.dim
    ]

  let grabbedOverlay =
    match model.Moving with
    | true ->
      let cards = cardsIn model.FocusCol model
      match model.FocusRow < cards.Length with
      | true ->
        let card = cards.[model.FocusRow]
        El.text $" ✋ Moving: {card.Title} "
          |> El.bold
          |> El.fg (Color.Named(Black, Normal))
          |> El.bg (Color.Named(Yellow, Bright))
          |> El.bordered Double
      | false -> El.empty
    | false -> El.empty

  El.overlay [
    El.column [
      header
      El.fill board
      footer
    ]
    grabbedOverlay
  ]

let keyBindings =
  Keys.bind [
    Key.Left, MoveLeft
    Key.Char (System.Text.Rune 'h'), MoveLeft
    Key.Right, MoveRight
    Key.Char (System.Text.Rune 'l'), MoveRight
    Key.Up, MoveUp
    Key.Char (System.Text.Rune 'k'), MoveUp
    Key.Down, MoveDown
    Key.Char (System.Text.Rune 'j'), MoveDown
    Key.Char (System.Text.Rune ' '), GrabOrDrop
    Key.Enter, GrabOrDrop
    Key.Char (System.Text.Rune 'q'), Quit
    Key.Char (System.Text.Rune 'Q'), Quit
    Key.Escape, Quit
  ]

let subscribe _model = [ keyBindings ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe
    OnError = None }

[<EntryPoint>]
let main _ = App.run program; 0

