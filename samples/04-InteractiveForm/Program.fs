module InteractiveForm

// Full interactive form with text inputs, dropdown, focus ring, validation.
// Demonstrates: TextInput, FocusRing, Select, keyboard navigation, styled feedback.

open System
open SageTUI

type Field = Name | Email | Role | Submit

type Model =
  { Name: TextInputModel
    Email: TextInputModel
    Role: SelectModel<string>
    Focus: FocusRing<Field>
    Submitted: bool
    Errors: Map<Field, string>
    DemoStep: int }

type Msg =
  | KeyInput of Key * Modifiers
  | Quit
  | NextDemoStep

// ─── Demo Mode ───────────────────────────────────────────────────────────────

let isDemoMode = Environment.GetEnvironmentVariable("SAGETUI_DEMO_MODE") = "1"

let private key k = Some (KeyInput(k, Modifiers.None))
let private shiftKey k = Some (KeyInput(k, Modifiers.Shift))
let private ch c = key (Key.Char c)
let private tab () = key Key.Tab
let private shiftTab () = shiftKey Key.Tab
let private enter () = key Key.Enter
let private bs () = key Key.Backspace
let private typeStr (delayMs: int) (s: string) = s |> Seq.toList |> List.map (fun c -> delayMs, ch c)
let private bsMany n = List.replicate n (90, bs())

// Sequence: type name → type bad email → submit → see error → fix email → select role → submit → success
let demoSteps : (int * Msg option) list = [
  yield  900, None                            // form loads, Name focused
  yield! typeStr 80 "Ada Lovelace"            // type name
  yield  450, tab()                           // Tab → Email
  yield  300, None
  yield! typeStr 75 "notvalid"                // type bad email (no @)
  yield  450, tab()                           // Tab → Role
  yield  350, tab()                           // Tab → Submit
  yield  600, None                            // hover on Submit button
  yield  350, enter()                         // SUBMIT → validation error!
  yield  1100, None                           // pause — let viewer read the error
  yield  300, shiftTab()                      // Shift+Tab → back to Email
  yield  250, None
  yield! bsMany 8                             // clear "notvalid"
  yield  200, None
  yield! typeStr 70 "ada@example.com"         // correct email
  yield  450, tab()                           // Tab → Role
  yield  400, enter()                         // open dropdown
  yield  500, None                            // dropdown opens
  yield  280, key Key.Down                    // → "Designer"
  yield  350, enter()                         // confirm selection
  yield  450, tab()                           // Tab → Submit
  yield  700, None                            // pause on Submit — anticipation
  yield  350, enter()                         // SUBMIT → READY ✅
  yield 1800, None                            // hold on success state
]

// ─── TEA ─────────────────────────────────────────────────────────────────────

let theme = Theme.catppuccin

let private chip fg bg text =
  El.text (sprintf " %s " text)
  |> El.bold
  |> El.fg fg
  |> El.bg bg

let validate model =
  let errors = Map.empty
  let errors =
    match model.Name.Text.Length with
    | 0 -> Map.add Name "Name is required" errors
    | n when n < 2 -> Map.add Name "Name must be at least 2 characters" errors
    | _ -> errors
  let errors =
    match model.Email.Text.Contains("@") with
    | true -> errors
    | false -> Map.add Email "Invalid email address" errors
  errors

let init () =
  { Name = TextInput.empty
    Email = TextInput.empty
    Role = Select.create ["Developer"; "Designer"; "Manager"; "DevOps"; "QA"]
    Focus = FocusRing.create [Name; Email; Role; Submit]
    Submitted = false
    Errors = Map.empty
    DemoStep = 0 },
  match isDemoMode with true -> Cmd.delay 400 NextDemoStep | false -> Cmd.none

let rec update msg model =
  match msg with
  | Quit -> model, Cmd.quit
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
  | KeyInput (key, _mods) ->
    let focus = FocusRing.current model.Focus
    match focus, key with
    // Dropdown-open takes priority — Escape closes dropdown, not the app
    | Some Role, Escape when model.Role.IsOpen ->
      { model with Role = Select.confirm model.Role }, Cmd.none
    | Some Role, Key.Up when model.Role.IsOpen ->
      { model with Role = Select.moveUp model.Role }, Cmd.none
    | Some Role, Key.Down when model.Role.IsOpen ->
      { model with Role = Select.moveDown model.Role }, Cmd.none
    | Some Role, Key.Enter ->
      { model with Role = Select.toggle model.Role }, Cmd.none
    // Global keys
    | _, Escape -> model, Cmd.quit
    | _, Key.Tab when _mods.HasFlag(Modifiers.Shift) ->
      { model with Focus = FocusRing.prev model.Focus; Submitted = false }, Cmd.none
    | _, Tab ->
      { model with Focus = FocusRing.next model.Focus; Submitted = false }, Cmd.none
    // Submit
    | Some Submit, Key.Enter ->
      let errors = validate model
      match Map.isEmpty errors with
      | true -> { model with Submitted = true; Errors = Map.empty }, Cmd.none
      | false -> { model with Errors = errors; Submitted = false }, Cmd.none
    // Text fields
    | Some Name, _ ->
      { model with Name = TextInput.handleKey key model.Name }, Cmd.none
    | Some Email, _ ->
      { model with Email = TextInput.handleKey key model.Email }, Cmd.none
    | _ -> model, Cmd.none

let fieldLabel (label: string) (focused: bool) (error: string option) =
  let labelEl =
    El.text label
    |> El.width 8
    |> El.bold
    |> El.fg (match focused with true -> theme.Primary | false -> theme.TextDim)

  let detailEl =
    match error with
    | Some e ->
      Theme.error theme (sprintf "⚠ %s" e)
    | None ->
      El.text ""

  El.row [
    labelEl
    detailEl
    El.fill El.empty
    match focused with
    | true -> chip (Color.Named(Black, Normal)) theme.Primary "ACTIVE"
    | false -> El.empty
  ]

let fieldHint (hint: string) (error: string option) =
  match error with
  | Some _ -> El.empty
  | None ->
    El.text hint
    |> El.fg theme.TextDim
    |> El.dim

let fieldShell (focused: bool) (content: Element) =
  El.row [
    El.text "[" |> El.fg (match focused with true -> theme.Primary | false -> theme.TextDim)
    content |> El.width 34
    El.text "]" |> El.fg (match focused with true -> theme.Primary | false -> theme.TextDim)
  ]

let textField label hint focused error content =
  El.column [
    fieldLabel label focused error
    fieldShell focused content
    fieldHint hint error
  ]

let textInputDisplay placeholder focused (model: TextInputModel) =
  let value =
    match model.Text, focused with
    | "", true -> "▌"
    | "", false -> placeholder
    | text, true -> text + "▌"
    | text, false -> text

  let styled =
    El.text value
    |> El.fg (match focused with true -> theme.TextFg | false -> theme.TextFg)

  match model.Text.Length = 0 with
  | true -> styled |> El.dim
  | false -> styled

let selectDisplay focused (model: SelectModel<string>) =
  let selected =
    model.Options
    |> List.tryItem model.Selected
    |> Option.defaultValue "Developer"

  let suffix = match model.IsOpen with true -> "[open]" | false -> "[enter]"
  El.text (sprintf "%s %s" selected suffix)
  |> El.fg (match focused with true -> theme.TextFg | false -> theme.TextFg)

let view model =
  let focusedField = FocusRing.current model.Focus
  let isFocused f = focusedField = Some f
  let errorFor f = Map.tryFind f model.Errors

  let title =
    El.row [
      chip (Color.Named(Black, Normal)) theme.Accent "SHOWCASE"
      Theme.heading theme "Interactive Form"
      El.fill (El.text "")
      El.text "Keyboard-first • focus • validation • select"
        |> El.fg theme.TextDim
        |> El.dim
    ]
    |> El.padHV 1 0

  let nameField =
    textField "Name" "Shown in alerts and ownership views." (isFocused Name) (errorFor Name)
      (textInputDisplay "Ada Lovelace" (isFocused Name) model.Name)

  let emailField =
    textField "Email" "Used for confirmations and deploy notices." (isFocused Email) (errorFor Email)
      (textInputDisplay "ada@example.com" (isFocused Email) model.Email)

  let roleField =
    El.column [
      fieldLabel "Role" (isFocused Role) None
      fieldShell (isFocused Role) (selectDisplay (isFocused Role) model.Role)
      fieldHint "Press Enter to open the role list." None
      match model.Role.IsOpen with
      | true ->
        model.Role.Options
        |> List.mapi (fun i option ->
          let prefix = match i = model.Role.Selected with true -> "›" | false -> " "
          El.text (sprintf "  %s %s" prefix option)
          |> El.fg (match i = model.Role.Selected with true -> theme.Secondary | false -> theme.TextDim))
        |> El.column
      | false -> El.empty
    ]

  let submitBtn =
    let button =
      match isFocused Submit with
      | true ->
        El.text " Submit request "
          |> El.bold
          |> El.fg (Color.Named(Black, Normal))
          |> El.bg theme.Success
      | false ->
        El.text " Submit request "
          |> El.bold
          |> El.fg theme.Primary
          |> El.bordered Rounded
    El.row [
      button
      El.text "  "
      El.text "Enter submits the request."
        |> El.fg theme.TextDim
        |> El.dim
    ]

  let result =
    match model.Submitted with
    | true ->
      let role = Select.selectedValue model.Role |> Option.defaultValue "?"
      El.column [
        El.row [
          chip (Color.Named(Black, Normal)) theme.Success "READY"
          El.text (sprintf " %s is configured as %s" model.Name.Text role)
            |> El.fg theme.Success
            |> El.bold
        ]
        El.text (sprintf "Confirmation and audit notices will go to %s." model.Email.Text)
          |> El.fg theme.TextFg
      ]
      |> El.padAll 1
      |> El.bordered Rounded
    | false -> El.empty

  let intro =
    El.column [
      Theme.heading theme "Create an operator profile"
      Theme.subheading theme "Move with Tab, edit inline, choose a role, and submit without leaving the keyboard."
    ]

  let formCard =
    El.column [
      intro
      nameField
      emailField
      roleField
      submitBtn
      result
    ]
    |> El.padHV 1 0
    |> El.bordered Rounded
    |> El.width 54

  let footer =
    El.row [
      El.text "Tab" |> El.bold |> El.fg theme.Primary
      El.text " next  " |> El.fg theme.TextDim |> El.dim
      El.text "Shift+Tab" |> El.bold |> El.fg theme.Primary
      El.text " previous  " |> El.fg theme.TextDim |> El.dim
      El.text "Enter" |> El.bold |> El.fg theme.Primary
      El.text " submit/select  " |> El.fg theme.TextDim |> El.dim
      El.text "Esc" |> El.bold |> El.fg theme.Primary
      El.text " quit" |> El.fg theme.TextDim |> El.dim
    ]
    |> El.padHV 1 0

  El.column [
    title
    formCard |> El.center
    footer
  ]
  |> Theme.apply theme

let subscribe _model =
  [ KeySub (fun (key, mods) -> Some (KeyInput(key, mods))) ]

let program : Program<Model, Msg> =
  { Init = init
    Update = update
    View = view
    Subscribe = subscribe }

[<EntryPoint>]
let main _ = App.run program; 0

