module InteractiveForm

// Full interactive form with text inputs, dropdown, focus ring, validation.
// Demonstrates: TextInput, FocusRing, Select, keyboard navigation, styled feedback.

open SageTUI

type Field = Name | Email | Role | Submit

type Model =
  { Name: TextInputModel
    Email: TextInputModel
    Role: SelectModel<string>
    Focus: FocusRing<Field>
    Submitted: bool
    Errors: Map<Field, string> }

type Msg =
  | KeyInput of Key * Modifiers
  | Quit

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
    Errors = Map.empty },
  Cmd.none

let update msg model =
  match msg with
  | Quit -> model, Cmd.quit
  | KeyInput (key, _mods) ->
    match key with
    | Escape -> model, Cmd.quit
    | Tab ->
      { model with Focus = FocusRing.next model.Focus; Submitted = false }, Cmd.none
    | Key.Enter ->
      match FocusRing.current model.Focus with
      | Some Submit ->
        let errors = validate model
        match Map.isEmpty errors with
        | true -> { model with Submitted = true; Errors = Map.empty }, Cmd.none
        | false -> { model with Errors = errors; Submitted = false }, Cmd.none
      | Some Role ->
        { model with Role = Select.toggle model.Role }, Cmd.none
      | _ -> model, Cmd.none
    | Key.Up ->
      match FocusRing.current model.Focus with
      | Some Role when model.Role.IsOpen ->
        { model with Role = Select.moveUp model.Role }, Cmd.none
      | _ -> model, Cmd.none
    | Key.Down ->
      match FocusRing.current model.Focus with
      | Some Role when model.Role.IsOpen ->
        { model with Role = Select.moveDown model.Role }, Cmd.none
      | _ -> model, Cmd.none
    | _ ->
      match FocusRing.current model.Focus with
      | Some Name ->
        { model with Name = TextInput.handleKey key model.Name }, Cmd.none
      | Some Email ->
        { model with Email = TextInput.handleKey key model.Email }, Cmd.none
      | Some Role when model.Role.IsOpen ->
        match key with
        | Escape -> { model with Role = Select.confirm model.Role }, Cmd.none
        | _ -> model, Cmd.none
      | _ -> model, Cmd.none

let fieldLabel (label: string) (focused: bool) (error: string option) =
  El.column [
    El.row [
      match focused with
      | true ->
        El.text (sprintf "▸ %s" label)
          |> El.fg (Color.Named(Cyan, Bright))
          |> El.bold
      | false ->
        El.text (sprintf "  %s" label)
          |> El.fg (Color.Named(White, Normal))
    ]
    match error with
    | Some e ->
      El.text (sprintf "    ⚠ %s" e)
        |> El.fg (Color.Named(Red, Bright))
    | None -> El.empty
  ]

let view model =
  let focusedField = FocusRing.current model.Focus
  let isFocused f = focusedField = Some f
  let errorFor f = Map.tryFind f model.Errors

  let title =
    El.row [
      El.text " ✦ Registration Form"
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.fill (El.text "")
      El.text "[Tab] Next Field  [Enter] Submit  [Esc] Quit "
        |> El.dim
    ]
    |> El.bg (Color.Named(Black, Normal))

  let nameField =
    El.column [
      fieldLabel "Name" (isFocused Name) (errorFor Name)
      El.row [
        El.text "    "
        TextInput.view (isFocused Name) model.Name
          |> El.width 30
          |> El.bordered (match isFocused Name with true -> Rounded | false -> Light)
      ]
    ]

  let emailField =
    El.column [
      fieldLabel "Email" (isFocused Email) (errorFor Email)
      El.row [
        El.text "    "
        TextInput.view (isFocused Email) model.Email
          |> El.width 30
          |> El.bordered (match isFocused Email with true -> Rounded | false -> Light)
      ]
    ]

  let roleField =
    El.column [
      fieldLabel "Role" (isFocused Role) None
      El.row [
        El.text "    "
        Select.view id (isFocused Role) model.Role
          |> El.width 30
          |> El.bordered (match isFocused Role with true -> Rounded | false -> Light)
      ]
    ]

  let submitBtn =
    let style =
      match isFocused Submit with
      | true ->
        El.text "  ✓ Submit  "
          |> El.bold
          |> El.fg (Color.Named(Black, Normal))
          |> El.bg (Color.Named(Green, Bright))
      | false ->
        El.text "  ✓ Submit  "
          |> El.dim
    El.row [ El.text "    "; style ]

  let result =
    match model.Submitted with
    | true ->
      let role = Select.selectedValue model.Role |> Option.defaultValue "?"
      El.column [
        El.text ""
        El.text (sprintf "  ✅ Welcome, %s! (%s)" model.Name.Text role)
          |> El.fg (Color.Named(Green, Bright))
          |> El.bold
        El.text (sprintf "     Confirmation sent to %s" model.Email.Text)
          |> El.fg (Color.Named(Green, Normal))
      ]
    | false -> El.empty

  El.column [
    title
    El.text ""
    nameField
    El.text ""
    emailField
    El.text ""
    roleField
    El.text ""
    submitBtn
    result
  ]
  |> El.padHV 2 0

let subscribe _model =
  [ KeySub (fun (key, mods) -> Some (KeyInput(key, mods))) ]

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
