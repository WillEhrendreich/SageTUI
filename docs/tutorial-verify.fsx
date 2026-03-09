// Tutorial verification script — compiled by CI to catch API drift.
// Run: dotnet fsi docs/tutorial-verify.fsx (after dotnet build)
// This does NOT run a terminal app; it type-checks all tutorial patterns.
#r "../bin/Debug/net10.0/SageTUI.Library.dll"

open SageTUI

// ── Section 3: Hello World patterns ──────────────────────────────────────────

type CounterModel = { Count: int }
type CounterMsg = Increment | Decrement | Quit

let counterView (m: CounterModel) : Element =
    El.column [
        El.text (sprintf "Count: %d" m.Count) |> El.bold
        El.text "↑ / ↓ to change • q to quit" |> El.dim
    ]
    |> El.padAll 1
    |> El.bordered Rounded
    |> El.width 30

// ── Section 4: Layout patterns ────────────────────────────────────────────────

let _layoutExample : Element =
    El.column [
        El.row [
            El.width 20 (El.text "fixed")
            El.fill (El.text "fills remaining")
        ]
        El.bordered Rounded (
            El.column [
                El.text "Content goes here"
            ]
            |> El.padAll 1
        )
    ]

// ── Section 5: Contact Form ───────────────────────────────────────────────────

type Field = Name | Email | Submit

type ContactModel = {
    Name:      TextInputModel
    Email:     TextInputModel
    Focus:     FocusRing<Field>
    Submitted: bool
}

type ContactMsg =
    | KeyPressed of Key * Modifiers
    | Quit

let init () =
    { Name      = TextInput.empty
      Email     = TextInput.empty
      Focus     = FocusRing.create [ Name; Email; Submit ]
      Submitted = false },
    Cmd.none

let update (msg: ContactMsg) (model: ContactModel) =
    match msg with
    | Quit -> model, Cmd.quit

    | KeyPressed(Key.Tab, mods) ->
        let focus =
            match mods.HasFlag Modifiers.Shift with
            | true  -> FocusRing.prev model.Focus
            | false -> FocusRing.next model.Focus
        { model with Focus = focus }, Cmd.none

    | KeyPressed(Key.Enter, _) when FocusRing.isFocused Submit model.Focus ->
        { model with Submitted = true }, Cmd.none

    | KeyPressed(k, _) ->
        match FocusRing.current model.Focus with
        | Some Name  ->
            { model with Name  = TextInput.handleKey k model.Name  }, Cmd.none
        | Some Email ->
            { model with Email = TextInput.handleKey k model.Email }, Cmd.none
        | _ -> model, Cmd.none

let theme = Theme.nord

let view (model: ContactModel) : Element =
    match model.Submitted with
    | true ->
        El.column [
            El.text "✓ Submitted!" |> El.bold |> El.fg theme.Success
            El.text (sprintf "  Name:  %s" model.Name.Text)
            El.text (sprintf "  Email: %s" model.Email.Text)
            El.text ""
            El.text "Press q to quit" |> El.dim
        ]
        |> El.padAll 2
        |> El.bordered Rounded
    | false ->
        El.column [
            Theme.heading theme "Contact Form"
            El.text ""
            El.text "Name"  |> El.dim
            TextInput.viewThemed theme (FocusRing.isFocused Name model.Focus) model.Name
            El.text ""
            El.text "Email" |> El.dim
            TextInput.viewThemed theme (FocusRing.isFocused Email model.Focus) model.Email
            El.text ""
            (if FocusRing.isFocused Submit model.Focus
             then El.text "[ Submit ]" |> El.bold |> El.fg theme.Primary
             else El.text "[ Submit ]" |> El.dim)
            El.text ""
            El.text "Tab / Shift-Tab to navigate • Enter to submit • q to quit"
            |> El.dim
        ]
        |> El.padAll 2
        |> El.bordered Rounded
        |> El.width 50

let quitBinding = Keys.bind [ Key.Char 'q', Quit; Key.Escape, Quit ]

// ── Section 6: Theme record shape ─────────────────────────────────────────────

let _themeCheck : Theme = {
    Primary    = Named(Cyan, Bright)
    Secondary  = Named(Blue, Normal)
    Accent     = Named(Magenta, Bright)
    Success    = Named(Green, Normal)
    Warning    = Named(Yellow, Normal)
    Error      = Named(Red, Bright)
    TextFg     = Default
    TextDim    = Named(White, Normal)
    Background = Named(Black, Normal)
    Border     = Rounded
}

let _headingCheck    : Element = Theme.heading    theme "Contact Form"
let _subheadingCheck : Element = Theme.subheading theme "Subtitle"
let _panelCheck      : Element = Theme.panel      theme "Title" (El.text "inner")
let _successCheck    : Element = Theme.success    theme "OK"
let _warningCheck    : Element = Theme.warning    theme "Watch out"
let _errorCheck      : Element = Theme.error      theme "Failed"
let _borderedCheck   : Element = Theme.bordered   theme (El.text "content")

// ── Section 6: Widget-level theming ──────────────────────────────────────────

let _barConfig = ProgressBar.withTheme Theme.dracula { ProgressBar.defaults with Percent = 0.7 }

let _cmdNone      : Cmd<ContactMsg> = Cmd.none
let _cmdQuit      : Cmd<ContactMsg> = Cmd.quit
let _cmdBatch     : Cmd<ContactMsg> = Cmd.batch [ Cmd.none; Cmd.none ]
let _cmdOfMsg     : Cmd<ContactMsg> = Cmd.ofMsg Quit

// ── Section 8: Subscription patterns ─────────────────────────────────────────

let _subKeyBind  : Sub<ContactMsg> = Keys.bind [ Key.Char 'q', Quit; Key.Escape, Quit ]
let _subKeySub   : Sub<ContactMsg> = Sub.KeySub(fun (k, mods) -> Some (KeyPressed(k, mods)))
let _subTimerSub : Sub<CounterMsg> = Sub.TimerSub("tick", System.TimeSpan.FromMilliseconds(1000.0), fun () -> Increment)

printfn "✓ Tutorial examples compiled successfully"
