// Tutorial verification script — compiled by CI to catch API drift.
// Run: dotnet fsi docs/tutorial-verify.fsx (after dotnet build)
// This does NOT run a terminal app; it type-checks all tutorial patterns.
// References the net8.0 output since SageTUI is a multi-TFM library (net8.0;net9.0;net10.0)
// and net8.0 is the lowest common denominator that always exists after any build.
#r "../bin/Debug/net8.0/SageTUI.Library.dll"

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

let quitBinding = Keys.bind [ Key.Char (Text.Rune 'q'), Quit; Key.Escape, Quit ]

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

// ── Section 7: Command patterns ───────────────────────────────────────────────

type DataMsg = DataLoaded of string | LoadFailed of exn

let _cmdNone      : Cmd<ContactMsg> = Cmd.none
let _cmdQuit      : Cmd<ContactMsg> = Cmd.quit
let _cmdBatch     : Cmd<ContactMsg> = Cmd.batch [ Cmd.none; Cmd.none ]
let _cmdOfMsg     : Cmd<ContactMsg> = Cmd.ofMsg Quit
let _cmdDelay     : Cmd<ContactMsg> = Cmd.delay 500 Quit

// ofTaskResult — preferred async pattern (handles success + failure)
let _cmdTaskResult : Cmd<DataMsg> =
    Cmd.ofTaskResult
        (fun () -> System.Threading.Tasks.Task.FromResult("data"))
        DataLoaded
        LoadFailed

// ofAsync — fire-and-forget async
let _cmdAsync : Cmd<DataMsg> =
    Cmd.ofAsync (fun dispatch ->
        async {
            dispatch (DataLoaded "result")
        })

// ofCancellableAsync — long-running with cancellation support
let _cmdCancellable : Cmd<DataMsg> =
    Cmd.ofCancellableAsync "my-fetch" (fun _ct dispatch ->
        async {
            dispatch (DataLoaded "result")
        })

// Cmd.cancel — cancel a running cancellable async by its id string
let _cmdCancel : Cmd<DataMsg> = Cmd.cancel "my-fetch"

// ── Section 8: Subscription patterns ─────────────────────────────────────────

type AppMsg2 = Key of Key * Modifiers | Tick | Resize of int * int | Quit2

let _subKeyBind  : Sub<ContactMsg>  = Keys.bind [ Key.Char (Text.Rune 'q'), Quit; Key.Escape, Quit ]
let _subKeySub   : Sub<AppMsg2>     = Sub.KeySub(fun (k, mods) -> Some (AppMsg2.Key(k, mods)))
let _subTimerSub : Sub<AppMsg2>     = Sub.TimerSub("tick", System.TimeSpan.FromMilliseconds(1000.0), fun () -> Tick)
let _subResizeSub: Sub<AppMsg2>     = Sub.ResizeSub(fun (w, h) -> Some (Resize(w, h)))

printfn "✓ Tutorial examples compiled successfully"
