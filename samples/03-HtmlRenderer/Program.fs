module HtmlRenderer

// Render raw HTML to the terminal — SageTUI's killer feature.
// Demonstrates: HtmlString.parseFragment, HTML→Element bridge, CSS colors,
// semantic tags, tables, lists, links, inline styles, forms.

open System.Text
open SageTUI
open SageTUI.Html

let pages = [|
  // Page 0: Styled content
  "Styled HTML",
  """<div>
  <h1 style="color: cyan">Welcome to SageTUI</h1>
  <p>This is <b>bold</b>, <i>italic</i>, and <u>underlined</u> text
     rendered from <span style="color: lightgreen">raw HTML</span>.</p>
  <p style="color: yellow">HTML is the universal UI language.
     Now your terminal speaks it too.</p>
</div>"""

  // Page 1: Lists and links
  "Lists & Links",
  """<div>
  <h2 style="color: cyan">Features</h2>
  <ul>
    <li><b style="color: lightgreen">Zero-GC rendering</b> — Arena + SIMD diff</li>
    <li><b style="color: lightgreen">HTML bridge</b> — Render web pages in your terminal</li>
    <li><b style="color: lightgreen">TEA architecture</b> — Elm-style state management</li>
    <li><b style="color: lightgreen">TrueColor</b> — Full 24-bit RGB support</li>
    <li><b style="color: lightgreen">Animations</b> — Fade, wipe, dissolve transitions</li>
  </ul>
  <p>Visit <a href="https://github.com/example/sagetui">SageTUI on GitHub</a></p>
</div>"""

  // Page 2: Table
  "Tables",
  """<div>
  <h2 style="color: cyan">System Status</h2>
  <table>
    <tr>
      <th style="color: yellow"><b>Service</b></th>
      <th style="color: yellow"><b>Status</b></th>
      <th style="color: yellow"><b>Latency</b></th>
    </tr>
    <tr>
      <td>API Gateway</td>
      <td style="color: lightgreen">● Online</td>
      <td>12ms</td>
    </tr>
    <tr>
      <td>Database</td>
      <td style="color: lightgreen">● Online</td>
      <td>3ms</td>
    </tr>
    <tr>
      <td>Cache</td>
      <td style="color: yellow">● Degraded</td>
      <td>45ms</td>
    </tr>
    <tr>
      <td>Worker</td>
      <td style="color: red">● Offline</td>
      <td>—</td>
    </tr>
  </table>
</div>"""

  // Page 3: Form elements
  "Forms",
  """<div>
  <h2 style="color: cyan">Login Form</h2>
  <div style="padding: 1">
    <div>
      <label style="color: yellow">Username:</label>
      <input type="text" placeholder="Enter username" />
    </div>
    <br/>
    <div>
      <label style="color: yellow">Password:</label>
      <input type="password" placeholder="Enter password" />
    </div>
    <br/>
    <button style="color: black; background-color: cyan"> Submit </button>
  </div>
</div>"""

  // Page 4: Nested styles and color showcase
  "Color Showcase",
  """<div>
  <h2 style="color: cyan">RGB Color Gradient</h2>
  <p>
    <span style="color: rgb(255,0,0)">██</span>
    <span style="color: rgb(255,64,0)">██</span>
    <span style="color: rgb(255,128,0)">██</span>
    <span style="color: rgb(255,192,0)">██</span>
    <span style="color: rgb(255,255,0)">██</span>
    <span style="color: rgb(192,255,0)">██</span>
    <span style="color: rgb(128,255,0)">██</span>
    <span style="color: rgb(0,255,0)">██</span>
    <span style="color: rgb(0,255,128)">██</span>
    <span style="color: rgb(0,255,255)">██</span>
    <span style="color: rgb(0,128,255)">██</span>
    <span style="color: rgb(0,0,255)">██</span>
    <span style="color: rgb(128,0,255)">██</span>
    <span style="color: rgb(255,0,255)">██</span>
    <span style="color: rgb(255,0,128)">██</span>
  </p>
  <h3 style="color: yellow">Named Colors</h3>
  <p>
    <span style="color: red">■ Red</span>
    <span style="color: green"> ■ Green</span>
    <span style="color: blue"> ■ Blue</span>
    <span style="color: cyan"> ■ Cyan</span>
    <span style="color: magenta"> ■ Magenta</span>
    <span style="color: yellow"> ■ Yellow</span>
  </p>
</div>"""

  // Page 5: Real-world content — a README-style page
  "README Preview",
  """<div>
  <h1 style="color: cyan">SageTUI</h1>
  <p><b>A functional terminal UI library for F#</b></p>
  <hr/>
  <h2 style="color: yellow">Quick Start</h2>
  <pre style="color: lightgreen">
dotnet add package SageTUI
  </pre>
  <p>Create your first app in <b>40 lines</b>:</p>
  <ol>
    <li>Define your <b>Model</b> — immutable state</li>
    <li>Define <b>Msg</b> — what can happen</li>
    <li>Write <b>Update</b> — pure state transitions</li>
    <li>Write <b>View</b> — model → elements</li>
  </ol>
  <h2 style="color: yellow">Why SageTUI?</h2>
  <ul>
    <li><span style="color: lightgreen">✓</span> The Elm Architecture — predictable state management</li>
    <li><span style="color: lightgreen">✓</span> HTML rendering — <code>curl</code> a page and display it</li>
    <li><span style="color: lightgreen">✓</span> Zero-GC — arena allocator + SIMD diff</li>
    <li><span style="color: lightgreen">✓</span> 24-bit color, transitions, widgets</li>
  </ul>
  <p style="color: gray"><i>MIT License — Built with ♥ in F#</i></p>
</div>"""
|]

type Model =
  { PageIndex: int
    Rendered: Element }

type Msg =
  | NextPage
  | PrevPage
  | Quit

let renderPage idx =
  let (_, html) = pages.[idx]
  HtmlString.parseFragment html

let init () =
  { PageIndex = 0; Rendered = renderPage 0 }, Cmd.none

let update msg model =
  match msg with
  | NextPage ->
    let idx = min (pages.Length - 1) (model.PageIndex + 1)
    { model with PageIndex = idx; Rendered = renderPage idx }, Cmd.none
  | PrevPage ->
    let idx = max 0 (model.PageIndex - 1)
    { model with PageIndex = idx; Rendered = renderPage idx }, Cmd.none
  | Quit -> model, Cmd.quit

let view model =
  let (title, _) = pages.[model.PageIndex]
  let nav =
    El.row [
      El.text (sprintf " ◀ %d/%d ▶  " (model.PageIndex + 1) pages.Length)
        |> El.fg (Color.Named(Yellow, Bright))
      El.text title
        |> El.bold
        |> El.fg (Color.Named(Cyan, Bright))
      El.fill (El.text "")
      El.text "HTML → Terminal "
        |> El.dim
    ]
    |> El.bg (Color.Named(Black, Normal))
  let content =
    model.Rendered
    |> El.padAll 1
  let footer =
    El.text " [←/→] Navigate  [q] Quit" |> El.dim
  El.column [
    nav
    El.fill content
    footer
  ]

let keyBindings =
  Keys.bind [
    Key.Right, NextPage
    Key.Char (Rune 'l'), NextPage
    Key.Left, PrevPage
    Key.Char (Rune 'h'), PrevPage
    Key.Char (Rune 'q'), Quit
    Key.Char (Rune 'Q'), Quit
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