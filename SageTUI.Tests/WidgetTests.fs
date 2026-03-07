module WidgetTests

open Expecto
open Expecto.Flip
open SageTUI

let progressBarTests = testList "ProgressBar" [
  test "0 percent shows all empty" {
    let config = { ProgressBar.defaults with Percent = 0.0; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has empty chars" "░░░░░░░░░░"
    | _ -> ()
  }
  test "100 percent shows all filled" {
    let config = { ProgressBar.defaults with Percent = 1.0; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has filled chars" "██████████"
    | _ -> ()
  }
  test "50 percent shows half" {
    let config = { ProgressBar.defaults with Percent = 0.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "has filled" "█████"
    | _ -> ()
  }
  test "with label shows Row" {
    let config = { ProgressBar.defaults with Percent = 0.75; Width = 10; ShowLabel = true }
    let elem = ProgressBar.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "bar + label" 2
    | _ -> failtest "expected Row"
  }
  test "clamps below 0" {
    let config = { ProgressBar.defaults with Percent = -0.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "all empty" "░░░░░░░░░░"
    | _ -> ()
  }
  test "clamps above 1" {
    let config = { ProgressBar.defaults with Percent = 1.5; Width = 10; ShowLabel = false }
    let elem = ProgressBar.view config
    match elem with
    | Text(t, _) -> t |> Expect.stringContains "all filled" "██████████"
    | _ -> ()
  }
  test "colored bar produces Row" {
    let config =
      { ProgressBar.defaults with
          Percent = 0.5; Width = 10; ShowLabel = false
          FilledColor = Some (Color.Named(BaseColor.Green, Intensity.Normal))
          EmptyColor = Some (Color.Named(BaseColor.Red, Intensity.Normal)) }
    let elem = ProgressBar.view config
    match elem with
    | Row _ -> ()
    | _ -> failtest "expected Row for colored bar"
  }
]

let tabsTests = testList "Tabs" [
  test "renders all tabs as Row" {
    let config: TabsConfig<string> = {
      Items = ["Home"; "Settings"; "Help"]
      ActiveIndex = 0
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "3 tabs" 3
    | _ -> failtest "expected Row"
  }
  test "active tab is bold" {
    let config: TabsConfig<string> = {
      Items = ["A"; "B"]
      ActiveIndex = 1
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row [_; Styled(s, _)] ->
      s.Attrs.Value &&& 1us |> Expect.equal "bold bit" 1us
    | other -> failtest (sprintf "expected styled active tab, got %A" other)
  }
  test "empty items produces empty Row" {
    let config: TabsConfig<string> = {
      Items = []
      ActiveIndex = 0
      ToString = id
      ActiveColor = None
      InactiveColor = None
    }
    let elem = Tabs.view config
    match elem with
    | Row children -> List.length children |> Expect.equal "empty" 0
    | _ -> failtest "expected Row"
  }
]

let tableTests = testList "Table" [
  test "produces Column with header, separator, rows" {
    let cols: TableColumn<string * int> list = [
      { Header = "Name"; Width = 10; Render = fun (n, _) -> El.text n }
      { Header = "Age"; Width = 5; Render = fun (_, a) -> El.text (string a) }
    ]
    let rows = [("Alice", 30); ("Bob", 25)]
    let elem = Table.view cols rows None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep+2rows" 4
    | _ -> failtest "expected Column"
  }
  test "selected row gets styled" {
    let cols: TableColumn<string> list = [
      { Header = "X"; Width = 5; Render = fun x -> El.text x }
    ]
    let rows = ["a"; "b"; "c"]
    let elem = Table.view cols rows (Some 1)
    match elem with
    | Column (_::_::dataRows) ->
      match List.item 1 dataRows with
      | Styled(s, _) -> s.Bg |> Expect.isSome "has bg color"
      | _ -> failtest "selected row should be styled"
    | _ -> failtest "expected Column"
  }
  test "empty rows has header and separator only" {
    let cols: TableColumn<string> list = [
      { Header = "H"; Width = 3; Render = fun _ -> El.text "" }
    ]
    let elem = Table.view cols [] None
    match elem with
    | Column children -> List.length children |> Expect.equal "header+sep" 2
    | _ -> failtest "expected Column"
  }
  test "multiple columns render in each row" {
    let cols: TableColumn<int> list = [
      { Header = "A"; Width = 5; Render = fun x -> El.text (string x) }
      { Header = "B"; Width = 5; Render = fun x -> El.text (string (x * 2)) }
    ]
    let rows = [1; 2; 3]
    let elem = Table.view cols rows None
    match elem with
    | Column children ->
      List.length children |> Expect.equal "header+sep+3rows" 5
      match List.item 2 children with
      | Row cells -> List.length cells |> Expect.equal "2 columns" 2
      | _ -> failtest "data row should be Row"
    | _ -> failtest "expected Column"
  }
]

[<Tests>]
let allWidgetTests = testList "Widgets" [
  progressBarTests
  tabsTests
  tableTests
]
