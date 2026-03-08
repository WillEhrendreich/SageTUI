module ScrollTests

open Expecto
open Expecto.Flip
open SageTUI

let scrollStateCreationTests = testList "ScrollState.creation" [
  test "starts at offset 0" {
    let s = ScrollState.create 100 10
    ScrollState.offset s |> Expect.equal "offset" 0
  }
  test "stores total items" {
    let s = ScrollState.create 100 10
    ScrollState.totalItems s |> Expect.equal "total" 100
  }
  test "stores viewport size" {
    let s = ScrollState.create 100 10
    ScrollState.viewportSize s |> Expect.equal "viewport" 10
  }
  test "negative total clamped to 0" {
    let s = ScrollState.create -5 10
    ScrollState.totalItems s |> Expect.equal "total" 0
  }
  test "negative viewport clamped to 0" {
    let s = ScrollState.create 100 -3
    ScrollState.viewportSize s |> Expect.equal "viewport" 0
  }
]

let scrollStateMaxOffsetTests = testList "ScrollState.maxOffset" [
  test "100 items, 10 viewport = 90" {
    let s = ScrollState.create 100 10
    ScrollState.maxOffset s |> Expect.equal "max" 90
  }
  test "5 items, 10 viewport = 0" {
    let s = ScrollState.create 5 10
    ScrollState.maxOffset s |> Expect.equal "max" 0
  }
  test "0 items = 0" {
    let s = ScrollState.create 0 10
    ScrollState.maxOffset s |> Expect.equal "max" 0
  }
  test "equal items and viewport = 0" {
    let s = ScrollState.create 10 10
    ScrollState.maxOffset s |> Expect.equal "max" 0
  }
]

let scrollStateScrollByTests = testList "ScrollState.scrollBy" [
  test "scroll down 1" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollBy 1
    ScrollState.offset s |> Expect.equal "offset" 1
  }
  test "scroll down past max clamps" {
    let s = ScrollState.create 15 10 |> ScrollState.scrollBy 100
    ScrollState.offset s |> Expect.equal "offset" 5
  }
  test "scroll up from 0 stays 0" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollBy -5
    ScrollState.offset s |> Expect.equal "offset" 0
  }
  test "scroll up from middle" {
    let s =
      ScrollState.create 100 10
      |> ScrollState.scrollTo 50
      |> ScrollState.scrollBy -3
    ScrollState.offset s |> Expect.equal "offset" 47
  }
]

let scrollStateScrollToTests = testList "ScrollState.scrollTo" [
  test "scroll to specific position" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 42
    ScrollState.offset s |> Expect.equal "offset" 42
  }
  test "scroll to negative clamps to 0" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo -10
    ScrollState.offset s |> Expect.equal "offset" 0
  }
  test "scroll to past max clamps" {
    let s = ScrollState.create 20 10 |> ScrollState.scrollTo 15
    ScrollState.offset s |> Expect.equal "offset" 10
  }
]

let scrollStateConvenienceTests = testList "ScrollState.convenience" [
  test "scrollUp from 5" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 5 |> ScrollState.scrollUp
    ScrollState.offset s |> Expect.equal "offset" 4
  }
  test "scrollDown from 0" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollDown
    ScrollState.offset s |> Expect.equal "offset" 1
  }
  test "scrollPageDown" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollPageDown
    ScrollState.offset s |> Expect.equal "offset" 10
  }
  test "scrollPageUp from 15" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 15 |> ScrollState.scrollPageUp
    ScrollState.offset s |> Expect.equal "offset" 5
  }
  test "scrollToTop from 50" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 50 |> ScrollState.scrollToTop
    ScrollState.offset s |> Expect.equal "offset" 0
  }
  test "scrollToBottom" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollToBottom
    ScrollState.offset s |> Expect.equal "offset" 90
  }
]

let scrollStatePredicateTests = testList "ScrollState.predicates" [
  test "canScrollUp at 0 is false" {
    let s = ScrollState.create 100 10
    ScrollState.canScrollUp s |> Expect.isFalse "at top"
  }
  test "canScrollUp at 5 is true" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 5
    ScrollState.canScrollUp s |> Expect.isTrue "not at top"
  }
  test "canScrollDown at max is false" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollToBottom
    ScrollState.canScrollDown s |> Expect.isFalse "at bottom"
  }
  test "canScrollDown at 0 is true" {
    let s = ScrollState.create 100 10
    ScrollState.canScrollDown s |> Expect.isTrue "not at bottom"
  }
  test "canScrollDown when all items fit is false" {
    let s = ScrollState.create 5 10
    ScrollState.canScrollDown s |> Expect.isFalse "all visible"
  }
]

let scrollStateVisibleRangeTests = testList "ScrollState.visibleRange" [
  test "at top: 0 to 10" {
    let s = ScrollState.create 100 10
    ScrollState.visibleRange s |> Expect.equal "range" (0, 10)
  }
  test "at offset 5: 5 to 15" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 5
    ScrollState.visibleRange s |> Expect.equal "range" (5, 15)
  }
  test "near bottom: clamped to total" {
    let s = ScrollState.create 15 10 |> ScrollState.scrollToBottom
    ScrollState.visibleRange s |> Expect.equal "range" (5, 15)
  }
  test "fewer items than viewport" {
    let s = ScrollState.create 3 10
    ScrollState.visibleRange s |> Expect.equal "range" (0, 3)
  }
]

let scrollStateEnsureVisibleTests = testList "ScrollState.ensureVisible" [
  test "item above viewport scrolls up" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 20 |> ScrollState.ensureVisible 15
    ScrollState.offset s |> Expect.equal "offset" 15
  }
  test "item below viewport scrolls down" {
    let s = ScrollState.create 100 10 |> ScrollState.ensureVisible 15
    ScrollState.offset s |> Expect.equal "offset" 6
  }
  test "item already visible stays" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 10 |> ScrollState.ensureVisible 15
    ScrollState.offset s |> Expect.equal "offset" 10
  }
  test "item at viewport boundary stays" {
    let s = ScrollState.create 100 10 |> ScrollState.ensureVisible 9
    ScrollState.offset s |> Expect.equal "offset" 0
  }
]

let scrollStateResizeTests = testList "ScrollState.resize" [
  test "withTotalItems increase keeps offset" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 50 |> ScrollState.withTotalItems 200
    ScrollState.offset s |> Expect.equal "offset" 50
    ScrollState.totalItems s |> Expect.equal "total" 200
  }
  test "withTotalItems decrease clamps offset" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 95 |> ScrollState.withTotalItems 50
    ScrollState.offset s |> Expect.equal "offset" 40
  }
  test "withViewportSize increase clamps offset" {
    let s = ScrollState.create 20 10 |> ScrollState.scrollTo 10 |> ScrollState.withViewportSize 15
    ScrollState.offset s |> Expect.equal "offset" 5
  }
  test "withViewportSize decrease keeps valid offset" {
    let s = ScrollState.create 100 10 |> ScrollState.scrollTo 5 |> ScrollState.withViewportSize 5
    ScrollState.offset s |> Expect.equal "offset" 5
  }
]

let scrollViewTests = testList "Scroll.view" [
  test "renders only visible items" {
    let items = [ "a"; "b"; "c"; "d"; "e" ]
    let scroll = ScrollState.create 5 3
    let result = Scroll.view scroll (fun _ item -> El.text item) items
    match result with
    | Column children -> List.length children |> Expect.equal "count" 3
    | _ -> failtest "expected Column"
  }
  test "scrolled down shows later items" {
    let items = [ "a"; "b"; "c"; "d"; "e" ]
    let scroll = ScrollState.create 5 3 |> ScrollState.scrollTo 2
    let result = Scroll.view scroll (fun _ item -> El.text item) items
    match result with
    | Column children ->
      List.length children |> Expect.equal "count" 3
      match children.[0] with
      | Text(s, _) -> s |> Expect.equal "first visible" "c"
      | _ -> failtest "expected Text"
    | _ -> failtest "expected Column"
  }
  test "fewer items than viewport shows all" {
    let items = [ "x"; "y" ]
    let scroll = ScrollState.create 2 5
    let result = Scroll.view scroll (fun _ item -> El.text item) items
    match result with
    | Column children -> List.length children |> Expect.equal "count" 2
    | _ -> failtest "expected Column"
  }
  test "empty items produces empty column" {
    let scroll = ScrollState.create 0 5
    let result = Scroll.view scroll (fun _ item -> El.text item) ([] : string list)
    match result with
    | Column children -> List.length children |> Expect.equal "count" 0
    | _ -> failtest "expected Column"
  }
  test "renderItem receives correct absolute index" {
    let items = [ "a"; "b"; "c"; "d"; "e" ]
    let scroll = ScrollState.create 5 3 |> ScrollState.scrollTo 2
    let indices = System.Collections.Generic.List<int>()
    let _ = Scroll.view scroll (fun idx item -> indices.Add(idx); El.text item) items
    Seq.toList indices |> Expect.equal "indices" [2; 3; 4]
  }
]

let scrollableListTests = testList "ScrollableList" [
  test "create with items" {
    let model = ScrollableList.create ["a";"b";"c";"d";"e"] 3
    model.SelectedIndex |> Expect.equal "selected" 0
    ScrollState.totalItems model.Scroll |> Expect.equal "total" 5
    ScrollState.viewportSize model.Scroll |> Expect.equal "viewport" 3
  }
  test "selectDown moves selection and scroll" {
    let model = ScrollableList.create ["a";"b";"c";"d";"e"] 3
    let model2 = model |> ScrollableList.selectDown |> ScrollableList.selectDown |> ScrollableList.selectDown
    model2.SelectedIndex |> Expect.equal "selected" 3
    ScrollState.offset model2.Scroll |> Expect.equal "scrolled to see item 3" 1
  }
  test "selectUp from 0 stays at 0" {
    let model = ScrollableList.create ["a";"b";"c"] 3
    let model2 = ScrollableList.selectUp model
    model2.SelectedIndex |> Expect.equal "selected" 0
  }
  test "selectLast scrolls to bottom" {
    let model = ScrollableList.create (List.init 20 string) 5
    let model2 = ScrollableList.selectLast model
    model2.SelectedIndex |> Expect.equal "selected" 19
    ScrollState.canScrollDown model2.Scroll |> Expect.isFalse "at bottom"
  }
  test "selectedItem returns correct item" {
    let model = ScrollableList.create ["x";"y";"z"] 3 |> ScrollableList.selectDown
    ScrollableList.selectedItem model |> Expect.equal "item" (Some "y")
  }
  test "empty list returns None" {
    let model = ScrollableList.create ([] : string list) 3
    ScrollableList.selectedItem model |> Expect.equal "item" None
  }
  test "withItems updates and clamps" {
    let model =
      ScrollableList.create (List.init 20 string) 5
      |> ScrollableList.selectIndex 15
      |> ScrollableList.withItems (List.init 10 string)
    model.SelectedIndex |> Expect.equal "clamped" 9
    ScrollState.totalItems model.Scroll |> Expect.equal "new total" 10
  }
  test "view renders visible items only" {
    let model = ScrollableList.create ["a";"b";"c";"d";"e"] 3
    let result = ScrollableList.view (fun _ _ item -> El.text item) model
    match result with
    | Column children -> List.length children |> Expect.equal "visible" 3
    | _ -> failtest "expected Column"
  }
]

let scrollVirtualTests = testList "Scroll.viewVirtual" [
  test "renders loaded items" {
    let rendered = Scroll.viewVirtual 10 0 5 (fun i -> El.text (sprintf "item-%d" i)) (fun _ -> false)
    match rendered with
    | Column children ->
      children |> List.length |> Expect.equal "5 items" 5
      // First item should be a loaded El.text, not a loading placeholder
      match children.[0] with
      | Text("item-0", _) -> ()
      | other -> failwithf "expected Text item-0 but got %A" other
    | _ -> failtest "expected Column"
  }

  test "renders loading placeholder for loading indices" {
    let rendered = Scroll.viewVirtual 10 0 3 (fun i -> El.text (sprintf "item-%d" i)) (fun i -> i = 1)
    match rendered with
    | Column [_; loadingRow; _] ->
      // The loading row should be a Row (not a text "item-1")
      match loadingRow with
      | Row _ -> ()
      | other -> failwithf "expected Row placeholder but got %A" other
    | Column children -> failwithf "expected 3 children but got %d" (List.length children)
    | _ -> failtest "expected Column"
  }

  test "respects offset" {
    let rendered = Scroll.viewVirtual 10 5 3 (fun i -> El.text (sprintf "item-%d" i)) (fun _ -> false)
    match rendered with
    | Column children ->
      children |> List.length |> Expect.equal "3 items from offset 5" 3
      match children.[0] with
      | Text("item-5", _) -> ()
      | other -> failwithf "expected item-5 but got %A" other
    | _ -> failtest "expected Column"
  }

  test "clamps offset to totalCount - viewportSize" {
    // offset 8 with total 10 and viewport 5 → clamps to 5
    let rendered = Scroll.viewVirtual 10 8 5 (fun i -> El.text (sprintf "item-%d" i)) (fun _ -> false)
    match rendered with
    | Column children ->
      children |> List.length |> Expect.equal "5 items" 5
      match children.[0] with
      | Text("item-5", _) -> ()
      | other -> failwithf "expected item-5 but got %A" other
    | _ -> failtest "expected Column"
  }

  test "handles totalCount smaller than viewportSize" {
    let rendered = Scroll.viewVirtual 3 0 10 (fun i -> El.text (sprintf "item-%d" i)) (fun _ -> false)
    match rendered with
    | Column children -> children |> List.length |> Expect.equal "3 items (all of them)" 3
    | _ -> failtest "expected Column"
  }

  test "empty totalCount gives empty column" {
    let rendered = Scroll.viewVirtual 0 0 5 (fun i -> El.text (sprintf "item-%d" i)) (fun _ -> false)
    match rendered with
    | Column [] -> ()
    | Column children -> failwithf "expected empty but got %d items" (List.length children)
    | _ -> failtest "expected Column"
  }
]

[<Tests>]
let allScrollTests = testList "Scrolling" [
  scrollStateCreationTests
  scrollStateMaxOffsetTests
  scrollStateScrollByTests
  scrollStateScrollToTests
  scrollStateConvenienceTests
  scrollStatePredicateTests
  scrollStateVisibleRangeTests
  scrollStateEnsureVisibleTests
  scrollStateResizeTests
  scrollViewTests
  scrollableListTests
  scrollVirtualTests
]
