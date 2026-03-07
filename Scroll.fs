namespace SageTUI

/// Immutable scroll position tracker with enforced invariants.
/// 0 <= Offset <= max(0, TotalItems - ViewportSize)
type ScrollState =
  private {
    _offset: int
    _totalItems: int
    _viewportSize: int
  }

module ScrollState =
  let private clamp total viewport offset =
    let maxOff = max 0 (total - viewport)
    max 0 (min offset maxOff)

  let create totalItems viewportSize =
    { _offset = 0
      _totalItems = max 0 totalItems
      _viewportSize = max 0 viewportSize }

  let offset (s: ScrollState) = s._offset
  let totalItems (s: ScrollState) = s._totalItems
  let viewportSize (s: ScrollState) = s._viewportSize
  let maxOffset (s: ScrollState) = max 0 (s._totalItems - s._viewportSize)

  let canScrollUp (s: ScrollState) = s._offset > 0
  let canScrollDown (s: ScrollState) = s._offset < maxOffset s

  let isAtTop (s: ScrollState) = s._offset = 0
  let isAtBottom (s: ScrollState) = s._offset >= maxOffset s

  /// Visible range as (startIndex, endIndexExclusive)
  let visibleRange (s: ScrollState) =
    let startIdx = s._offset
    let endIdx = min s._totalItems (s._offset + s._viewportSize)
    (startIdx, endIdx)

  let scrollTo line (s: ScrollState) =
    { s with _offset = clamp s._totalItems s._viewportSize line }

  let scrollBy delta (s: ScrollState) =
    scrollTo (s._offset + delta) s

  let scrollUp (s: ScrollState) = scrollBy -1 s
  let scrollDown (s: ScrollState) = scrollBy 1 s
  let scrollPageUp (s: ScrollState) = scrollBy -(s._viewportSize) s
  let scrollPageDown (s: ScrollState) = scrollBy s._viewportSize s
  let scrollToTop (s: ScrollState) = scrollTo 0 s
  let scrollToBottom (s: ScrollState) = scrollTo (maxOffset s) s

  /// Update total items count, clamping offset if necessary.
  let withTotalItems total (s: ScrollState) =
    let t = max 0 total
    { s with _totalItems = t; _offset = clamp t s._viewportSize s._offset }

  /// Update viewport size, clamping offset if necessary.
  let withViewportSize viewport (s: ScrollState) =
    let v = max 0 viewport
    { s with _viewportSize = v; _offset = clamp s._totalItems v s._offset }

  /// Ensure a specific index is visible, scrolling minimally if needed.
  let ensureVisible index (s: ScrollState) =
    match index with
    | i when i < s._offset -> scrollTo i s
    | i when i >= s._offset + s._viewportSize -> scrollTo (i - s._viewportSize + 1) s
    | _ -> s

/// Pure composition-based scrolling. No new Element DU case —
/// slices the item list and renders only visible items.
module Scroll =
  /// Render visible items as a Column. renderItem receives (absoluteIndex, item).
  let view (scroll: ScrollState) (renderItem: int -> 'a -> Element) (items: 'a list) : Element =
    let (startIdx, endIdx) = ScrollState.visibleRange scroll
    items
    |> List.skip startIdx
    |> List.take (endIdx - startIdx)
    |> List.mapi (fun i item -> renderItem (startIdx + i) item)
    |> El.column

  /// Render with a vertical scroll indicator bar.
  let viewWithIndicator (scroll: ScrollState) (renderItem: int -> 'a -> Element) (items: 'a list) : Element =
    let content = view scroll renderItem items
    let total = ScrollState.totalItems scroll
    let viewport = ScrollState.viewportSize scroll
    match total <= viewport with
    | true -> content
    | false ->
      let pct = float (ScrollState.offset scroll) / float (ScrollState.maxOffset scroll)
      let barH = max 1 (viewport * viewport / total)
      let barPos = int (float (viewport - barH) * pct)
      let indicator =
        List.init viewport (fun i ->
          match i >= barPos && i < barPos + barH with
          | true -> El.text "\u2588" |> El.fg (Color.Named(BaseColor.White, Intensity.Normal))
          | false -> El.text "\u2591" |> El.fg (Color.Named(BaseColor.Black, Intensity.Bright))
        )
        |> El.column
      El.row [ El.fill content; El.width 1 indicator ]

/// Scrollable list with selection tracking and automatic scroll-into-view.
type ScrollableListModel<'a> = {
  Items: 'a list
  SelectedIndex: int
  Scroll: ScrollState
}

module ScrollableList =
  let create (items: 'a list) (viewportSize: int) =
    { Items = items
      SelectedIndex = 0
      Scroll = ScrollState.create (List.length items) viewportSize }

  let selectedItem (model: ScrollableListModel<'a>) =
    match model.Items with
    | [] -> None
    | items when model.SelectedIndex >= 0 && model.SelectedIndex < List.length items ->
      Some items.[model.SelectedIndex]
    | _ -> None

  let private clampIndex items idx =
    match List.length items with
    | 0 -> 0
    | n -> max 0 (min idx (n - 1))

  let selectIndex idx (model: ScrollableListModel<'a>) =
    let i = clampIndex model.Items idx
    { model with
        SelectedIndex = i
        Scroll = ScrollState.ensureVisible i model.Scroll }

  let selectUp (model: ScrollableListModel<'a>) =
    selectIndex (model.SelectedIndex - 1) model

  let selectDown (model: ScrollableListModel<'a>) =
    selectIndex (model.SelectedIndex + 1) model

  let selectFirst (model: ScrollableListModel<'a>) =
    selectIndex 0 model

  let selectLast (model: ScrollableListModel<'a>) =
    selectIndex (List.length model.Items - 1) model

  let scrollUp (model: ScrollableListModel<'a>) =
    { model with Scroll = ScrollState.scrollUp model.Scroll }

  let scrollDown (model: ScrollableListModel<'a>) =
    { model with Scroll = ScrollState.scrollDown model.Scroll }

  /// Replace items, clamping selection and scroll offset.
  let withItems (items: 'a list) (model: ScrollableListModel<'a>) =
    let scroll = ScrollState.withTotalItems (List.length items) model.Scroll
    let idx = clampIndex items model.SelectedIndex
    { model with
        Items = items
        SelectedIndex = idx
        Scroll = ScrollState.ensureVisible idx scroll }

  /// Update viewport size (e.g., after terminal resize).
  let withViewportSize vp (model: ScrollableListModel<'a>) =
    { model with Scroll = ScrollState.withViewportSize vp model.Scroll }

  /// Render visible items. renderItem receives (isSelected, absoluteIndex, item).
  let view (renderItem: bool -> int -> 'a -> Element) (model: ScrollableListModel<'a>) : Element =
    Scroll.view model.Scroll (fun idx item ->
      let isSelected = idx = model.SelectedIndex
      renderItem isSelected idx item
    ) model.Items

  /// Render with a scroll indicator bar alongside the list.
  let viewWithScrollbar (renderItem: bool -> int -> 'a -> Element) (model: ScrollableListModel<'a>) : Element =
    Scroll.viewWithIndicator model.Scroll (fun idx item ->
      let isSelected = idx = model.SelectedIndex
      renderItem isSelected idx item
    ) model.Items
