namespace SageTUI

/// A typed, bidirectional navigation stack (zipper over a route DU).
///
/// Routes are user-defined discriminated unions — not URL strings.
/// Route payloads should carry identifiers, not large data.
///
/// Example:
///   type Page = Home | Settings | UserDetail of userId:int
///
///   let stack = Nav.init Home |> Nav.push Settings |> Nav.push (UserDetail 42)
///   let back  = Nav.pop stack   // Some { Current = Settings; ... }
type NavigationStack<'route> = {
  Current : 'route
  History : 'route list
  Forward : 'route list
}

/// Pure navigation functions for `NavigationStack<'route>`.
module Nav =

  /// Create a new stack with `route` as the only page.
  let init (route: 'route) : NavigationStack<'route> =
    { Current = route; History = []; Forward = [] }

  /// Navigate to a new `route`, adding the current page to history and clearing forward stack.
  let push (route: 'route) (stack: NavigationStack<'route>) : NavigationStack<'route> =
    { Current = route; History = stack.Current :: stack.History; Forward = [] }

  /// Go back one step. Returns `None` if already at the root.
  let pop (stack: NavigationStack<'route>) : NavigationStack<'route> option =
    match stack.History with
    | [] -> None
    | prev :: rest ->
      Some { Current = prev; History = rest; Forward = stack.Current :: stack.Forward }

  /// Go forward one step (undo a `pop`). Returns `None` if no forward history.
  let forward (stack: NavigationStack<'route>) : NavigationStack<'route> option =
    match stack.Forward with
    | [] -> None
    | next :: rest ->
      Some { Current = next; History = stack.Current :: stack.History; Forward = rest }

  /// Replace the current page without touching history or forward stack.
  let replace (route: 'route) (stack: NavigationStack<'route>) : NavigationStack<'route> =
    { stack with Current = route; Forward = [] }

  /// True when there is at least one page to go back to.
  let canGoBack (stack: NavigationStack<'route>) : bool =
    not (List.isEmpty stack.History)

  /// True when there is at least one page to go forward to.
  let canGoForward (stack: NavigationStack<'route>) : bool =
    not (List.isEmpty stack.Forward)

  /// All pages in order from oldest to current (excludes forward stack).
  let toList (stack: NavigationStack<'route>) : 'route list =
    List.rev stack.History @ [ stack.Current ]

  /// Number of pages from root to current (inclusive). Minimum 1.
  let depth (stack: NavigationStack<'route>) : int =
    List.length stack.History + 1
