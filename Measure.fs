namespace SageTUI

open System.Text

/// Intrinsic content measurement for Element trees.
/// Analogous to CSS min-content / max-content sizing.
module Measure =
  let textWidth (s: string) =
    let mutable w = 0
    for rune in s.EnumerateRunes() do
      w <- w + RuneWidth.getColumnWidth rune
    w

  let rec measureWidth (elem: Element) : int =
    match elem with
    | Empty -> 0
    | Text(s, _) -> textWidth s
    | Row children -> children |> List.sumBy measureWidth
    | Column children ->
      match children with
      | [] -> 0
      | _ -> children |> List.map measureWidth |> List.max
    | Overlay children ->
      match children with
      | [] -> 0
      | _ -> children |> List.map measureWidth |> List.max
    | Styled(_, child) -> measureWidth child
    | Constrained(Fixed n, _) -> n
    | Constrained(Min n, child) -> max n (measureWidth child)
    | Constrained(Max n, child) -> min n (measureWidth child)
    | Constrained(_, child) -> measureWidth child
    | Bordered(_, child) -> measureWidth child + 2
    | Padded(p, child) -> measureWidth child + p.Left + p.Right
    | Keyed(_, _, _, child) -> measureWidth child
    | Canvas _ -> 0
    | Aligned(_, _, child) -> measureWidth child
    | Gapped(_, child) -> measureWidth child

  let rec measureHeight (elem: Element) : int =
    match elem with
    | Empty -> 0
    | Text _ -> 1
    | Row children ->
      match children with
      | [] -> 0
      | _ -> children |> List.map measureHeight |> List.max
    | Column children ->
      // In column context, Constrained(Fixed n) IS a height allocation
      children |> List.sumBy (fun child ->
        match child with
        | Constrained(Fixed n, _) -> n
        | Constrained(Min n, inner) -> max n (measureHeight inner)
        | Constrained(Max n, inner) -> min n (measureHeight inner)
        | _ -> measureHeight child)
    | Overlay children ->
      match children with
      | [] -> 0
      | _ -> children |> List.map measureHeight |> List.max
    | Styled(_, child) -> measureHeight child
    | Constrained(_, child) -> measureHeight child
    | Bordered(_, child) -> measureHeight child + 2
    | Padded(p, child) -> measureHeight child + p.Top + p.Bottom
    | Keyed(_, _, _, child) -> measureHeight child
    | Canvas _ -> 0
    | Aligned(_, _, child) -> measureHeight child
    | Gapped(_, child) -> measureHeight child

  /// Measure content sizes for children in a Row context (width dimension).
  let childWidths (children: Element list) : int list =
    children |> List.map (fun child ->
      match child with
      | Constrained(_, inner) -> measureWidth inner
      | other -> measureWidth other)

  /// Measure content sizes for children in a Column context (height dimension).
  let childHeights (children: Element list) : int list =
    children |> List.map (fun child ->
      match child with
      | Constrained(_, inner) -> measureHeight inner
      | other -> measureHeight other)
