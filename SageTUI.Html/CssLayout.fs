namespace SageTUI.Html

open SageTUI

module CssLayout =

  let isBlockLevel (d: CssDisplay) =
    match d with
    | CssBlock | CssFlex _ | CssTable | CssListItem
    | CssTableRow | CssTableCell -> true
    | _ -> false

  /// MDN Block/Inline flow layout.
  /// Given children with their computed display types, produce the
  /// correct Element tree: Column for block, Row for inline,
  /// anonymous box wrapping for mixed content.
  let flowLayout (children: (CssDisplay * Element) list) : Element =
    match children with
    | [] -> Element.Empty
    | [(_, single)] -> single
    | _ ->
      let hasBlock = children |> List.exists (fun (d, _) -> isBlockLevel d)
      let hasInline = children |> List.exists (fun (d, _) -> not (isBlockLevel d))
      match hasBlock, hasInline with
      | true, false ->
        Element.Column (children |> List.map snd)
      | false, true ->
        Element.Row (children |> List.map snd)
      | true, true ->
        let rec groupRuns acc curInline items =
          match items with
          | [] ->
            match curInline with
            | [] -> List.rev acc
            | inls -> List.rev (Element.Row (List.rev inls) :: acc)
          | (d, el) :: rest ->
            match isBlockLevel d with
            | true ->
              match curInline with
              | [] -> groupRuns (el :: acc) [] rest
              | inls -> groupRuns (el :: Element.Row (List.rev inls) :: acc) [] rest
            | false ->
              groupRuns acc (el :: curInline) rest
        Element.Column (groupRuns [] [] children)
      | false, false -> Element.Empty
