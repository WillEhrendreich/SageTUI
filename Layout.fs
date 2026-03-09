namespace SageTUI

type Area = { X: int; Y: int; Width: int; Height: int }

[<RequireQualifiedAccess>]
type HAlign = Left | HCenter | Right
[<RequireQualifiedAccess>]
type VAlign = Top | VCenter | Bottom

type Constraint =
  | Fixed of int
  | Min of int
  | Max of int
  | Percentage of int
  | Fill
  | Ratio of int * int

type BorderStyle = Light | Heavy | Double | Rounded | Ascii

type Padding = { Top: int; Right: int; Bottom: int; Left: int }

module Padding =
  let zero = { Top = 0; Right = 0; Bottom = 0; Left = 0 }
  let all n = { Top = n; Right = n; Bottom = n; Left = n }
  let hv h v = { Top = v; Right = h; Bottom = v; Left = h }
  let horizontal (p: Padding) = p.Left + p.Right
  let vertical (p: Padding) = p.Top + p.Bottom

module Layout =
  let solve (available: int) (constraints: Constraint list) : (int * int) list =
    let n = List.length constraints
    let sizes = Array.create n 0
    let mutable remaining = available
    // Min participates in fill distribution only when no Fill items exist.
    // When Fill items exist, Min acts as a floor (Fixed-like) and Fill takes surplus.
    let hasFill = constraints |> List.exists (function Fill -> true | _ -> false)
    let mutable fillCount = 0

    constraints |> List.iteri (fun i c ->
      match c with
      | Fixed size ->
        sizes[i] <- min size remaining
        remaining <- remaining - sizes[i]
      | Percentage pct ->
        sizes[i] <- available * pct / 100
        remaining <- remaining - sizes[i]
      | Ratio(num, den) when den > 0 ->
        sizes[i] <- available * num / den
        remaining <- remaining - sizes[i]
      | Min minSize ->
        sizes[i] <- min minSize remaining
        remaining <- remaining - sizes[i]
        match hasFill with
        | false -> fillCount <- fillCount + 1  // grows to fill surplus when no Fill items
        | true  -> ()                          // stays at floor when Fill items exist
      | Max maxSize ->
        sizes[i] <- min maxSize remaining
        remaining <- remaining - sizes[i]
      | Fill ->
        fillCount <- fillCount + 1
      | _ -> ())

    match fillCount > 0 with
    | true ->
      let perFill = max 0 (remaining / fillCount)
      let mutable extra = max 0 (remaining % fillCount)
      constraints |> List.iteri (fun i c ->
        match c with
        | Fill ->
          sizes[i] <- perFill + (match extra > 0 with | true -> extra <- extra - 1; 1 | false -> 0)
        | Min _ when not hasFill ->
          sizes[i] <- sizes[i] + perFill + (match extra > 0 with | true -> extra <- extra - 1; 1 | false -> 0)
        | _ -> ())
    | false -> ()

    let mutable offset = 0
    [ for i in 0 .. n - 1 do
        let result = (offset, sizes[i])
        offset <- offset + sizes[i]
        yield result ]

  /// Content-aware layout: Fill children get at least their content size,
  /// then remaining space is distributed equally (like CSS flex-basis: auto).
  let solveWithContent (available: int) (constraints: Constraint list) (contentSizes: int list) : (int * int) list =
    let n = List.length constraints
    let sizes = Array.create n 0
    let mutable remaining = available
    let hasFill = constraints |> List.exists (function Fill -> true | _ -> false)
    let mutable fillCount = 0
    let mutable fillContentTotal = 0
    let contentArr = List.toArray contentSizes

    // Phase 1: allocate non-Fill items (identical to solve)
    constraints |> List.iteri (fun i c ->
      match c with
      | Fixed size ->
        sizes[i] <- min size remaining
        remaining <- remaining - sizes[i]
      | Percentage pct ->
        sizes[i] <- available * pct / 100
        remaining <- remaining - sizes[i]
      | Ratio(num, den) when den > 0 ->
        sizes[i] <- available * num / den
        remaining <- remaining - sizes[i]
      | Min minSize ->
        sizes[i] <- min minSize remaining
        remaining <- remaining - sizes[i]
        match hasFill with
        | false ->
          fillCount <- fillCount + 1
          fillContentTotal <- fillContentTotal + max 0 (contentArr[i] - sizes[i])
        | true -> ()
      | Max maxSize ->
        sizes[i] <- min maxSize remaining
        remaining <- remaining - sizes[i]
      | Fill ->
        fillCount <- fillCount + 1
        fillContentTotal <- fillContentTotal + contentArr[i]
      | _ -> ())

    // Phase 2: allocate Fill/Min items with content awareness
    // Min items participate in this phase only when no Fill items exist
    match fillCount > 0 with
    | true ->
      match fillContentTotal <= remaining with
      | true ->
        // Enough space: each fill participant gets content size + equal share of excess
        let excess = remaining - fillContentTotal
        let perExtra = excess / fillCount
        let mutable extraRem = excess % fillCount
        constraints |> List.iteri (fun i c ->
          match c with
          | Fill ->
            let bonus = match extraRem > 0 with | true -> extraRem <- extraRem - 1; 1 | false -> 0
            sizes[i] <- contentArr[i] + perExtra + bonus
          | Min _ when not hasFill ->
            let bonus = match extraRem > 0 with | true -> extraRem <- extraRem - 1; 1 | false -> 0
            let extra = max 0 (contentArr[i] - sizes[i])
            sizes[i] <- sizes[i] + extra + perExtra + bonus - extra  // grow to at least content, then share excess
          | _ -> ())
      | false ->
        // Not enough space: proportional shrink (flex-shrink)
        let fillItems =
          constraints |> List.mapi (fun i c ->
            match c with
            | Fill -> Some(i, contentArr[i])
            | Min _ when not hasFill -> Some(i, max 0 (contentArr[i] - sizes[i]))
            | _ -> None)
          |> List.choose id
        let totalContent = fillItems |> List.sumBy snd
        match totalContent with
        | 0 -> ()
        | _ ->
          let pool = max 0 remaining
          let sized = fillItems |> List.map (fun (i, c) -> (i, c * pool / totalContent))
          let used = sized |> List.sumBy snd
          let mutable rem = pool - used
          sized |> List.iter (fun (i, s) ->
            let bonus = match rem > 0 with | true -> rem <- rem - 1; 1 | false -> 0
            sizes[i] <- sizes[i] + s + bonus)
    | false -> ()

    let mutable offset = 0
    [ for i in 0 .. n - 1 do
        let result = (offset, sizes[i])
        offset <- offset + sizes[i]
        yield result ]

  let splitH (constraints: Constraint list) (area: Area) : Area list =
    solve area.Width constraints
    |> List.map (fun (offset, width) ->
      { X = area.X + offset; Y = area.Y; Width = width; Height = area.Height })

  let splitV (constraints: Constraint list) (area: Area) : Area list =
    solve area.Height constraints
    |> List.map (fun (offset, height) ->
      { X = area.X; Y = area.Y + offset; Width = area.Width; Height = height })

  let splitHWithContent (constraints: Constraint list) (contentWidths: int list) (area: Area) : Area list =
    solveWithContent area.Width constraints contentWidths
    |> List.map (fun (offset, width) ->
      { X = area.X + offset; Y = area.Y; Width = width; Height = area.Height })

  let splitVWithContent (constraints: Constraint list) (contentHeights: int list) (area: Area) : Area list =
    solveWithContent area.Height constraints contentHeights
    |> List.map (fun (offset, height) ->
      { X = area.X; Y = area.Y + offset; Width = area.Width; Height = height })

  let applyConstraint (c: Constraint) (area: Area) =
    match c with
    | Fixed n -> { area with Width = min n area.Width }
    | Min n -> { area with Width = max n area.Width }
    | Max n -> { area with Width = min n area.Width }
    | Percentage pct -> { area with Width = area.Width * pct / 100 }
    | Fill -> area
    | Ratio(num, den) ->
      match den > 0 with
      | true -> { area with Width = area.Width * num / den }
      | false -> area

  let applyConstraintV (c: Constraint) (area: Area) =
    match c with
    | Fixed n -> { area with Height = min n area.Height }
    | Min n -> { area with Height = max n area.Height }
    | Max n -> { area with Height = min n area.Height }
    | Percentage pct -> { area with Height = area.Height * pct / 100 }
    | Fill -> area
    | Ratio(num, den) ->
      match den > 0 with
      | true -> { area with Height = area.Height * num / den }
      | false -> area

  let shrinkForBorder (area: Area) =
    { X = area.X + 1; Y = area.Y + 1
      Width = max 0 (area.Width - 2); Height = max 0 (area.Height - 2) }

  let alignArea (hAlign: HAlign) (vAlign: VAlign) (contentW: int) (contentH: int) (area: Area) : Area =
    let x =
      match hAlign with
      | HAlign.Left -> area.X
      | HAlign.HCenter -> area.X + max 0 (area.Width - contentW) / 2
      | HAlign.Right -> area.X + max 0 (area.Width - contentW)
    let y =
      match vAlign with
      | VAlign.Top -> area.Y
      | VAlign.VCenter -> area.Y + max 0 (area.Height - contentH) / 2
      | VAlign.Bottom -> area.Y + max 0 (area.Height - contentH)
    let w = min contentW area.Width
    let h = min contentH area.Height
    { X = max area.X x; Y = max area.Y y; Width = w; Height = h }

  let intersectArea (parent: Area) (child: Area) : Area =
    let x1 = max parent.X child.X
    let y1 = max parent.Y child.Y
    let x2 = min (parent.X + parent.Width) (child.X + child.Width)
    let y2 = min (parent.Y + parent.Height) (child.Y + child.Height)
    { X = x1; Y = y1; Width = max 0 (x2 - x1); Height = max 0 (y2 - y1) }

  let solveWithGap (gap: int) (available: int) (constraints: Constraint list) (contentSizes: int list) : (int * int) list =
    let n = List.length constraints
    match n <= 1 with
    | true -> solveWithContent available constraints contentSizes
    | false ->
      let totalGap = gap * (n - 1)
      let usable = max 0 (available - totalGap)
      let solved = solveWithContent usable constraints contentSizes
      solved |> List.mapi (fun i (offset, size) ->
        (offset + i * gap, size))

  let splitHWithGap (gap: int) (constraints: Constraint list) (contentWidths: int list) (area: Area) : Area list =
    solveWithGap gap area.Width constraints contentWidths
    |> List.map (fun (offset, width) ->
      { X = area.X + offset; Y = area.Y; Width = width; Height = area.Height })

  let splitVWithGap (gap: int) (constraints: Constraint list) (contentHeights: int list) (area: Area) : Area list =
    solveWithGap gap area.Height constraints contentHeights
    |> List.map (fun (offset, height) ->
      { X = area.X; Y = area.Y + offset; Width = area.Width; Height = height })
