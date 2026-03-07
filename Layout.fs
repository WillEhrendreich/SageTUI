namespace SageTUI

type Area = { X: int; Y: int; Width: int; Height: int }

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
        sizes[i] <- minSize
        remaining <- remaining - sizes[i]
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
        | _ -> ())
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
