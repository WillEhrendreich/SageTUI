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
