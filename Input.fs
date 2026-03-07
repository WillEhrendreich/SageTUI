namespace SageTUI

open System

[<Flags>]
type Modifiers =
  | None = 0
  | Shift = 1
  | Alt = 2
  | Ctrl = 4
  | Meta = 8

type Key =
  | Char of char
  | Enter | Escape | Backspace | Tab
  | Up | Down | Left | Right
  | Home | End | PageUp | PageDown
  | Insert | Delete
  | F of int

type MouseButton = LeftButton | RightButton | MiddleButton | ScrollUp | ScrollDown

type MouseEvent =
  { Button: MouseButton; X: int; Y: int; Modifiers: Modifiers }

type TerminalEvent =
  | KeyPressed of key: Key * modifiers: Modifiers
  | MouseInput of MouseEvent
  | Resized of width: int * height: int
  | FocusGained
  | FocusLost
  | Pasted of string
