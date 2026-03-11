namespace SageTUI

type ColorCapability =
  | NoColor = 0
  | Basic16 = 1
  | Indexed256 = 2
  | TrueColor = 3

type UnicodeCapability =
  | AsciiOnly = 0
  | Latin1 = 1
  | UnicodeBmp = 2
  | FullUnicode = 3

type GraphicsCapability =
  | TextOnly = 0
  | HalfBlock = 1
  | Braille = 2
  | Sixel = 3
  | KittyGraphics = 4
  | ITermInline = 5

type InputCapability =
  | BasicKeys = 0
  | FunctionKeys = 1
  | MouseX11 = 2
  | MouseSgr = 3
  | BracketedPaste = 4
  | KittyKeyboard = 5

type OutputCapability =
  | LineBuffered = 0
  | RawMode = 1
  | AltScreen = 2
  | SynchronizedOutput = 3

type Platform = Windows | MacOS | Linux

type TerminalProfile = {
  Color: ColorCapability
  Unicode: UnicodeCapability
  Graphics: GraphicsCapability
  Input: InputCapability
  Output: OutputCapability
  Size: int * int
  TermName: string
  Platform: Platform
  SupportsOsc8: bool
}
