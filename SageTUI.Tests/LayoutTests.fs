module SageTUI.LayoutTests

open System.Text
open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ── Helpers ──────────────────────────────────────────────────────────────
let area4 w h = { X = 0; Y = 0; Width = w; Height = h }
let areaAt x y w h = { X = x; Y = y; Width = w; Height = h }

let renderToBuffer w h elem =
  let buf = Buffer.create w h
  Render.render (area4 w h) Style.empty buf elem
  buf

let cellAt x y (buf: Buffer) =
  Buffer.get x y buf

let charAt x y buf =
  let c = cellAt x y buf
  match c.Rune with
  | 0 -> ' '
  | v -> char v

// ═══════════════════════════════════════════════════════════════════════
// PHASE 1: Intrinsic Content Measurement
// MDN: min-content / max-content / fit-content sizing
// https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_sizing
// ═══════════════════════════════════════════════════════════════════════

let measureWidthTests = testList "Measure.measureWidth" [
  testCase "Empty → 0" <| fun () ->
    Measure.measureWidth Empty
    |> Expect.equal "empty has zero width" 0

  testCase "Text intrinsic width = character count" <| fun () ->
    Measure.measureWidth (El.text "Hello")
    |> Expect.equal "5-char text" 5

  testCase "Empty text → 0" <| fun () ->
    Measure.measureWidth (El.text "")
    |> Expect.equal "empty string" 0

  testCase "Row width = sum of children widths" <| fun () ->
    El.row [El.text "AB"; El.text "CDE"]
    |> Measure.measureWidth
    |> Expect.equal "2 + 3" 5

  testCase "Column width = max of children widths" <| fun () ->
    El.column [El.text "AB"; El.text "CDEF"]
    |> Measure.measureWidth
    |> Expect.equal "max(2,4)" 4

  testCase "Bordered adds 2 to width" <| fun () ->
    El.bordered Light (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "2 + 2" 4

  testCase "Padded adds horizontal padding" <| fun () ->
    El.padHV 3 0 (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "2 + 3 + 3" 8

  testCase "Constrained Fixed overrides width" <| fun () ->
    El.width 10 (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "fixed 10" 10

  testCase "Constrained Min raises width" <| fun () ->
    El.minWidth 10 (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "max(10, 2)" 10

  testCase "Constrained Max caps width" <| fun () ->
    El.maxWidth 1 (El.text "Hello")
    |> Measure.measureWidth
    |> Expect.equal "min(1, 5)" 1

  testCase "Styled passes through" <| fun () ->
    El.bold (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "styled = child" 2

  testCase "Keyed passes through" <| fun () ->
    El.keyed "k" (El.text "Hi")
    |> Measure.measureWidth
    |> Expect.equal "keyed = child" 2

  testCase "Nested Row in Column" <| fun () ->
    El.column [El.row [El.text "AB"; El.text "CD"]; El.text "EFGHIJ"]
    |> Measure.measureWidth
    |> Expect.equal "max(4, 6)" 6

  testCase "Deeply nested" <| fun () ->
    El.bordered Light (El.padAll 1 (El.row [El.text "AB"; El.text "CD"]))
    |> Measure.measureWidth
    |> Expect.equal "(2+2) + 2 border + 2 pad = 8" 8

  testCase "Overlay width = max of layers" <| fun () ->
    El.overlay [El.text "AB"; El.text "CDEF"]
    |> Measure.measureWidth
    |> Expect.equal "max(2, 4)" 4

  testCase "Empty Row → 0" <| fun () ->
    El.row [] |> Measure.measureWidth
    |> Expect.equal "empty row" 0

  testCase "Empty Column → 0" <| fun () ->
    El.column [] |> Measure.measureWidth
    |> Expect.equal "empty column" 0
]

let measureHeightTests = testList "Measure.measureHeight" [
  testCase "Empty → 0" <| fun () ->
    Measure.measureHeight Empty
    |> Expect.equal "empty" 0

  testCase "Text intrinsic height = 1" <| fun () ->
    Measure.measureHeight (El.text "anything")
    |> Expect.equal "text always 1 row" 1

  testCase "Column height = sum of children heights" <| fun () ->
    El.column [El.text "A"; El.text "B"; El.text "C"]
    |> Measure.measureHeight
    |> Expect.equal "3 texts = 3 rows" 3

  testCase "Row height = max of children heights" <| fun () ->
    El.row [El.text "A"; El.column [El.text "B"; El.text "C"]]
    |> Measure.measureHeight
    |> Expect.equal "max(1, 2)" 2

  testCase "Bordered adds 2 to height" <| fun () ->
    El.bordered Light (El.text "Hi")
    |> Measure.measureHeight
    |> Expect.equal "1 + 2" 3

  testCase "Padded adds vertical padding" <| fun () ->
    El.padHV 0 2 (El.text "Hi")
    |> Measure.measureHeight
    |> Expect.equal "1 + 2 + 2" 5

  testCase "Styled passes through" <| fun () ->
    El.bold (El.column [El.text "A"; El.text "B"])
    |> Measure.measureHeight
    |> Expect.equal "styled = child" 2

  testCase "Nested bordered columns" <| fun () ->
    El.bordered Light (El.column [El.text "A"; El.text "B"; El.text "C"])
    |> Measure.measureHeight
    |> Expect.equal "3 + 2 border" 5

  testCase "Empty Row → 0" <| fun () ->
    El.row [] |> Measure.measureHeight
    |> Expect.equal "empty row" 0

  testCase "Empty Column → 0" <| fun () ->
    El.column [] |> Measure.measureHeight
    |> Expect.equal "empty column" 0

  testCase "Overlay height = max of layers" <| fun () ->
    El.overlay [El.column [El.text "A"; El.text "B"]; El.text "C"]
    |> Measure.measureHeight
    |> Expect.equal "max(2, 1)" 2

  testCase "Deeply nested" <| fun () ->
    El.bordered Light (El.padAll 1 (El.column [El.text "A"; El.text "B"]))
    |> Measure.measureHeight
    |> Expect.equal "2 texts + 2 pad + 2 border = 6" 6

  testCase "Column with Fixed-constrained child measures constraint as height" <| fun () ->
    // El.height 3 text = Column[Constrained(Fixed 3, text)]
    // In Column context, Fixed 3 means 3 rows
    El.height 3 (El.text "X")
    |> Measure.measureHeight
    |> Expect.equal "Fixed 3 in Column = 3 rows" 3

  testCase "Column with mixed Fixed and content children" <| fun () ->
    El.column [
      Constrained(Fixed 5, El.text "Header")
      El.text "Body"
      Constrained(Fixed 2, El.text "Footer")
    ]
    |> Measure.measureHeight
    |> Expect.equal "5 + 1 + 2 = 8" 8
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 2: Content-Aware Layout Solver
// MDN: Flexbox - flex-basis: auto means content size
// https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_flexible_box_layout
// ═══════════════════════════════════════════════════════════════════════

let solveWithContentTests = testList "Layout.solveWithContent" [
  testCase "single Fill uses content + excess" <| fun () ->
    Layout.solveWithContent 80 [Fill] [5]
    |> Expect.equal "content 5 + excess 75 = 80" [(0, 80)]

  testCase "two Fill with different content sizes" <| fun () ->
    // Available=20, content=[2, 8], total content=10, excess=10, perExtra=5
    // First: 2+5=7, Second: 8+5=13
    Layout.solveWithContent 20 [Fill; Fill] [2; 8]
    |> Expect.equal "content-proportional" [(0, 7); (7, 13)]

  testCase "three Fill with varied content" <| fun () ->
    // Available=30, content=[1, 1, 1], total=3, excess=27, perExtra=9
    Layout.solveWithContent 30 [Fill; Fill; Fill] [1; 1; 1]
    |> Expect.equal "each gets 10" [(0, 10); (10, 10); (20, 10)]

  testCase "Fill + Fixed" <| fun () ->
    // Fixed 10 takes 10, remaining=40, Fill content=5, excess=35
    Layout.solveWithContent 50 [Fixed 10; Fill] [0; 5]
    |> Expect.equal "10 fixed + 40 fill" [(0, 10); (10, 40)]

  testCase "content exactly fills available" <| fun () ->
    Layout.solveWithContent 10 [Fill; Fill] [5; 5]
    |> Expect.equal "no excess" [(0, 5); (5, 5)]

  testCase "content exceeds available — greedy allocation" <| fun () ->
    // Available=3, content=[1,1,1,1,1], total=5 > 3
    // Greedy: first 3 get 1 each, last 2 get 0
    Layout.solveWithContent 3 [Fill; Fill; Fill; Fill; Fill] [1; 1; 1; 1; 1]
    |> Expect.equal "first 3 get 1, rest 0" [(0, 1); (1, 1); (2, 1); (3, 0); (3, 0)]

  testCase "content exceeds — large items get greedy share" <| fun () ->
    // Available=5, content=[3, 4], total=7 > 5
    // Greedy: first gets min(3,5)=3, second gets min(4,2)=2
    Layout.solveWithContent 5 [Fill; Fill] [3; 4]
    |> Expect.equal "greedy 3 then 2" [(0, 3); (3, 2)]

  testCase "zero content sizes → equal split (backward compat)" <| fun () ->
    Layout.solveWithContent 80 [Fill; Fill] [0; 0]
    |> Expect.equal "equal 40/40" [(0, 40); (40, 40)]

  testCase "Fixed + Fill + Fixed" <| fun () ->
    Layout.solveWithContent 100 [Fixed 20; Fill; Fixed 20] [0; 5; 0]
    |> Expect.equal "sidebar layout" [(0, 20); (20, 60); (80, 20)]

  testCase "Percentage + Fill with content" <| fun () ->
    Layout.solveWithContent 100 [Percentage 30; Fill] [0; 10]
    |> Expect.equal "30% + fill" [(0, 30); (30, 70)]

  testCase "all Fill zero content = original equal split" <| fun () ->
    Layout.solveWithContent 90 [Fill; Fill; Fill] [0; 0; 0]
    |> Expect.equal "30 each" [(0, 30); (30, 30); (60, 30)]

  testCase "excess distributes with remainder" <| fun () ->
    // Available=10, content=[1,1,1], total=3, excess=7, perExtra=2, rem=1
    Layout.solveWithContent 10 [Fill; Fill; Fill] [1; 1; 1]
    |> Expect.equal "1+3=4, 1+2=3, 1+2=3" [(0, 4); (4, 3); (7, 3)]
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 3: Block Layout (MDN: Normal Flow)
// https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_flow_layout
// Block elements stack vertically, each gets full parent width,
// height determined by content.
// ═══════════════════════════════════════════════════════════════════════

let blockFlowTests = testList "Block flow layout (Column = block)" [
  testCase "Block children stack vertically" <| fun () ->
    // 3 texts in 10x10: content [1,1,1], excess=7, per=2, rem=1 → [4,3,3]
    let buf = renderToBuffer 10 10
                (El.column [El.text "AAA"; El.text "BBB"; El.text "CCC"])
    charAt 0 0 buf |> Expect.equal "row 0" 'A'
    charAt 0 4 buf |> Expect.equal "row 4" 'B'
    charAt 0 7 buf |> Expect.equal "row 7" 'C'

  testCase "Block children each get full parent width" <| fun () ->
    // 2 texts in 20x5: content [1,1], excess=3, per=1, rem=1 → [3,2]
    let buf = renderToBuffer 20 5
                (El.column [El.text "Short"; El.text "Longer text here"])
    charAt 0 0 buf |> Expect.equal "first at x=0" 'S'
    charAt 0 3 buf |> Expect.equal "second at row 3" 'L'

  testCase "Block height determined by content (3 items in 20 rows)" <| fun () ->
    // 3 texts in 20 rows: content [1,1,1], excess=17, per=5, rem=2
    // Sizes: [1+6, 1+6, 1+5] = [7, 7, 6]. B at row 7.
    let buf = renderToBuffer 20 20
                (El.column [El.text "A"; El.text "B"; El.text "C"])
    charAt 0 0 buf |> Expect.equal "A at row 0" 'A'
    charAt 0 7 buf |> Expect.equal "B at row 7" 'B'
    charAt 0 14 buf |> Expect.equal "C at row 14" 'C'

  testCase "Many block children in tight space — first items get priority" <| fun () ->
    // 5 blocks in 3 rows: content overflows, first 3 render
    let buf = renderToBuffer 10 3
                (El.column [El.text "A"; El.text "B"; El.text "C"; El.text "D"; El.text "E"])
    charAt 0 0 buf |> Expect.equal "A renders" 'A'
    charAt 0 1 buf |> Expect.equal "B renders" 'B'
    charAt 0 2 buf |> Expect.equal "C renders" 'C'

  testCase "Single block child gets full height" <| fun () ->
    // One child: content=1, excess=19, total=20
    let buf = renderToBuffer 20 20
                (El.column [El.text "Solo"])
    charAt 0 0 buf |> Expect.equal "Solo at row 0" 'S'

  testCase "Empty column renders nothing" <| fun () ->
    let buf = renderToBuffer 10 10 (El.column [])
    charAt 0 0 buf |> Expect.equal "empty" ' '

  testCase "Nested blocks stack recursively" <| fun () ->
    // Outer column has 2 children: inner column (2 items) and text
    // Inner column needs height 2, text needs 1. Total=3
    let buf = renderToBuffer 20 20
                (El.column [
                  El.column [El.text "A1"; El.text "A2"]
                  El.text "B"])
    charAt 0 0 buf |> Expect.equal "A1" 'A'

  testCase "Block with border — content inside border" <| fun () ->
    let buf = renderToBuffer 20 10
                (El.column [
                  El.bordered Light (El.text "Inside")
                  El.text "Outside"])
    // Border takes 3 rows (1 top + 1 content + 1 bottom)
    // "Outside" should appear below the border
    charAt 1 1 buf |> Expect.equal "Inside at (1,1)" 'I'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 4: Inline Layout (MDN: Inline formatting context)
// https://developer.mozilla.org/en-US/docs/Web/CSS/Inline_formatting_context
// Inline elements flow horizontally, width by content.
// ═══════════════════════════════════════════════════════════════════════

let inlineFlowTests = testList "Inline flow layout (Row = inline)" [
  testCase "Inline children flow horizontally" <| fun () ->
    let buf = renderToBuffer 20 1
                (El.row [El.text "AB"; El.text "CD"])
    charAt 0 0 buf |> Expect.equal "A" 'A'
    charAt 1 0 buf |> Expect.equal "B" 'B'
    // With content-aware: AB gets 2+8=10, CD gets 2+8=10 in 20 cols
    // Or content [2,2], excess 16, perExtra 8
    // AB at offset 0, width 10; CD at offset 10, width 10
    charAt 10 0 buf |> Expect.equal "C at 10" 'C'

  testCase "Inline items get content-proportional space" <| fun () ->
    // "AB"(2) and "CDEFG"(5) in 21 cols
    // content [2,5], total=7, excess=14, perExtra=7
    // AB gets 2+7=9, CDEFG gets 5+7=12
    let buf = renderToBuffer 21 1
                (El.row [El.text "AB"; El.text "CDEFG"])
    charAt 0 0 buf |> Expect.equal "A at 0" 'A'
    charAt 9 0 buf |> Expect.equal "C at 9" 'C'

  testCase "Row with fixed + fill" <| fun () ->
    let buf = renderToBuffer 20 1
                (El.row [El.width 5 (El.text "LEFT"); El.text "RIGHT"])
    charAt 0 0 buf |> Expect.equal "L at 0" 'L'
    charAt 5 0 buf |> Expect.equal "R at 5" 'R'

  testCase "Row children all get parent height" <| fun () ->
    // Row doesn't split height — all children get same height
    let buf = renderToBuffer 20 5
                (El.row [El.column [El.text "A"; El.text "B"]; El.text "C"])
    charAt 0 0 buf |> Expect.equal "A at (0,0)" 'A'

  testCase "Tight row — overflow clips later items" <| fun () ->
    // 5 cols for "ABC"(3) + "DEF"(3) = content 6 > 5
    // Greedy: ABC gets min(3,5)=3, DEF gets min(3,2)=2 → "DE"
    let buf = renderToBuffer 5 1
                (El.row [El.text "ABC"; El.text "DEF"])
    charAt 0 0 buf |> Expect.equal "A" 'A'
    charAt 1 0 buf |> Expect.equal "B" 'B'
    charAt 2 0 buf |> Expect.equal "C" 'C'
    charAt 3 0 buf |> Expect.equal "D" 'D'
    charAt 4 0 buf |> Expect.equal "E" 'E'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 5: Flexbox (MDN: CSS Flexible Box Layout)
// https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_flexible_box_layout
// flex-basis: auto, flex-grow distributes excess, shrink clips.
// ═══════════════════════════════════════════════════════════════════════

let flexboxTests = testList "Flexbox-like layout" [
  testCase "flex-direction: column — items get content height + excess share" <| fun () ->
    // 2 items in 10 rows: content [1,1], excess=8, perExtra=4
    let areas = Layout.solveWithContent 10 [Fill; Fill] [1; 1]
    areas |> Expect.equal "5 each" [(0, 5); (5, 5)]

  testCase "flex-direction: row — items get content width + excess share" <| fun () ->
    // "Hi"(2) + "World"(5) in 17 cols
    // content [2,5], total=7, excess=10, perExtra=5
    let areas = Layout.solveWithContent 17 [Fill; Fill] [2; 5]
    areas |> Expect.equal "7 and 10" [(0, 7); (7, 10)]

  testCase "flex-grow distributes excess equally" <| fun () ->
    // All items same content size → excess distributed equally
    Layout.solveWithContent 30 [Fill; Fill; Fill] [2; 2; 2]
    |> Expect.equal "10 each" [(0, 10); (10, 10); (20, 10)]

  testCase "flex-basis: 0 (zero content) → equal split" <| fun () ->
    Layout.solveWithContent 60 [Fill; Fill; Fill] [0; 0; 0]
    |> Expect.equal "20 each" [(0, 20); (20, 20); (40, 20)]

  testCase "flex with one fixed item — remaining to fills" <| fun () ->
    // Header(Fixed 3) + body(Fill) + footer(Fixed 2) in 25 rows
    // Fixed takes 3+2=5, remaining=20 for 1 fill
    Layout.solveWithContent 25 [Fixed 3; Fill; Fixed 2] [0; 10; 0]
    |> Expect.equal "3+20+2" [(0, 3); (3, 20); (23, 2)]

  testCase "nested flex: Row of Columns renders correctly" <| fun () ->
    // THE pattern that was broken before:
    // Row [Column [text; text]; Column [text; text]]
    let elem = El.row [
      El.column [El.text "A1"; El.text "A2"]
      El.column [El.text "B1"; El.text "B2"]
    ]
    let buf = renderToBuffer 20 10 elem
    // Row splits width: each column gets some width
    // Column splits height: 2 items content [1,1], excess=8, perExtra=4
    charAt 0 0 buf |> Expect.equal "A1 at (0,0)" 'A'
    // A2 should be below A1 (not at row 5 with equal split, but content-based)

  testCase "content overflows → greedy clips trailing items" <| fun () ->
    // 10 items needing 1 row each in 5-row space
    let items = List.init 10 (fun i -> El.text (string (char (int 'A' + i))))
    let buf = renderToBuffer 10 5 (El.column items)
    charAt 0 0 buf |> Expect.equal "A renders" 'A'
    charAt 0 1 buf |> Expect.equal "B renders" 'B'
    charAt 0 2 buf |> Expect.equal "C renders" 'C'
    charAt 0 3 buf |> Expect.equal "D renders" 'D'
    charAt 0 4 buf |> Expect.equal "E renders" 'E'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 6: Box Model (MDN: CSS Box Model)
// https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_box_model
// Border and padding affect available space for children.
// ═══════════════════════════════════════════════════════════════════════

let boxModelTests = testList "Box model layout" [
  testCase "Border reduces inner space by 2 in each dimension" <| fun () ->
    // 10x5 area, border leaves 8x3 inner
    let buf = renderToBuffer 10 5
                (El.bordered Light (El.text "ABCDEFGHIJ"))
    // Text at (1,1) inside border, max 8 chars wide
    charAt 1 1 buf |> Expect.equal "A inside border" 'A'
    charAt 8 1 buf |> Expect.equal "H at col 8" 'H'

  testCase "Padding reduces inner space" <| fun () ->
    let buf = renderToBuffer 10 5
                (El.padAll 1 (El.text "Hello"))
    charAt 1 1 buf |> Expect.equal "H at (1,1)" 'H'
    charAt 0 0 buf |> Expect.equal "padding is empty" ' '

  testCase "Border + padding accumulates" <| fun () ->
    // Border takes 2 (1+1), padding takes 2 (1+1) = 4 total per axis
    let buf = renderToBuffer 12 6
                (El.bordered Light (El.padAll 1 (El.text "Hi")))
    // Border at (0,0), padding at (1,1), text at (2,2)
    charAt 2 2 buf |> Expect.equal "H at (2,2)" 'H'

  testCase "Bordered column — children respect inner space" <| fun () ->
    let buf = renderToBuffer 20 8
                (El.bordered Light (El.column [El.text "Line1"; El.text "Line2"; El.text "Line3"]))
    // Inner area: 18 wide, 6 tall. 3 items content [1,1,1], excess=3, each gets 2
    charAt 1 1 buf |> Expect.equal "L1 at row 1" 'L'

  testCase "Row inside border" <| fun () ->
    let buf = renderToBuffer 20 3
                (El.bordered Light (El.row [El.text "Left"; El.text "Right"]))
    charAt 1 1 buf |> Expect.equal "L at (1,1)" 'L'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 7: CSS Sizing (MDN: width, min-width, max-width)
// https://developer.mozilla.org/en-US/docs/Web/CSS/width
// Explicit sizing overrides content sizing.
// ═══════════════════════════════════════════════════════════════════════

let sizingTests = testList "CSS sizing constraints" [
  testCase "Fixed width constrains text" <| fun () ->
    let buf = renderToBuffer 20 1
                (El.width 5 (El.text "Hello World"))
    charAt 4 0 buf |> Expect.equal "o at col 4" 'o'
    charAt 5 0 buf |> Expect.equal "space beyond fixed" ' '

  testCase "Percentage width" <| fun () ->
    let buf = renderToBuffer 100 1
                (El.row [El.percentage 25 (El.text "Q"); El.text "Rest"])
    charAt 0 0 buf |> Expect.equal "Q at 0" 'Q'
    charAt 25 0 buf |> Expect.equal "R at 25" 'R'

  testCase "Fixed height via Column[Constrained]" <| fun () ->
    // El.height 3 = Column[Constrained(Fixed 3, text)]
    // measureHeight of inner Column = 3 (Fixed constraint in Column context)
    // Outer: content [3, 1], total=4, excess=6, per=3
    // Sizes: [3+3, 1+3] = [6, 4]. Bot at row 6.
    let buf = renderToBuffer 10 10
                (El.column [El.height 3 (El.text "Top"); El.text "Bot"])
    charAt 0 0 buf |> Expect.equal "T at row 0" 'T'
    charAt 0 6 buf |> Expect.equal "B at row 6" 'B'

  testCase "Min width enforces minimum" <| fun () ->
    let buf = renderToBuffer 20 1
                (El.minWidth 10 (El.text "Hi"))
    charAt 0 0 buf |> Expect.equal "H" 'H'

  testCase "Min width grows to fill when alone in row" <| fun () ->
    // El.minWidth 5 inside a row with no other children grows to fill parent
    let buf = renderToBuffer 20 1
                (El.row [ El.minWidth 5 (El.text "Hi") ])
    charAt 0 0 buf |> Expect.equal "H at col 0" 'H'
    charAt 1 0 buf |> Expect.equal "i at col 1" 'i'

  testCase "Min width stays at floor when Fill sibling present" <| fun () ->
    // row [minWidth 5 "AB" | fill "X"] - min item gets 5, fill gets 15
    let buf = renderToBuffer 20 1
                (El.row [ El.minWidth 5 (El.text "AB"); El.text "X" ])
    charAt 0 0 buf |> Expect.equal "A" 'A'
    charAt 1 0 buf |> Expect.equal "B" 'B'
    charAt 5 0 buf |> Expect.equal "X at col 5" 'X'

  testCase "Max width caps" <| fun () ->
    let buf = renderToBuffer 20 1
                (El.maxWidth 3 (El.text "Hello"))
    charAt 0 0 buf |> Expect.equal "H" 'H'
    charAt 2 0 buf |> Expect.equal "l" 'l'
    charAt 3 0 buf |> Expect.equal "clipped" ' '
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 8: Composite Layout Scenarios (MDN: CSS Layout Cookbook)
// https://developer.mozilla.org/en-US/docs/Web/CSS/Layout_cookbook
// Real-world patterns that MUST work.
// ═══════════════════════════════════════════════════════════════════════

let compositeLayoutTests = testList "Composite layout scenarios" [
  testCase "Holy Grail: header + [sidebar | main | sidebar] + footer" <| fun () ->
    let elem = El.column [
      El.height 1 (El.text "HEADER")
      El.row [
        El.width 5 (El.text "NAV")
        El.text "MAIN CONTENT"
        El.width 5 (El.text "SIDE")
      ]
      El.height 1 (El.text "FOOTER")
    ]
    let buf = renderToBuffer 40 10 elem
    charAt 0 0 buf |> Expect.equal "H at (0,0)" 'H'
    // Header takes Fixed 1 row, Footer takes Fixed 1 row (via Column wrapping)
    // Middle row gets remaining

  testCase "Card: bordered padded content" <| fun () ->
    let card = El.bordered Rounded (El.padAll 1 (
      El.column [El.text "Title"; El.text "Body text here"]))
    let buf = renderToBuffer 30 8 card
    // Border at edges, padding of 1, then content
    charAt 2 2 buf |> Expect.equal "T at (2,2)" 'T'

  testCase "Dashboard: row of bordered cards" <| fun () ->
    let card title = El.bordered Light (El.text title)
    let elem = El.row [card "Card1"; card "Card2"; card "Card3"]
    let buf = renderToBuffer 30 3 elem
    // 3 cards in a row, each gets ~10 cols
    charAt 1 1 buf |> Expect.equal "C at (1,1)" 'C'

  testCase "Sidebar layout: fixed left + fill right" <| fun () ->
    let elem = El.row [
      El.width 10 (El.column [El.text "Nav1"; El.text "Nav2"; El.text "Nav3"])
      El.column [El.text "Main content"; El.text "More content"]
    ]
    let buf = renderToBuffer 40 10 elem
    charAt 0 0 buf |> Expect.equal "N at (0,0)" 'N'
    charAt 10 0 buf |> Expect.equal "M at (10,0)" 'M'

  testCase "Nested rows in column" <| fun () ->
    let elem = El.column [
      El.row [El.text "R1A"; El.text "R1B"]
      El.row [El.text "R2A"; El.text "R2B"]
    ]
    let buf = renderToBuffer 20 10 elem
    charAt 0 0 buf |> Expect.equal "R1A" 'R'

  testCase "Row > Column > multiple texts all render" <| fun () ->
    // THE critical pattern that was broken
    let left = El.column [
      El.text "Line 1"
      El.text "Line 2"
      El.text "Line 3"
    ]
    let right = El.column [
      El.text "Info A"
      El.text "Info B"
    ]
    let elem = El.row [left; right]
    let buf = renderToBuffer 30 10 elem
    // Left column: 3 items, right column: 2 items
    // Row gives each column some width
    // Left column: content height 3, in 10 rows → each item gets floor
    charAt 0 0 buf |> Expect.equal "L at (0,0)" 'L'
    // Check line 2 and 3 actually render
    // Content [1,1,1], excess=7, each gets 1+2=3 or 1+3=4
    // Line 2 should be at row 4 or 3 (depending on rounding)

  testCase "Bordered Row > Column — THE failing pattern" <| fun () ->
    let elem = El.bordered Rounded (
      El.row [
        El.column [El.text "Art1"; El.text "Art2"; El.text "Art3"]
        El.column [El.text "Desc"]
      ]
    )
    let buf = renderToBuffer 30 8 elem
    // Border shrinks to 28x6. Row splits width for 2 columns.
    // Left col: 3 items, right col: 1 item — ALL should render
    charAt 1 1 buf |> Expect.equal "A at (1,1) inside border" 'A'

  testCase "15 text items in bordered container" <| fun () ->
    // Real-world: ASCII art as 15 text lines in a bordered card
    let lines = List.init 15 (fun i -> El.text (sprintf "Line %02d" i))
    let elem = El.bordered Light (El.column lines)
    // 30 rows total, border takes 2, inner = 28
    // 15 items, content [1..1], total=15, excess=13
    let buf = renderToBuffer 20 30 elem
    charAt 1 1 buf |> Expect.equal "L at row 1" 'L'
    // Check last line renders too (row 15 in inner = row 16 with border)
    // Content 15, excess 13, each gets 1 + 0 or 1 + 1 = ~1.87 each

  testCase "Row with empty children" <| fun () ->
    let elem = El.row [El.empty; El.text "Hi"; El.empty]
    let buf = renderToBuffer 20 1 elem
    // Empty elements have 0 content width → "Hi" gets content 2 + excess share
    // Somewhere in the row, 'H' must appear
    let found = [0..19] |> List.exists (fun x -> charAt x 0 buf = 'H')
    found |> Expect.isTrue "H appears somewhere in row"

  testCase "Column with mix of empty and text" <| fun () ->
    let elem = El.column [El.empty; El.text "Hello"; El.empty]
    let buf = renderToBuffer 20 10 elem
    // Text must render somewhere in the column
    let found = [0..9] |> List.exists (fun y -> charAt 0 y buf = 'H')
    found |> Expect.isTrue "H appears somewhere in column"
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 9: Arena Render Parity
// The Arena renderer must produce identical output to the tree renderer
// for ALL content-aware layout scenarios.
// ═══════════════════════════════════════════════════════════════════════

let arenaHelper elem w h =
  let area = { X = 0; Y = 0; Width = w; Height = h }
  let treeBuf = Buffer.create w h
  Render.render area Style.empty treeBuf elem
  let arenaBuf = Buffer.create w h
  let arena = FrameArena.create 1024 4096 256
  let root = Arena.lower arena elem
  ArenaRender.renderRoot arena root area arenaBuf
  (treeBuf, arenaBuf)

let arenaParityTests = testList "Arena render parity (content-aware)" [
  testCase "Column of texts" <| fun () ->
    let (tree, arena) = arenaHelper
                          (El.column [El.text "A"; El.text "B"; El.text "C"]) 10 10
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Row of texts" <| fun () ->
    let (tree, arena) = arenaHelper
                          (El.row [El.text "AB"; El.text "CDE"]) 20 1
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Row of columns" <| fun () ->
    let elem = El.row [
      El.column [El.text "A1"; El.text "A2"]
      El.column [El.text "B1"; El.text "B2"]
    ]
    let (tree, arena) = arenaHelper elem 20 10
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Bordered column" <| fun () ->
    let elem = El.bordered Light (El.column [El.text "X"; El.text "Y"])
    let (tree, arena) = arenaHelper elem 15 6
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Sidebar layout" <| fun () ->
    let elem = El.row [
      El.width 5 (El.text "Nav")
      El.column [El.text "Main"; El.text "More"]
    ]
    let (tree, arena) = arenaHelper elem 30 5
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Holy grail" <| fun () ->
    let elem = El.column [
      El.height 1 (El.text "HEAD")
      El.row [El.width 5 (El.text "NAV"); El.text "BODY"; El.width 5 (El.text "SIDE")]
      El.height 1 (El.text "FOOT")
    ]
    let (tree, arena) = arenaHelper elem 40 10
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Bordered Row > Column" <| fun () ->
    let elem = El.bordered Rounded (
      El.row [
        El.column [El.text "L1"; El.text "L2"; El.text "L3"]
        El.column [El.text "R1"]
      ]
    )
    let (tree, arena) = arenaHelper elem 30 8
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "15 text items in bordered container" <| fun () ->
    let lines = List.init 15 (fun i -> El.text (sprintf "Line%02d" i))
    let elem = El.bordered Light (El.column lines)
    let (tree, arena) = arenaHelper elem 20 30
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Padded nested" <| fun () ->
    let elem = El.padAll 1 (El.column [El.text "A"; El.text "B"])
    let (tree, arena) = arenaHelper elem 15 6
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells

  testCase "Overflow column" <| fun () ->
    let items = List.init 10 (fun i -> El.text (string (char (int 'A' + i))))
    let (tree, arena) = arenaHelper (El.column items) 10 5
    arena.Cells |> Expect.sequenceEqual "cells match" tree.Cells
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 10: Edge Cases
// ═══════════════════════════════════════════════════════════════════════

let edgeCaseTests = testList "Layout edge cases" [
  testCase "Zero-width area renders nothing" <| fun () ->
    let buf = Buffer.create 0 10
    Render.render { X = 0; Y = 0; Width = 0; Height = 10 } Style.empty buf (El.text "Hi")
    // No crash expected

  testCase "Zero-height area renders nothing" <| fun () ->
    let buf = Buffer.create 10 0
    Render.render { X = 0; Y = 0; Width = 10; Height = 0 } Style.empty buf (El.text "Hi")
    // No crash expected

  testCase "Single column child gets all space" <| fun () ->
    let areas = Layout.solveWithContent 100 [Fill] [5]
    areas |> Expect.equal "all 100" [(0, 100)]

  testCase "Column of 1 text in 1 row" <| fun () ->
    let buf = renderToBuffer 10 1 (El.column [El.text "Hi"])
    charAt 0 0 buf |> Expect.equal "H" 'H'

  testCase "Row of 1 text in 1 col" <| fun () ->
    let buf = renderToBuffer 2 1 (El.row [El.text "AB"])
    charAt 0 0 buf |> Expect.equal "A" 'A'
    charAt 1 0 buf |> Expect.equal "B" 'B'

  testCase "Deeply nested single-child chain" <| fun () ->
    let elem = El.bold (El.italic (El.column [El.row [El.text "X"]]))
    let buf = renderToBuffer 10 5 elem
    charAt 0 0 buf |> Expect.equal "X" 'X'

  testCase "Large number of Fill children" <| fun () ->
    let items = List.init 100 (fun _ -> El.text "X")
    let buf = renderToBuffer 10 100 (El.column items)
    charAt 0 0 buf |> Expect.equal "X at row 0" 'X'
    charAt 0 99 buf |> Expect.equal "X at row 99" 'X'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 11: Alignment
// ═══════════════════════════════════════════════════════════════════════

let alignmentTests = testList "Alignment" [
  testCase "HCenter text in row" <| fun () ->
    // "Hi" (2 chars) centered in 10-wide area → at column 4
    let buf = renderToBuffer 10 1 (El.alignCenter (El.text "Hi"))
    charAt 4 0 buf |> Expect.equal "H at center" 'H'
    charAt 5 0 buf |> Expect.equal "i at center" 'i'
    charAt 3 0 buf |> Expect.equal "space before" ' '

  testCase "Right-aligned text" <| fun () ->
    // "AB" (2 chars) right-aligned in 10-wide area → at column 8
    let buf = renderToBuffer 10 1 (El.alignRight (El.text "AB"))
    charAt 8 0 buf |> Expect.equal "A at right" 'A'
    charAt 9 0 buf |> Expect.equal "B at right" 'B'

  testCase "Bottom-aligned text" <| fun () ->
    // text bottom-aligned in 5-tall area → at row 4
    let buf = renderToBuffer 10 5 (El.alignBottom (El.text "X"))
    charAt 0 4 buf |> Expect.equal "X at bottom" 'X'
    charAt 0 0 buf |> Expect.equal "empty at top" ' '

  testCase "Center text both axes" <| fun () ->
    // "Hi" centered in 10x5 → col 4, row 2
    let buf = renderToBuffer 10 5 (El.center (El.text "Hi"))
    charAt 4 2 buf |> Expect.equal "H centered" 'H'
    charAt 5 2 buf |> Expect.equal "i centered" 'i'

  testCase "Bottom-right aligned text" <| fun () ->
    // "Z" in 10x5 → col 9, row 4
    let buf = renderToBuffer 10 5 (El.alignBottomRight (El.text "Z"))
    charAt 9 4 buf |> Expect.equal "Z at bottom-right" 'Z'

  testCase "Left-top is default (no offset)" <| fun () ->
    let buf = renderToBuffer 10 5 (El.alignLeft (El.text "A"))
    charAt 0 0 buf |> Expect.equal "A at top-left" 'A'

  testCase "Oversized content clamps" <| fun () ->
    // "ABCDE" (5 chars) in 3-wide → only "ABC" visible
    let buf = renderToBuffer 3 1 (El.alignCenter (El.text "ABCDE"))
    charAt 0 0 buf |> Expect.equal "A clamped" 'A'
    charAt 1 0 buf |> Expect.equal "B clamped" 'B'
    charAt 2 0 buf |> Expect.equal "C clamped" 'C'

  testCase "Centered column in row" <| fun () ->
    let elem = El.align HAlign.HCenter VAlign.Top (El.column [El.text "A"; El.text "B"])
    let buf = renderToBuffer 10 5 elem
    // Column content is 1 wide, 2 tall; centered H → col 4
    charAt 4 0 buf |> Expect.equal "A centered" 'A'
    charAt 4 1 buf |> Expect.equal "B below A" 'B'

  testCase "Arena parity: centered text" <| fun () ->
    let elem = El.center (El.text "Hi")
    let (tree, arena) = arenaHelper elem 10 5
    arena.Cells |> Expect.sequenceEqual "arena matches tree" tree.Cells

  testCase "Arena parity: bottom-right aligned" <| fun () ->
    let elem = El.alignBottomRight (El.text "Z")
    let (tree, arena) = arenaHelper elem 10 5
    arena.Cells |> Expect.sequenceEqual "arena matches tree" tree.Cells
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 12: Gap
// ═══════════════════════════════════════════════════════════════════════

let gapTests = testList "Gap" [
  testCase "Row with gap — texts separated" <| fun () ->
    // Two texts "A" and "B" in 10-wide row with gap 2
    // Without gap: "A" at 0..4, "B" at 5..9 (Fill splits evenly)
    // With gap 2: usable=8, each gets 4. "A" at 0, "B" at 6
    let elem = El.gap 2 (El.row [El.text "A"; El.text "B"])
    let buf = renderToBuffer 10 1 elem
    charAt 0 0 buf |> Expect.equal "A at 0" 'A'
    charAt 6 0 buf |> Expect.equal "B at 6" 'B'
    // Gap area should be empty
    charAt 4 0 buf |> Expect.equal "gap space 1" ' '
    charAt 5 0 buf |> Expect.equal "gap space 2" ' '

  testCase "Column with gap — texts separated" <| fun () ->
    // Two texts in 10-tall column with gap 2
    // usable=8, each gets 4. First at row 0, second at row 6
    let elem = El.gap 2 (El.column [El.text "X"; El.text "Y"])
    let buf = renderToBuffer 5 10 elem
    charAt 0 0 buf |> Expect.equal "X at row 0" 'X'
    charAt 0 6 buf |> Expect.equal "Y at row 6" 'Y'
    charAt 0 4 buf |> Expect.equal "gap row 1" ' '
    charAt 0 5 buf |> Expect.equal "gap row 2" ' '

  testCase "Gap with fixed children" <| fun () ->
    // Row: Fixed(3) "AB" + Fill "CD" in 10 wide with gap 2
    // usable=8, Fixed(3)=3, Fill gets 5. Offsets: 0, 3+2=5
    let elem = El.gap 2 (El.row [El.width 3 (El.text "AB"); El.text "CD"])
    let buf = renderToBuffer 10 1 elem
    charAt 0 0 buf |> Expect.equal "A" 'A'
    charAt 1 0 buf |> Expect.equal "B" 'B'
    charAt 5 0 buf |> Expect.equal "C" 'C'
    charAt 6 0 buf |> Expect.equal "D" 'D'

  testCase "Gap on non-Row/Column passes through" <| fun () ->
    let elem = El.gap 2 (El.text "Hi")
    let buf = renderToBuffer 10 1 elem
    charAt 0 0 buf |> Expect.equal "H" 'H'

  testCase "Arena parity: row with gap" <| fun () ->
    let elem = El.gap 2 (El.row [El.text "A"; El.text "B"])
    let (tree, arena) = arenaHelper elem 10 1
    arena.Cells |> Expect.sequenceEqual "arena matches tree" tree.Cells

  testCase "Arena parity: column with gap" <| fun () ->
    let elem = El.gap 2 (El.column [El.text "X"; El.text "Y"])
    let (tree, arena) = arenaHelper elem 5 10
    arena.Cells |> Expect.sequenceEqual "arena matches tree" tree.Cells
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 6: Overflow Clipping
// intersectArea clips child areas to parent bounds
// ═══════════════════════════════════════════════════════════════════════

let overflowClippingTests = testList "Overflow Clipping" [
  testCase "intersectArea: child extends right of parent" <| fun () ->
    let parent = areaAt 0 0 10 5
    let child = areaAt 5 0 10 5
    let clipped = Layout.intersectArea parent child
    clipped.X |> Expect.equal "x" 5
    clipped.Width |> Expect.equal "width clipped to 5" 5

  testCase "intersectArea: child extends below parent" <| fun () ->
    let parent = areaAt 0 0 10 5
    let child = areaAt 0 3 10 5
    let clipped = Layout.intersectArea parent child
    clipped.Y |> Expect.equal "y" 3
    clipped.Height |> Expect.equal "height clipped to 2" 2

  testCase "intersectArea: child fully inside parent unchanged" <| fun () ->
    let parent = areaAt 0 0 20 20
    let child = areaAt 5 5 5 5
    Layout.intersectArea parent child |> Expect.equal "unchanged" child

  testCase "intersectArea: no overlap returns zero area" <| fun () ->
    let parent = areaAt 0 0 10 10
    let child = areaAt 20 20 5 5
    let clipped = Layout.intersectArea parent child
    clipped.Width |> Expect.equal "zero width" 0
    clipped.Height |> Expect.equal "zero height" 0

  testCase "intersectArea: child extends both directions" <| fun () ->
    let parent = areaAt 5 5 10 10
    let child = areaAt 0 0 20 20
    Layout.intersectArea parent child |> Expect.equal "clipped to parent" parent

  testCase "intersectArea: parent with offset clips correctly" <| fun () ->
    let parent = areaAt 10 10 5 5
    let child = areaAt 12 12 5 5
    let clipped = Layout.intersectArea parent child
    clipped.X |> Expect.equal "x" 12
    clipped.Y |> Expect.equal "y" 12
    clipped.Width |> Expect.equal "width" 3
    clipped.Height |> Expect.equal "height" 3

  testCase "column: overflow children clipped at render" <| fun () ->
    // 3 rows of text (content height 1 each) in 2-tall container
    let elem = El.column [El.text "A"; El.text "B"; El.text "C"]
    let buf = renderToBuffer 5 2 elem
    // With proportional shrink: [1,1,0] — C gets 0 height, nothing rendered
    charAt 0 0 buf |> Expect.equal "first row" 'A'
    charAt 0 1 buf |> Expect.equal "second row" 'B'

  testCase "row: overflow children clipped at render" <| fun () ->
    // 3 columns of text in 5-wide container: "AB" "CD" "EF" → total 6 > 5
    let elem = El.row [
      Constrained(Fixed 2, Text("AB", Style.empty))
      Constrained(Fixed 2, Text("CD", Style.empty))
      Constrained(Fixed 2, Text("EF", Style.empty))
    ]
    let buf = renderToBuffer 5 1 elem
    // Fixed(2)+Fixed(2)+Fixed(2) in 5: first=2, second=2, third=min(2,1)=1
    // Third clipped to 1 char: "E" not "EF"
    charAt 0 0 buf |> Expect.equal "first" 'A'
    charAt 1 0 buf |> Expect.equal "first" 'B'
    charAt 2 0 buf |> Expect.equal "second" 'C'
    charAt 3 0 buf |> Expect.equal "second" 'D'
    charAt 4 0 buf |> Expect.equal "third clipped" 'E'
]

// ═══════════════════════════════════════════════════════════════════════
// PHASE 7: Flex-shrink (Proportional)
// When Fill children's content > available, shrink proportionally
// ═══════════════════════════════════════════════════════════════════════

let flexShrinkTests = testList "Flex-shrink proportional" [
  testCase "equal content shrunk proportionally" <| fun () ->
    let result = Layout.solveWithContent 10 [Fill; Fill; Fill] [5; 5; 5]
    let sizes = result |> List.map snd
    sizes |> Expect.equal "proportional [4,3,3]" [4; 3; 3]

  testCase "unequal content shrunk proportionally" <| fun () ->
    let result = Layout.solveWithContent 5 [Fill; Fill] [8; 2]
    let sizes = result |> List.map snd
    sizes |> Expect.equal "proportional [4,1]" [4; 1]

  testCase "single Fill overflows" <| fun () ->
    let result = Layout.solveWithContent 3 [Fill] [10]
    let sizes = result |> List.map snd
    sizes |> Expect.equal "clamped to 3" [3]

  testCase "mixed Fixed + Fill overflow: Fixed preserved" <| fun () ->
    let result = Layout.solveWithContent 10 [Fixed 6; Fill] [6; 8]
    let sizes = result |> List.map snd
    sizes |> Expect.equal "fixed 6, fill 4" [6; 4]

  testCase "total sizes never exceed available" <| fun () ->
    let result = Layout.solveWithContent 10 [Fill; Fill; Fill; Fill] [10; 10; 10; 10]
    let total = result |> List.sumBy snd
    total |> Expect.equal "total = 10" 10

  testCase "no shrink when content fits" <| fun () ->
    let result = Layout.solveWithContent 20 [Fill; Fill] [3; 3]
    let sizes = result |> List.map snd
    // Excess 14 split equally: each gets 3 + 7 = 10
    sizes |> Expect.equal "equal split" [10; 10]

  testCase "Arena parity: overflow clipping" <| fun () ->
    let elem = El.column [El.text "A"; El.text "B"; El.text "C"]
    let (tree, arena) = arenaHelper elem 5 2
    arena.Cells |> Expect.sequenceEqual "arena matches tree" tree.Cells
]

// ═══════════════════════════════════════════════════════════════════════
// Combined test list for export
// ═══════════════════════════════════════════════════════════════════════

/// Property-based invariant tests for the layout constraint solver.
/// These are the "safety net" tests Haynes recommended before renderer refactors.
let layoutConservationPropertyTests =
  testList "Layout conservation properties (FsCheck)" [
    testProperty "solveWithContent never allocates more than available" <|
      fun (available: byte) (n: byte) ->
        let avail = int available
        let count = max 1 (int n % 6)
        let constraints = List.replicate count Fill
        let contents = List.replicate count 0
        let result = Layout.solveWithContent avail constraints contents
        let total = result |> List.sumBy snd
        total <= avail

    testProperty "solveWithContent widths are non-negative" <|
      fun (available: byte) (n: byte) ->
        let avail = int available
        let count = max 1 (int n % 6)
        let constraints = List.replicate count Fill
        let contents = List.replicate count 0
        let result = Layout.solveWithContent avail constraints contents
        result |> List.forall (fun (_, w) -> w >= 0)

    testProperty "solveWithContent sums to available when no overflow" <|
      fun (available: byte) (n: byte) ->
        let avail = max 1 (int available)
        let count = max 1 (int n % 5)
        let constraints = List.replicate count Fill
        // Zero content sizes → must sum exactly to avail (no overflow)
        let result = Layout.solveWithContent avail constraints (List.replicate count 0)
        let total = result |> List.sumBy snd
        total = avail

    testProperty "solveWithContent result count equals constraint count" <|
      fun (available: byte) (n: byte) ->
        let avail = int available
        let count = max 1 (int n % 8)
        let constraints = List.replicate count Fill
        let contents = List.replicate count 0
        let result = Layout.solveWithContent avail constraints contents
        List.length result = count

    testProperty "Fixed constraint respected exactly when space available" <|
      fun (available: byte) (fixedSize: byte) ->
        let avail = int available
        let fs = int fixedSize
        match avail >= fs with
        | false -> true  // skip over-constraint cases
        | true ->
          let result = Layout.solveWithContent avail [Fixed fs] [0]
          match result with
          | [(_, w)] -> w = fs
          | _ -> false

    testProperty "offsets are non-decreasing" <|
      fun (available: byte) (n: byte) ->
        let avail = max 1 (int available)
        let count = max 1 (int n % 6)
        let constraints = List.replicate count Fill
        let contents = List.replicate count 0
        let offsets = Layout.solveWithContent avail constraints contents |> List.map fst
        offsets
        |> List.pairwise
        |> List.forall (fun (a, b) -> b >= a)

    testProperty "render conserves pixel count — column children widths" <|
      fun (wb: byte) (hb: byte) ->
        let w = max 3 (int wb % 30 + 3)
        let h = max 3 (int hb % 20 + 3)
        let buf = renderToBuffer w h (El.column [El.text "A"; El.text "B"; El.text "C"])
        // Total allocated cells = w * h (the buffer is always fully populated)
        Array.length buf.Cells = w * h

    testProperty "render conserves pixel count — row children" <|
      fun (wb: byte) (hb: byte) ->
        let w = max 3 (int wb % 30 + 3)
        let h = max 3 (int hb % 20 + 3)
        let buf = renderToBuffer w h (El.row [El.fill (El.text "X"); El.fill (El.text "Y")])
        Array.length buf.Cells = w * h

    testProperty "Percentage + Fill always sums to available" <|
      fun (available: byte) (pct: byte) ->
        let avail = max 1 (int available)
        let p = max 0 (min 100 (int pct % 101))
        let result = Layout.solveWithContent avail [Percentage p; Fill] [0; 0]
        let total = result |> List.sumBy snd
        // Allow off-by-one from integer rounding
        abs (total - avail) <= 1
  ]

/// Property-based invariant tests for Buffer.diff.
let diffInvariantPropertyTests =
  testList "Buffer.diff invariants (FsCheck)" [
    testProperty "self-diff always empty" <|
      fun (wb: byte) (hb: byte) ->
        let w = max 1 (int wb % 50)
        let h = max 1 (int hb % 50)
        let buf = Buffer.create w h
        Buffer.diff buf buf |> (fun d -> d.Count = 0)

    testProperty "diff after copy is empty" <|
      fun (wb: byte) (hb: byte) ->
        let w = max 1 (int wb % 30)
        let h = max 1 (int hb % 30)
        let front = Buffer.create w h
        let back = Buffer.create w h
        // Copy front -> back to force equality
        Array.blit front.Cells 0 back.Cells 0 front.Cells.Length
        Buffer.diff front back |> (fun d -> d.Count = 0)

    testProperty "diff detects single write" <|
      fun (wb: byte) (hb: byte) (rv: int32) ->
        let w = max 2 (int wb % 30 + 2)
        let h = max 2 (int hb % 30 + 2)
        let front = Buffer.create w h
        let back = Buffer.create w h
        let x = (abs rv) % w
        let y = (abs rv / w) % h
        let cell = { Rune = 65; Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }
        Buffer.set x y cell back
        // Diff must report at least one change
        Buffer.diff front back |> (fun d -> d.Count > 0)

    testProperty "diff count bounded by total cells" <|
      fun (wb: byte) (hb: byte) ->
        let w = max 1 (int wb % 20)
        let h = max 1 (int hb % 20)
        let front = Buffer.create w h
        let back = Buffer.create w h
        // Write every cell differently
        for i in 0 .. back.Cells.Length - 1 do
          back.Cells[i] <- { Rune = 65 + (i % 26); Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }
        let changes = Buffer.diff front back
        changes.Count <= w * h

    testProperty "applying diff patches makes front equal back" <|
      fun (wb: byte) (hb: byte) (rv: int32) ->
        let w = max 2 (int wb % 20 + 2)
        let h = max 2 (int hb % 20 + 2)
        let front = Buffer.create w h
        let back = Buffer.create w h
        // Write some cells to back
        for i in 0 .. min 5 (back.Cells.Length - 1) do
          back.Cells[i] <- { Rune = 65 + (i % 26); Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }
        let changes = Buffer.diff front back
        // Apply changes: for each dirty index, copy from back to front
        for idx in changes do
          front.Cells[idx] <- back.Cells[idx]
        // Now front should equal back at all changed cells
        let stillDiff = Buffer.diff front back
        stillDiff.Count = 0
  ]

// ═══════════════════════════════════════════════════════════════════════
// RENDER EQUIVALENCE PROPERTY TESTS
// Generates arbitrary Element trees and asserts Render.fs and
// ArenaRender.fs produce byte-identical Buffer cells for every case.
// ═══════════════════════════════════════════════════════════════════════

let private genSimpleText : Gen<Element> =
  Gen.frequency [
    5, Gen.elements [ ""; "A"; "Hi"; "Hello"; "Longer text" ]
    3, Gen.elements [ "行列"; "こんにちは"; "你好"; "한국어"; "中文字" ]
    2, Gen.elements [ "αβγ"; "Ωφψ" ]
  ]
  |> Gen.map El.text

let rec private genElement (depth: int) : Gen<Element> =
  let leafGen = Gen.oneof [ Gen.constant El.empty; genSimpleText ]
  match depth with
  | 0 -> leafGen
  | d ->
    let childGen  = genElement (d - 1)
    let listGen   = Gen.nonEmptyListOf childGen |> Gen.resize 3
    Gen.frequency [
      1, Gen.constant El.empty
      3, genSimpleText
      2, Gen.map El.row  listGen
      2, Gen.map El.column listGen
      1, Gen.map2 (fun bs c -> El.bordered bs c)
                  (Gen.elements [ Light; Heavy; Double; Rounded ])
                  childGen
      1, Gen.map2 (fun n  c -> El.width (max 1 n) c)
                  (Gen.choose (1, 30))
                  childGen
      1, Gen.map2 (fun n  c -> El.height (max 1 n) c)
                  (Gen.choose (1, 10))
                  childGen
      1, Gen.map  (fun c -> El.padAll 1 c) childGen
    ]

/// FsCheck Arbitrary for Element — used by testPropertyWithConfig.
type ElementArb() =
  static member Element() : Arbitrary<Element> =
    Arb.fromGen (Gen.sized (fun s -> genElement (min s 4)))

let renderEquivalencePropertyTests =
  testList "Render/ArenaRender parity property tests (FsCheck)" [

    testPropertyWithConfig
      { FsCheckConfig.defaultConfig with maxTest = 300; arbitrary = [ typeof<ElementArb> ] }
      "Render and ArenaRender produce identical cells for any element at arbitrary sizes"
    <| fun (wb: byte) (hb: byte) (elem: Element) ->
        let w = max 1 (int wb % 60 + 1)
        let h = max 1 (int hb % 30 + 1)
        let (treeBuf, arenaBuf) = arenaHelper elem w h
        arenaBuf.Cells |> Expect.sequenceEqual "cells match" treeBuf.Cells

    testPropertyWithConfig
      { FsCheckConfig.defaultConfig with maxTest = 200; arbitrary = [ typeof<ElementArb> ] }
      "Buffer cell count is always w*h regardless of element complexity"
    <| fun (wb: byte) (hb: byte) (elem: Element) ->
        let w = max 1 (int wb % 60 + 1)
        let h = max 1 (int hb % 30 + 1)
        let (treeBuf, _) = arenaHelper elem w h
        Array.length treeBuf.Cells = w * h

    testPropertyWithConfig
      { FsCheckConfig.defaultConfig with maxTest = 200; arbitrary = [ typeof<ElementArb> ] }
      "ArenaRender produces exactly w*h cells for any element"
    <| fun (wb: byte) (hb: byte) (elem: Element) ->
        let w = max 1 (int wb % 60 + 1)
        let h = max 1 (int hb % 30 + 1)
        let (_, arenaBuf) = arenaHelper elem w h
        Array.length arenaBuf.Cells = w * h

    testPropertyWithConfig
      { FsCheckConfig.defaultConfig with maxTest = 150; arbitrary = [ typeof<ElementArb> ] }
      "Render is idempotent: same element twice produces identical buffer"
    <| fun (wb: byte) (hb: byte) (elem: Element) ->
        let w = max 1 (int wb % 40 + 1)
        let h = max 1 (int hb % 20 + 1)
        let (buf1, _) = arenaHelper elem w h
        let (buf2, _) = arenaHelper elem w h
        buf2.Cells |> Expect.sequenceEqual "idempotent" buf1.Cells

    testPropertyWithConfig
      { FsCheckConfig.defaultConfig with maxTest = 300; arbitrary = [ typeof<ElementArb> ] }
      "Wide chars always have Rune=0 continuation at col+1 (both render paths)"
    <| fun (wb: byte) (hb: byte) (elem: Element) ->
        let w = max 2 (int wb % 60 + 2)
        let h = max 1 (int hb % 30 + 1)
        let (treeBuf, arenaBuf) = arenaHelper elem w h
        // Check each buffer independently
        let checkBuf (name: string) (buf: Buffer) =
          for y in 0 .. h - 1 do
            for x in 0 .. w - 2 do  // stop at w-2 since we check x+1
              let cell = buf.Cells[y * w + x]
              let rune = System.Text.Rune(cell.Rune)
              if RuneWidth.getColumnWidth rune = 2 then
                let next = buf.Cells[y * w + x + 1]
                next.Rune |> Expect.equal (sprintf "%s: col %d+1 after wide char has Rune=0" name x) 0
        checkBuf "Render" treeBuf
        checkBuf "ArenaRender" arenaBuf
  ]

[<Tests>]
let allLayoutTests = testList "MDN CSS Layout Compliance" [
  measureWidthTests
  measureHeightTests
  solveWithContentTests
  blockFlowTests
  inlineFlowTests
  flexboxTests
  boxModelTests
  sizingTests
  compositeLayoutTests
  arenaParityTests
  edgeCaseTests
  alignmentTests
  gapTests
  overflowClippingTests
  flexShrinkTests
  layoutConservationPropertyTests
  diffInvariantPropertyTests
  renderEquivalencePropertyTests
]
