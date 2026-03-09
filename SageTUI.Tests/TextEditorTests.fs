module TextEditorTests

open Expecto
open Expecto.Flip
open SageTUI

// ── helpers ──────────────────────────────────────────────────────────────────

/// Shorthand to send a sequence of TextEditorMsgs to a model.
let apply msgs m = msgs |> List.fold (fun m msg -> TextEditor.update msg m) m

let mk = TextEditor.init

// ── basic init ───────────────────────────────────────────────────────────────

let initTests = testList "TextEditor.init" [
  test "empty string yields single empty line" {
    let m = mk ""
    m.Lines |> Expect.hasLength "one line" 1
    m.Lines.[0] |> Expect.equal "empty" ""
    m.Row |> Expect.equal "row 0" 0
    m.Col |> Expect.equal "col 0" 0
  }
  test "single line with no newline" {
    let m = mk "hello"
    m.Lines |> Expect.hasLength "one line" 1
    m.Lines.[0] |> Expect.equal "line content" "hello"
  }
  test "multi-line splits on newline" {
    let m = mk "a\nb\nc"
    m.Lines |> Expect.hasLength "3 lines" 3
    m.Lines.[0] |> Expect.equal "line 0" "a"
    m.Lines.[1] |> Expect.equal "line 1" "b"
    m.Lines.[2] |> Expect.equal "line 2" "c"
  }
  test "content roundtrip" {
    let s = "line1\nline2\nline3"
    mk s |> TextEditor.content |> Expect.equal "content roundtrip" s
  }
]

// ── insert char ──────────────────────────────────────────────────────────────

let insertTests = testList "TextEditor.TEInsertChar" [
  test "insert at start of empty line" {
    let m = mk "" |> TextEditor.update (TEInsertChar 'x')
    m.Lines.[0] |> Expect.equal "line" "x"
    m.Col |> Expect.equal "col advances" 1
  }
  test "insert appends at col = length" {
    let m = { mk "hello" with Col = 5 } |> TextEditor.update (TEInsertChar '!')
    m.Lines.[0] |> Expect.equal "line" "hello!"
    m.Col |> Expect.equal "col" 6
  }
  test "insert in middle splits correctly" {
    let m = { mk "ac" with Col = 1 } |> TextEditor.update (TEInsertChar 'b')
    m.Lines.[0] |> Expect.equal "line" "abc"
    m.Col |> Expect.equal "col" 2
  }
  test "insert does not affect other lines" {
    let m = { mk "a\nb" with Row = 0; Col = 1 } |> TextEditor.update (TEInsertChar 'X')
    m.Lines.[1] |> Expect.equal "line 1 unchanged" "b"
  }
]

// ── newline ──────────────────────────────────────────────────────────────────

let newlineTests = testList "TextEditor.TENewline" [
  test "newline at end appends blank line" {
    let m = { mk "hello" with Col = 5 } |> TextEditor.update TENewline
    m.Lines |> Expect.hasLength "2 lines" 2
    m.Lines.[0] |> Expect.equal "line 0" "hello"
    m.Lines.[1] |> Expect.equal "line 1" ""
    m.Row |> Expect.equal "row advances" 1
    m.Col |> Expect.equal "col resets" 0
  }
  test "newline at start prepends blank line" {
    let m = mk "hello" |> TextEditor.update TENewline
    m.Lines.[0] |> Expect.equal "line 0" ""
    m.Lines.[1] |> Expect.equal "line 1" "hello"
    m.Row |> Expect.equal "row 1" 1
  }
  test "newline in middle splits line" {
    let m = { mk "hello" with Col = 3 } |> TextEditor.update TENewline
    m.Lines.[0] |> Expect.equal "before" "hel"
    m.Lines.[1] |> Expect.equal "after" "lo"
    m.Lines |> Expect.hasLength "2 lines" 2
  }
  test "maxLines blocks newline when full" {
    let m = { TextEditor.init "a\nb" with MaxLines = Some 2 }
    let m' = m |> TextEditor.update TENewline
    m'.Lines |> Expect.hasLength "still 2 lines" 2
  }
]

// ── backspace ────────────────────────────────────────────────────────────────

let backspaceTests = testList "TextEditor.TEBackspace" [
  test "backspace at doc start is noop" {
    let m = mk "" |> TextEditor.update TEBackspace
    m.Lines |> Expect.hasLength "still 1 line" 1
    m.Lines.[0] |> Expect.equal "empty" ""
  }
  test "backspace removes preceding char" {
    let m = { mk "abc" with Col = 2 } |> TextEditor.update TEBackspace
    m.Lines.[0] |> Expect.equal "line" "ac"
    m.Col |> Expect.equal "col decrements" 1
  }
  test "backspace at line start merges with previous line" {
    let m = { mk "foo\nbar" with Row = 1; Col = 0 } |> TextEditor.update TEBackspace
    m.Lines |> Expect.hasLength "1 line" 1
    m.Lines.[0] |> Expect.equal "merged" "foobar"
    m.Col |> Expect.equal "col = prev length" 3
  }
]

// ── delete ───────────────────────────────────────────────────────────────────

let deleteTests = testList "TextEditor.TEDelete" [
  test "delete at end of last line is noop" {
    let m = { mk "abc" with Col = 3 } |> TextEditor.update TEDelete
    m.Lines.[0] |> Expect.equal "unchanged" "abc"
  }
  test "delete removes char at cursor" {
    let m = { mk "abc" with Col = 1 } |> TextEditor.update TEDelete
    m.Lines.[0] |> Expect.equal "line" "ac"
    m.Col |> Expect.equal "col unchanged" 1
  }
  test "delete at line end joins next line" {
    let m = { mk "foo\nbar" with Row = 0; Col = 3 } |> TextEditor.update TEDelete
    m.Lines |> Expect.hasLength "1 line" 1
    m.Lines.[0] |> Expect.equal "joined" "foobar"
  }
]

// ── cursor movement ──────────────────────────────────────────────────────────

let movementTests = testList "TextEditor.Movement" [
  test "MoveLeft at start of line wraps to end of previous" {
    let m = { mk "abc\ndef" with Row = 1; Col = 0 } |> TextEditor.update TEMoveLeft
    m.Row |> Expect.equal "row" 0
    m.Col |> Expect.equal "col = prev line end" 3
  }
  test "MoveRight at end of line wraps to start of next" {
    let m = { mk "abc\ndef" with Row = 0; Col = 3 } |> TextEditor.update TEMoveRight
    m.Row |> Expect.equal "row" 1
    m.Col |> Expect.equal "col" 0
  }
  test "MoveUp clamps row" {
    let m = mk "a" |> TextEditor.update TEMoveUp
    m.Row |> Expect.equal "still row 0" 0
  }
  test "MoveDown clamps row" {
    let m = { mk "a\nb" with Row = 1 } |> TextEditor.update TEMoveDown
    m.Row |> Expect.equal "still row 1" 1
  }
  test "MoveLineStart goes to col 0" {
    let m = { mk "hello" with Col = 3 } |> TextEditor.update TEMoveLineStart
    m.Col |> Expect.equal "col 0" 0
  }
  test "MoveLineEnd goes to end of line" {
    let m = mk "hello" |> TextEditor.update TEMoveLineEnd
    m.Col |> Expect.equal "col 5" 5
  }
  test "MoveDocStart goes to (0,0)" {
    let m = { mk "a\nb\nc" with Row = 2; Col = 1 } |> TextEditor.update TEMoveDocStart
    m.Row |> Expect.equal "row 0" 0
    m.Col |> Expect.equal "col 0" 0
  }
  test "MoveDocEnd goes to last row, last col" {
    let m = mk "a\nb\ncde" |> TextEditor.update TEMoveDocEnd
    m.Row |> Expect.equal "last row" 2
    m.Col |> Expect.equal "last col" 3
  }
  test "WordJumpRight skips to next word boundary" {
    let m = mk "hello world" |> TextEditor.update TEWordJumpRight
    m.Col |> Expect.equal "after first word" 5
  }
  test "WordJumpLeft skips back to prev word boundary" {
    let m = { mk "hello world" with Col = 11 } |> TextEditor.update TEWordJumpLeft
    m.Col |> Expect.equal "start of second word" 6
  }
]

// ── selection ────────────────────────────────────────────────────────────────

let selectionTests = testList "TextEditor.Selection" [
  test "SelectAll sets anchor at (0,0) and cursor at end" {
    let m = mk "abc\nde" |> TextEditor.update TESelectAll
    m.SelectionAnchor |> Expect.equal "anchor" (Some (0, 0))
    m.Row |> Expect.equal "last row" 1
    m.Col |> Expect.equal "last col" 2
  }
  test "selectedText returns correct span" {
    let m = mk "hello world"
    let m' = { m with Col = 5; SelectionAnchor = Some (0, 0) }
    TextEditor.selectedText m' |> Expect.equal "selection" (Some "hello")
  }
  test "Backspace deletes selection" {
    let m = { mk "hello world" with Col = 5; SelectionAnchor = Some (0, 0) }
    let m' = TextEditor.update TEBackspace m
    m'.Lines.[0] |> Expect.equal "after delete" " world"
    m'.SelectionAnchor |> Expect.equal "anchor cleared" None
  }
]

// ── paste ────────────────────────────────────────────────────────────────────

let pasteTests = testList "TextEditor.TEPaste" [
  test "paste single line inserts inline" {
    let m = { mk "ac" with Col = 1 } |> TextEditor.update (TEPaste "b")
    m.Lines.[0] |> Expect.equal "line" "abc"
    m.Col |> Expect.equal "col" 2
  }
  test "paste multi-line splits line and inserts" {
    let m = { mk "start-end" with Col = 6 } |> TextEditor.update (TEPaste "X\nY")
    m.Lines.[0] |> Expect.equal "first part" "start-X"
    m.Lines.[1] |> Expect.equal "second part" "Yend"
    m.Lines |> Expect.hasLength "2 lines" 2
    m.Row |> Expect.equal "row 1" 1
    m.Col |> Expect.equal "col after Y" 1
  }
]

// ── setContent ───────────────────────────────────────────────────────────────

let setContentTests = testList "TextEditor.TESetContent" [
  test "setContent replaces all lines and resets cursor" {
    let m = { mk "old content" with Row = 0; Col = 5 }
    let m' = TextEditor.update (TESetContent "new\ncontent") m
    m'.Lines.[0] |> Expect.equal "line 0" "new"
    m'.Lines.[1] |> Expect.equal "line 1" "content"
    m'.Row |> Expect.equal "row reset" 0
    m'.Col |> Expect.equal "col reset" 0
  }
]

// ── scroll ───────────────────────────────────────────────────────────────────

let scrollTests = testList "TextEditor.Scroll" [
  test "scrollIntoView keeps cursor visible: scroll down" {
    let m = { mk "a\nb\nc\nd\ne" with Row = 4; ScrollTop = 0 }
    let m' = TextEditor.scrollIntoView 3 m
    (m'.ScrollTop <= m'.Row && m'.Row < m'.ScrollTop + 3) |> Expect.isTrue "cursor in view"
  }
  test "scrollIntoView keeps cursor visible: scroll up" {
    let m = { mk "a\nb\nc\nd\ne" with Row = 0; ScrollTop = 3 }
    let m' = TextEditor.scrollIntoView 3 m
    m'.ScrollTop |> Expect.equal "scroll back to top" 0
  }
]

// ── view ─────────────────────────────────────────────────────────────────────

let viewTests = testList "TextEditor.view" [
  test "view returns Column element" {
    let m = mk "line1\nline2"
    let el = TextEditor.view false 10 m
    match el with
    | Column _ -> ()
    | _ -> failtest "expected Column"
  }
  test "view focused: current row shows reversed cursor" {
    let m = { mk "hi" with Col = 1 }
    let el = TextEditor.view true 5 m
    // Row 0 is focused: should have Row with reversed text at cursor
    match el with
    | Column (firstRow :: _) ->
      match firstRow with
      | Row _ -> ()  // Contains split text + reversed cursor
      | _ -> failtest "expected Row for cursor line"
    | _ -> failtest "expected Column"
  }
  test "view unfocused: lines rendered as plain text" {
    let m = mk "line1\nline2"
    let el = TextEditor.view false 5 m
    match el with
    | Column (t1 :: t2 :: []) ->
      match t1, t2 with
      | Text _, Text _ -> ()
      | _ -> failtest "expected two Text elements"
    | _ -> failtest "expected two-element Column"
  }
]

[<Tests>]
let allTextEditorTests = testList "TextEditor" [
  initTests
  insertTests
  newlineTests
  backspaceTests
  deleteTests
  movementTests
  selectionTests
  pasteTests
  setContentTests
  scrollTests
  viewTests
]

