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
  test "Backspace with mid-text selection deletes selection only (no double-delete)" {
    // Bug: m'.SelectionAnchor was checked (always None after deleteSelection) → extra char deleted
    // Fix: m.SelectionAnchor is checked → selection-only delete
    // "hello world", anchor at col 3, cursor at col 5 → selects "lo"
    let m = { mk "hello world" with Col = 5; SelectionAnchor = Some (0, 3) }
    let m' = TextEditor.update TEBackspace m
    m'.Lines.[0] |> Expect.equal "selection deleted only" "hel world"
    m'.Col       |> Expect.equal "cursor at anchor" 3
    m'.SelectionAnchor |> Expect.equal "anchor cleared" None
  }
  test "Delete with mid-text selection deletes selection only (no double-delete)" {
    // "hello world", anchor at col 6, cursor at col 11 → selects "world"
    let m = { mk "hello world" with Col = 11; SelectionAnchor = Some (0, 6) }
    let m' = TextEditor.update TEDelete m
    m'.Lines.[0] |> Expect.equal "selection deleted only" "hello "
    m'.Col       |> Expect.equal "cursor at anchor" 6
    m'.SelectionAnchor |> Expect.equal "anchor cleared" None
  }
  test "TESelectLeft extends selection leftward" {
    let m = { mk "hello" with Col = 3 }
    let m' = TextEditor.update TESelectLeft m
    m'.Col |> Expect.equal "col at 2" 2
    m'.SelectionAnchor |> Expect.equal "anchor at (0,3)" (Some (0, 3))
  }
  test "TESelectRight extends selection rightward" {
    let m = { mk "hello" with Col = 2 }
    let m' = TextEditor.update TESelectRight m
    m'.Col |> Expect.equal "col at 3" 3
    m'.SelectionAnchor |> Expect.equal "anchor at (0,2)" (Some (0, 2))
  }
  test "TESelectUp extends selection to previous row" {
    let m = { mk "abc\ndef" with Row = 1; Col = 2 }
    let m' = TextEditor.update TESelectUp m
    m'.Row |> Expect.equal "row 0" 0
    m'.Col |> Expect.equal "col" 2
    m'.SelectionAnchor |> Expect.equal "anchor at (1,2)" (Some (1, 2))
  }
  test "TESelectDown extends selection to next row" {
    let m = { mk "abc\ndef" with Row = 0; Col = 1 }
    let m' = TextEditor.update TESelectDown m
    m'.Row |> Expect.equal "row 1" 1
    m'.Col |> Expect.equal "col" 1
    m'.SelectionAnchor |> Expect.equal "anchor at (0,1)" (Some (0, 1))
  }
  test "TESelectUp clamps col to line length" {
    let m = { mk "ab\ncde" with Row = 1; Col = 3 }
    let m' = TextEditor.update TESelectUp m
    m'.Row |> Expect.equal "row 0" 0
    m'.Col |> Expect.equal "clamped to 2" 2
  }
  test "TESelectWordLeft extends selection by word" {
    let m = { mk "hello world" with Col = 11 }
    let m' = TextEditor.update TESelectWordLeft m
    m'.Col |> Expect.equal "at word start" 6
    m'.SelectionAnchor |> Expect.equal "anchor at (0,11)" (Some (0, 11))
  }
  test "TESelectWordRight extends selection by word" {
    let m = { mk "hello world" with Col = 0 }
    let m' = TextEditor.update TESelectWordRight m
    m'.Col |> Expect.equal "at word end" 5
    m'.SelectionAnchor |> Expect.equal "anchor at (0,0)" (Some (0, 0))
  }
  // ── boundary / degenerate-anchor tests ───────────────────────────────────
  test "TESelectLeft at document start (Row=0 Col=0) does not create anchor" {
    let m = mk "hello"  // cursor at Row=0, Col=0 — document start
    let m' = TextEditor.update TESelectLeft m
    m'.Col |> Expect.equal "col unchanged" 0
    m'.Row |> Expect.equal "row unchanged" 0
    m'.SelectionAnchor |> Expect.equal "no anchor created" None
  }
  test "TESelectRight at single-line document end does not create anchor" {
    let m = { mk "hello" with Col = 5 }  // cursor at end of single line (= document end)
    let m' = TextEditor.update TESelectRight m
    m'.Col |> Expect.equal "col unchanged" 5
    m'.SelectionAnchor |> Expect.equal "no anchor created" None
  }
  test "TESelectUp at row 0 does not change model (no degenerate anchor)" {
    let m = mk "hello"  // single-line, already at row 0
    let m' = TextEditor.update TESelectUp m
    m'.Row |> Expect.equal "row unchanged" 0
    m'.SelectionAnchor |> Expect.equal "no anchor created" None
  }
  test "TESelectDown at last row does not change model (no degenerate anchor)" {
    let m = { mk "abc\ndef" with Row = 1; Col = 0 }  // already at last row
    let m' = TextEditor.update TESelectDown m
    m'.Row |> Expect.equal "row unchanged" 1
    m'.SelectionAnchor |> Expect.equal "no anchor created" None
  }
  test "TESelectAll then Backspace deletes all content" {
    let m = mk "hello world"
    let m' = m |> TextEditor.update TESelectAll |> TextEditor.update TEBackspace
    m'.Lines |> Expect.hasLength "one empty line" 1
    m'.Lines.[0] |> Expect.equal "empty" ""
    m'.Col |> Expect.equal "col at 0" 0
    m'.SelectionAnchor |> Expect.equal "anchor cleared" None
  }
  test "TESelectAll then InsertChar replaces all content" {
    let m = mk "hello world"
    let m' = m |> TextEditor.update TESelectAll |> TextEditor.update (TEInsertChar 'X')
    m'.Lines |> Expect.hasLength "one line" 1
    m'.Lines.[0] |> Expect.equal "replaced" "X"
  }
  // ── cross-line selection ────────────────────────────────────────────────────
  test "TESelectLeft at Col=0 extends selection to end of previous line" {
    let m = { mk "abc\ndef" with Row = 1; Col = 0 }
    let m' = TextEditor.update TESelectLeft m
    m'.Row |> Expect.equal "moved to row 0" 0
    m'.Col |> Expect.equal "col at end of 'abc'" 3
    m'.SelectionAnchor |> Expect.equal "anchor at (1,0)" (Some (1, 0))
  }
  test "TESelectRight at end-of-line extends selection to start of next line" {
    let m = { mk "abc\ndef" with Row = 0; Col = 3 }
    let m' = TextEditor.update TESelectRight m
    m'.Row |> Expect.equal "moved to row 1" 1
    m'.Col |> Expect.equal "col at 0" 0
    m'.SelectionAnchor |> Expect.equal "anchor at (0,3)" (Some (0, 3))
  }
  test "TESelectLeft at first line Col=0 stays put (document start)" {
    let m = mk "hello"  // cursor at row=0, col=0
    let m' = TextEditor.update TESelectLeft m
    m'.Row |> Expect.equal "row unchanged" 0
    m'.Col |> Expect.equal "col unchanged" 0
    m'.SelectionAnchor |> Expect.equal "no anchor at doc start" None
  }
  test "TESelectRight at last line end stays put (document end)" {
    let m = { mk "abc\ndef" with Row = 1; Col = 3 }  // at 'f' end
    let m' = TextEditor.update TESelectRight m
    m'.Row |> Expect.equal "row unchanged" 1
    m'.Col |> Expect.equal "col unchanged" 3
    m'.SelectionAnchor |> Expect.equal "no anchor at doc end" None
  }
  test "TESelectWordLeft at document start (Row=0 Col=0) does not create anchor" {
    let m = mk "hello world"  // cursor at Row=0, Col=0
    let m' = TextEditor.update TESelectWordLeft m
    m'.Row |> Expect.equal "row unchanged" 0
    m'.Col |> Expect.equal "col unchanged" 0
    m'.SelectionAnchor |> Expect.equal "no anchor at doc start" None
  }
  test "TESelectWordRight at document end does not create anchor" {
    let m = { mk "hello world" with Col = 11 }  // at end of "hello world"
    let m' = TextEditor.update TESelectWordRight m
    m'.Col |> Expect.equal "col unchanged" 11
    m'.SelectionAnchor |> Expect.equal "no anchor at doc end" None
  }
  test "TESelectWordLeft normal word selection sets anchor correctly" {
    let m = { mk "hello world" with Col = 11 }  // cursor at end
    let m' = TextEditor.update TESelectWordLeft m
    m'.SelectionAnchor |> Expect.equal "anchor at original position" (Some (0, 11))
    m'.Col |> Expect.equal "jumped to start of 'world'" 6
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

// ── undo / redo ──────────────────────────────────────────────────────────────

let undoTests = testList "TextEditor.Undo" [
  test "updateWithUndo commits text edit to history" {
    let um = TextEditor.withUndo { mk "hello" with Col = 5 }
    let um' = TextEditor.updateWithUndo (TEInsertChar '!') um
    um' |> Undoable.canUndo |> Expect.isTrue "can undo after edit"
    um'.Present.Lines.[0] |> Expect.equal "text changed" "hello!"
  }
  test "updateWithUndo TEUndo restores previous state" {
    let um = TextEditor.withUndo { mk "hello" with Col = 5 }
    let um' = TextEditor.updateWithUndo (TEInsertChar '!') um
    let um'' = TextEditor.updateWithUndo TEUndo um'
    um''.Present.Lines.[0] |> Expect.equal "restored" "hello"
    um'' |> Undoable.canUndo |> Expect.isFalse "no more history"
  }
  test "updateWithUndo TERedo restores undone state" {
    let um = TextEditor.withUndo { mk "hello" with Col = 5 }
    let um' = um |> TextEditor.updateWithUndo (TEInsertChar '!')
                 |> TextEditor.updateWithUndo TEUndo
    let um'' = TextEditor.updateWithUndo TERedo um'
    um''.Present.Lines.[0] |> Expect.equal "redone" "hello!"
  }
  test "updateWithUndo does not commit cursor moves" {
    let um = TextEditor.withUndo (mk "hello")
    let um' = TextEditor.updateWithUndo TEMoveRight um
    um' |> Undoable.canUndo |> Expect.isFalse "cursor move is not undoable"
  }
  test "updateWithUndo does not commit selection messages" {
    let um = TextEditor.withUndo (mk "hello")
    let um' = TextEditor.updateWithUndo TESelectAll um
    um' |> Undoable.canUndo |> Expect.isFalse "selection is not undoable"
  }
  test "updateWithUndo TEBackspace commits and is undoable" {
    let um = TextEditor.withUndo { mk "hello" with Col = 5 }
    let um' = TextEditor.updateWithUndo TEBackspace um
    um'.Present.Lines.[0] |> Expect.equal "char deleted" "hell"
    um' |> Undoable.canUndo |> Expect.isTrue "backspace is undoable"
    let um'' = TextEditor.updateWithUndo TEUndo um'
    um''.Present.Lines.[0] |> Expect.equal "restored" "hello"
  }
  test "updateWithUndo multiline: newline commit and undo" {
    let um = TextEditor.withUndo { mk "ab" with Col = 1 }
    let um' = TextEditor.updateWithUndo TENewline um
    um'.Present.Lines |> Expect.hasLength "two lines" 2
    let um'' = TextEditor.updateWithUndo TEUndo um'
    um''.Present.Lines |> Expect.hasLength "one line restored" 1
    um''.Present.Lines.[0] |> Expect.equal "restored" "ab"
  }
  test "updateWithUndo idempotent: identical content does not grow history" {
    // TEBackspace at Col=0 produces identical text — commitIfChanged should NOT push
    let um = TextEditor.withUndo (mk "hello")  // Col=0, nothing to delete
    let um' = TextEditor.updateWithUndo TEBackspace um
    um' |> Undoable.canUndo |> Expect.isFalse "no-op does not create undo entry"
    um'.Present.Lines.[0] |> Expect.equal "unchanged" "hello"
  }
  test "updateWithUndoDepth caps history at given depth" {
    let um = TextEditor.withUndo (mk "")
    let um' =
      [1..10]
      |> List.fold (fun acc _ ->
          TextEditor.updateWithUndoDepth 3 (TEInsertChar 'x') acc) um
    // With depth=3, at most 3 entries in Past
    um'.Past |> List.length |> Expect.equal "history capped at 3" 3
  }
]



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
  test "view renders single-line selection as highlighted span" {
    let m = { mk "hello" with Col = 5; SelectionAnchor = Some (0, 0) }
    let el = TextEditor.view true 5 m
    match el with
    | Column (row0 :: _) ->
      match row0 with
      | Row [Styled(_, Text("hello", _))] -> ()
      | Row [Text("hello", _)] -> failtest "expected Styled selection, got plain Text"
      | _ -> failwithf "unexpected row: %A" row0
    | _ -> failtest "expected Column"
  }
  test "view renders partial-line selection: before, selected, after" {
    // "hello world", anchor at col 6, cursor at col 11 → selects "world"
    let m = { mk "hello world" with Col = 11; SelectionAnchor = Some (0, 6) }
    let el = TextEditor.view true 5 m
    match el with
    | Column (Row (before :: sel :: []) :: _) ->
      match before with
      | Text("hello ", _) -> ()
      | _ -> failwithf "expected 'hello ' before selection, got %A" before
      match sel with
      | Styled(_, Text("world", _)) -> ()
      | Text("world", _) -> failtest "selected span should be Styled"
      | _ -> failwithf "unexpected sel element: %A" sel
    | other -> failwithf "unexpected: %A" other
  }
  test "view renders multi-line selection: first row and last row" {
    // "abc\ndef", anchor at (0,1), cursor at (1,2)
    // Row 0: "a" plain + "bc" highlighted
    // Row 1: "de" highlighted + "f" plain
    let m = { mk "abc\ndef" with Row = 1; Col = 2; SelectionAnchor = Some (0, 1) }
    let el = TextEditor.view true 5 m
    match el with
    | Column (row0 :: row1 :: _) ->
      match row0 with
      | Row (Text("a", _) :: Styled(_, Text("bc", _)) :: []) -> ()
      | Row (Text("a", _) :: Text("bc", _) :: []) ->
        failtest "row 0 tail should be Styled (selected)"
      | _ -> failwithf "row 0 unexpected: %A" row0
      match row1 with
      | Row (Styled(_, Text("de", _)) :: Text("f", _) :: []) -> ()
      | Row (Text("de", _) :: Text("f", _) :: []) ->
        failtest "row 1 head should be Styled (selected)"
      | _ -> failwithf "row 1 unexpected: %A" row1
    | _ -> failtest "expected Column with at least 2 rows"
  }
  test "view renders emoji cursor without broken surrogate" {
    // U+1F600 😀 → surrogate pair \uD83D\uDE00 in .NET; col=1 points to high surrogate
    let m = { mk "a\U0001F600b" with Col = 1 }
    let el = TextEditor.view true 5 m
    match el with
    | Column (Row children :: _) ->
      match children with
      | [Text("a", _); Styled(_, Text(emoji, _)); Text("b", _)]
      | [Text("a", _); Text(emoji, _); Text("b", _)] ->
        emoji |> Expect.equal "emoji preserved" "\U0001F600"
      | _ -> failwithf "unexpected children: %A" children
    | _ -> failtest "expected Column"
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
  undoTests
  viewTests
]

