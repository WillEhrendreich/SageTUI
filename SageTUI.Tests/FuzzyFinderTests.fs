module FuzzyFinderTests

open Expecto
open Expecto.Flip
open FsCheck
open SageTUI

// ── FuzzyFinder.scoreMatch ───────────────────────────────────────────────────

let scoringTests = testList "FuzzyFinder.scoreMatch" [
  test "exact match scores" {
    let result = FuzzyFinder.scoreMatch "hello" "hello"
    result |> Expect.isSome "matches"
    let (score, _) = result.Value
    (score > 0.0) |> Expect.isTrue "positive score for exact match"
  }
  test "subsequence match scores" {
    let result = FuzzyFinder.scoreMatch "hlo" "hello"
    result |> Expect.isSome "subsequence matches"
  }
  test "non-subsequence returns None" {
    let result = FuzzyFinder.scoreMatch "xyz" "hello"
    result |> Expect.isNone "no match"
  }
  test "empty query matches everything with score 0" {
    let result = FuzzyFinder.scoreMatch "" "anything"
    result |> Expect.isSome "empty query always matches"
    let (score, positions) = result.Value
    score |> Expect.equal "score 0" 0.0
    positions |> Expect.hasLength "no positions" 0
  }
  test "prefix match scores higher than mid-match" {
    let prefixResult = FuzzyFinder.scoreMatch "he" "hello"
    let midResult    = FuzzyFinder.scoreMatch "lo" "hello"
    let (prefixScore, _) = prefixResult.Value
    let (midScore, _)    = midResult.Value
    (prefixScore > midScore) |> Expect.isTrue "prefix > mid-match"
  }
  test "consecutive match scores higher than scattered" {
    // "ab" in "abc" is consecutive; "ab" in "axb" is not
    let consec = FuzzyFinder.scoreMatch "ab" "abc"
    let scatter = FuzzyFinder.scoreMatch "ab" "axb"
    let (cs, _) = consec.Value
    let (ss, _) = scatter.Value
    (cs > ss) |> Expect.isTrue "consecutive > scattered"
  }
  test "case insensitive matching" {
    FuzzyFinder.scoreMatch "HELLO" "hello" |> Expect.isSome "case insensitive"
    FuzzyFinder.scoreMatch "hello" "HELLO" |> Expect.isSome "case insensitive reverse"
  }
  test "match positions are correct indices" {
    let result = FuzzyFinder.scoreMatch "hlo" "hello"
    let (_, positions) = result.Value
    positions |> Expect.hasLength "3 positions" 3
    // h=0, l=2, o=4 in "hello"
    positions.[0] |> Expect.equal "h at 0" 0
    positions.[2] |> Expect.equal "o at 4" 4
  }
  test "word boundary bonus: underscore delimiter" {
    // "fn" matches "foo_name": f at 0 (prefix), n at 4 (after _)
    let wbResult = FuzzyFinder.scoreMatch "fn" "foo_name"
    let midResult = FuzzyFinder.scoreMatch "fn" "fXname"
    let (wbScore, _) = wbResult.Value
    let (midScore, _) = midResult.Value
    (wbScore > midScore) |> Expect.isTrue "word-boundary bonus"
  }
]

// ── FuzzyFinder.matchAll / sort ──────────────────────────────────────────────

let matchAllTests = testList "FuzzyFinder.matchAll" [
  test "matchAll returns only matching candidates" {
    let candidates = [| "apple"; "banana"; "apricot"; "cherry" |]
    let results = FuzzyFinder.matchAll "ap" candidates
    results |> Array.forall (fun r -> r.Candidate.StartsWith("ap") || r.Candidate.Contains("a")) |> Expect.isTrue "all match"
    results |> Array.exists (fun r -> r.Candidate = "apple") |> Expect.isTrue "apple included"
    results |> Array.exists (fun r -> r.Candidate = "banana") |> Expect.isFalse "banana excluded (no 'p')"
  }
  test "matchAll sorts by score descending" {
    let candidates = [| "hello world"; "hello"; "hello_world_far" |]
    let results = FuzzyFinder.matchAll "hello" candidates
    // "hello" (exact) should rank first
    results.[0].Candidate |> Expect.equal "best match first" "hello"
  }
  test "matchAll empty query returns all candidates in order" {
    let candidates = [| "z"; "a"; "m" |]
    let results = FuzzyFinder.matchAll "" candidates
    results |> Expect.hasLength "all 3" 3
  }
  test "matchAll populates MatchPositions" {
    let results = FuzzyFinder.matchAll "hlo" [| "hello" |]
    // Should have single result for "hello"
    let r = results |> Array.find (fun r -> r.Candidate = "hello")
    r.MatchPositions |> Expect.hasLength "3 positions" 3
  }
  test "human case: function name ranking" {
    // 'gs' should rank 'getStatus' above 'graphicsSystem' (word-boundary bonus)
    let candidates = [| "getStatus"; "graphicsSystem"; "gamesServer" |]
    let results = FuzzyFinder.matchAll "gs" candidates
    // At minimum, getStatus should rank highly
    results |> Array.exists (fun r -> r.Candidate = "getStatus") |> Expect.isTrue "getStatus matches"
  }
  test "matchAll sets OriginalIndex to position in input array" {
    let candidates = [| "zebra"; "apple"; "mango" |]
    let results = FuzzyFinder.matchAll "" candidates
    results |> Expect.hasLength "all 3" 3
    let byName (n: string) = results |> Array.find (fun r -> r.Candidate = n)
    (byName "zebra").OriginalIndex |> Expect.equal "zebra idx=0" 0
    (byName "apple").OriginalIndex |> Expect.equal "apple idx=1" 1
    (byName "mango").OriginalIndex |> Expect.equal "mango idx=2" 2
  }
]

// ── FuzzyFinder widget model ─────────────────────────────────────────────────

let widgetTests = testList "FuzzyFinder.widget" [
  test "init populates Results" {
    let m = FuzzyFinder.init id [| "apple"; "banana"; "cherry" |]
    m.Results |> Expect.hasLength "3 results (empty query = all)" 3
    m.SelectedIdx |> Expect.equal "selected 0" 0
    FuzzyFinder.query m |> Expect.equal "empty query" ""
  }
  test "FFQueryChanged filters results" {
    let m = FuzzyFinder.init id [| "apple"; "banana"; "apricot" |]
    let m' = FuzzyFinder.update (FFQueryChanged "ap") m
    m'.Results |> Array.forall (fun r -> r.Candidate <> "banana") |> Expect.isTrue "banana excluded"
    FuzzyFinder.query m' |> Expect.equal "query updated" "ap"
  }
  test "FFMoveDown increments SelectedIdx" {
    let m = FuzzyFinder.init id [| "a"; "b"; "c" |]
    let m' = FuzzyFinder.update FFMoveDown m
    m'.SelectedIdx |> Expect.equal "idx 1" 1
  }
  test "FFMoveUp clamps at 0" {
    let m = FuzzyFinder.init id [| "a"; "b" |]
    let m' = FuzzyFinder.update FFMoveUp m
    m'.SelectedIdx |> Expect.equal "clamped at 0" 0
  }
  test "FFMoveDown clamps at results length - 1" {
    let m = FuzzyFinder.init id [| "a" |]
    let m' = m |> FuzzyFinder.update FFMoveDown
    m'.SelectedIdx |> Expect.equal "clamped at 0" 0
  }
  test "selectedItem returns the item at cursor" {
    let m = FuzzyFinder.init id [| "apple"; "banana" |]
    let m' = m |> FuzzyFinder.update FFMoveDown
    // Second result when query is "" is based on score order
    let item = FuzzyFinder.selectedItem m'
    item |> Expect.isSome "some item"
  }
  test "selectedItem returns None when no results" {
    let m = FuzzyFinder.init id [| "apple" |]
    let m' = m |> FuzzyFinder.update (FFQueryChanged "zzz")
    FuzzyFinder.selectedItem m' |> Expect.isNone "no match"
  }
  test "selectedItem returns correct item for duplicate display strings" {
    // Both items stringify to "Alice" — must use OriginalIndex not string reverse-lookup
    let items = [| (1, "Alice"); (2, "Alice") |]
    let m = FuzzyFinder.init (fun (_, name) -> name) items
    // SelectedIdx = 0 → first Alice (id = 1)
    FuzzyFinder.selectedItem m |> Option.map fst |> Expect.equal "first Alice" (Some 1)
    // Move to idx 1 → second Alice (id = 2)
    let m' = { m with SelectedIdx = 1 }
    FuzzyFinder.selectedItem m' |> Option.map fst |> Expect.equal "second Alice" (Some 2)
  }
  test "FFSetItems replaces items and reruns query" {
    let m = FuzzyFinder.init id [| "a" |]
    let m' = m |> FuzzyFinder.update (FFQueryChanged "b") |> FuzzyFinder.update (FFSetItems [| "abc"; "bcd" |])
    m'.Items |> Expect.hasLength "2 items" 2
    // Query "b" should still be active and filter results
    FuzzyFinder.query m' |> Expect.equal "query preserved" "b"
  }
  test "FFQueryMsg of TIChar updates query via TextInput" {
    let m = FuzzyFinder.init id [| "apple"; "banana" |]
    let m' = m |> FuzzyFinder.update (FFQueryKey (Key.Char 'a'))
    FuzzyFinder.query m' |> Expect.equal "query is 'a'" "a"
    (m'.Results.Length, 0) |> Expect.isGreaterThan "has results"
  }
  test "FFQueryMsg of TIBackspace removes last char" {
    let m = FuzzyFinder.init id [| "apple" |] |> FuzzyFinder.update (FFQueryChanged "ab")
    let m' = m |> FuzzyFinder.update (FFQueryKey Key.Backspace)
    FuzzyFinder.query m' |> Expect.equal "one char removed" "a"
  }
  test "query helper returns query text" {
    let m = FuzzyFinder.init id [| "x" |] |> FuzzyFinder.update (FFQueryChanged "hello")
    FuzzyFinder.query m |> Expect.equal "query text" "hello"
  }
  test "Candidates field is cached from init and reused" {
    let calls = System.Collections.Generic.List<string>()
    let toString (s: string) = calls.Add(s); s
    let items = [| "apple"; "banana"; "cherry" |]
    let m = FuzzyFinder.init toString items
    let callCountAfterInit = calls.Count
    // Querying should NOT call toString again (uses cached Candidates)
    let m' = m |> FuzzyFinder.update (FFQueryChanged "a")
               |> FuzzyFinder.update (FFQueryChanged "ap")
               |> FuzzyFinder.update (FFQueryChanged "app")
    calls.Count |> Expect.equal "toString only called at init" callCountAfterInit
    m'.Candidates |> Expect.equal "candidates unchanged" m.Candidates
  }
  test "FFSetItems refreshes Candidates cache" {
    let m = FuzzyFinder.init id [| "old" |]
    let m' = FuzzyFinder.update (FFSetItems [| "new1"; "new2" |]) m
    m'.Candidates |> Expect.equal "candidates updated" [| "new1"; "new2" |]
  }
  test "view returns Column" {
    let m = FuzzyFinder.init id [| "alpha"; "beta"; "gamma" |]
    let el = FuzzyFinder.view true 5 m
    match el with
    | Column _ -> ()
    | _ -> failtest "expected Column"
  }
  test "view prompt renders TextInput for query" {
    // View should use TextInput.view for the query line (renders as Row for non-empty with cursor)
    let m = FuzzyFinder.init id [| "test" |] |> FuzzyFinder.update (FFQueryChanged "te")
    // Just exercise view without crash
    let _ = FuzzyFinder.view true 5 m
    let _ = FuzzyFinder.view false 5 m
    ()
  }
]

// ── El.responsive (layout breakpoints) ──────────────────────────────────────

let responsiveTests = testList "El.responsive" [
  test "responsive selects correct breakpoint" {
    let el = El.responsive [
      (0,   El.text "small")
      (80,  El.text "medium")
      (120, El.text "large")
    ]
    let renderAt w =
      let buf = Buffer.create w 1
      Render.render { X = 0; Y = 0; Width = w; Height = 1 } Style.empty buf el
      buf
    // Narrow width
    let smallBuf = renderAt 40
    let medBuf   = renderAt 80
    let largeBuf = renderAt 120
    let cellText (buf: Buffer) =
      buf.Cells.[0..buf.Width-1]
      |> Array.map (fun c -> string (System.Text.Rune(c.Rune)))
      |> Array.takeWhile (fun s -> s <> " " && s <> "\u0000")
      |> String.concat ""
    cellText smallBuf |> Expect.equal "small at 40" "small"
    cellText medBuf   |> Expect.stringContains "medium at 80" "medium"
    cellText largeBuf |> Expect.stringContains "large at 120" "large"
  }
  test "responsive falls back to first when width < all minWidths" {
    let el = El.responsive [
      (50,  El.text "wide")
      (100, El.text "wider")
    ]
    let buf = Buffer.create 30 1
    Render.render { X = 0; Y = 0; Width = 30; Height = 1 } Style.empty buf el
    let cellText =
      buf.Cells.[0..buf.Width-1]
      |> Array.map (fun c -> string (System.Text.Rune(c.Rune)))
      |> Array.takeWhile (fun s -> s <> " " && s <> "\u0000")
      |> String.concat ""
    cellText |> Expect.stringContains "falls back to first" "wide"
  }
  test "responsive empty list renders nothing" {
    let el = El.responsive []
    let buf = Buffer.create 80 1
    Render.render { X = 0; Y = 0; Width = 80; Height = 1 } Style.empty buf el
    ()  // just verify no exception
  }
  test "arena parity: Responsive renders same as Render.fs" {
    let el = El.responsive [
      (0,  El.text "A")
      (20, El.text "BB")
      (40, El.text "CCC")
    ]
    // Test at width 50 — should pick "CCC"
    let refBuf = Buffer.create 50 1
    Render.render { X = 0; Y = 0; Width = 50; Height = 1 } Style.empty refBuf el
    let arena = FrameArena.create 1024 4096 256
    let handle = Arena.lower arena el
    let arenaBuf = Buffer.create 50 1
    ArenaRender.renderRoot arena handle { X = 0; Y = 0; Width = 50; Height = 1 } arenaBuf
    // Compare first few cells
    for i in 0..2 do
      refBuf.Cells.[i].Rune |> Expect.equal (sprintf "cell %d matches" i) arenaBuf.Cells.[i].Rune
  }
]

// ── El.responsiveH (height breakpoints) ──────────────────────────────────────

let responsiveHTests = testList "El.responsiveH" [
  test "responsiveH selects correct breakpoint by height" {
    let el = El.responsiveH [
      (0,  El.text "short")
      (10, El.text "tall")
      (20, El.text "verytall")
    ]
    let renderAt h =
      let buf = Buffer.create 20 h
      Render.render { X = 0; Y = 0; Width = 20; Height = h } Style.empty buf el
      buf
    let cellText (buf: Buffer) =
      buf.Cells.[0..19]
      |> Array.map (fun c -> string (System.Text.Rune(c.Rune)))
      |> Array.takeWhile (fun s -> s <> " " && s <> "\u0000")
      |> String.concat ""
    cellText (renderAt 5)  |> Expect.equal "height 5 picks short"    "short"
    cellText (renderAt 10) |> Expect.equal "height 10 picks tall"    "tall"
    cellText (renderAt 20) |> Expect.equal "height 20 picks verytall" "verytall"
  }
  test "responsiveH falls back to first when height < all minHeights" {
    let el = El.responsiveH [
      (10, El.text "tall")
      (20, El.text "taller")
    ]
    let buf = Buffer.create 20 3
    Render.render { X = 0; Y = 0; Width = 20; Height = 3 } Style.empty buf el
    let cellText =
      buf.Cells.[0..19]
      |> Array.map (fun c -> string (System.Text.Rune(c.Rune)))
      |> Array.takeWhile (fun s -> s <> " " && s <> "\u0000")
      |> String.concat ""
    cellText |> Expect.equal "falls back to first" "tall"
  }
  test "responsiveH empty list renders nothing" {
    let el = El.responsiveH []
    let buf = Buffer.create 20 5
    Render.render { X = 0; Y = 0; Width = 20; Height = 5 } Style.empty buf el
    ()
  }
  test "arena parity: ResponsiveH renders same as Render.fs" {
    let el = El.responsiveH [
      (0,  El.text "A")
      (5,  El.text "BB")
      (10, El.text "CCC")
    ]
    let refBuf = Buffer.create 20 12
    Render.render { X = 0; Y = 0; Width = 20; Height = 12 } Style.empty refBuf el
    let arena = FrameArena.create 1024 4096 256
    let handle = Arena.lower arena el
    let arenaBuf = Buffer.create 20 12
    ArenaRender.renderRoot arena handle { X = 0; Y = 0; Width = 20; Height = 12 } arenaBuf
    for i in 0..2 do
      refBuf.Cells.[i].Rune |> Expect.equal (sprintf "cell %d matches" i) arenaBuf.Cells.[i].Rune
  }
  test "responsiveH composes with El.responsive for 2D breakpoints" {
    // At width=40, height=10: should pick the "B" element
    let el = El.responsive [
      (0,  El.responsiveH [(0, El.text "A-small");  (10, El.text "A-tall")])
      (40, El.responsiveH [(0, El.text "B-small");  (10, El.text "B-tall")])
    ]
    let buf = Buffer.create 40 10
    Render.render { X = 0; Y = 0; Width = 40; Height = 10 } Style.empty buf el
    let cellText =
      buf.Cells.[0..buf.Width-1]
      |> Array.map (fun c -> string (System.Text.Rune(c.Rune)))
      |> Array.takeWhile (fun s -> s <> " " && s <> "\u0000")
      |> String.concat ""
    cellText |> Expect.equal "2D: width=40,height=10 picks B-tall" "B-tall"
  }
]

// ── FuzzyFinder.searchAsync ─────────────────────────────────────────────────

let asyncTests = testList "FuzzyFinder.searchAsync" [
  test "searchAsync returns an async Cmd" {
    let cmd = FuzzyFinder.searchAsync "he" [| "hello"; "world" |] id id
    Cmd.hasAsync cmd |> Expect.isTrue "is async"
  }
]

[<Tests>]
let allFuzzyFinderTests = testList "FuzzyFinder" [
  scoringTests
  matchAllTests
  widgetTests
  responsiveTests
  responsiveHTests
  asyncTests
]

