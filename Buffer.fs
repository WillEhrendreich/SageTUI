namespace SageTUI

open System
open System.Text
open System.Runtime.InteropServices

/// 16-byte blittable cell for SIMD-accelerated diff.
/// One cell = one Vector128<byte> comparison.
[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type PackedCell =
  { Rune: int32
    Fg: int32
    Bg: int32
    Attrs: uint16
    _pad: uint16 }

module PackedCell =
  let empty =
    { Rune = int (Rune ' ').Value
      Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }

  /// Sentinel written at col X+1 after a wide (2-column) character at col X.
  /// The Presenter skips cells with Rune=0 — they are visually occupied by the
  /// left half of the preceding wide character.
  let wideContinuation =
    { Rune = 0; Fg = 0; Bg = 0; Attrs = 0us; _pad = 0us }

  let create rune fg bg attrs =
    { Rune = rune; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us }

/// Column width for terminal rendering.
module RuneWidth =
  let getColumnWidth (r: Rune) =
    let v = r.Value
    match v with
    | _ when v < 0x20 -> 0
    | _ when
        v >= 0x1100 &&
        (v <= 0x115F || (v >= 0x2E80 && v <= 0x9FFF) ||
         (v >= 0xAC00 && v <= 0xD7A3) || (v >= 0xF900 && v <= 0xFAFF) ||
         (v >= 0xFE10 && v <= 0xFE6F) || (v >= 0xFF01 && v <= 0xFF60) ||
         (v >= 0xFFE0 && v <= 0xFFE6) || (v >= 0x20000 && v <= 0x2FFFD) ||
         (v >= 0x30000 && v <= 0x3FFFD)) -> 2
    | _ -> 1

/// Flat 1D buffer. Index = row * width + col.
/// Mutable by design — write-once-per-frame, SIMD diff, present, swap.
type Buffer =
  { Cells: PackedCell array
    Width: int
    Height: int }

module Buffer =
  let emptyCell = PackedCell.empty

  let create width height =
    { Cells = Array.create (width * height) emptyCell
      Width = width; Height = height }

  let get x y buf =
    if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height
    then buf.Cells[y * buf.Width + x]
    else emptyCell

  let set x y cell buf =
    if x >= 0 && x < buf.Width && y >= 0 && y < buf.Height
    then buf.Cells[y * buf.Width + x] <- cell

  /// Legacy allocating path — used by Render.fs (reference implementation).
  /// For new code, prefer writeCharSpan which avoids the intermediate string and
  /// hoists the loop-invariant y bounds check.
  let writeString x y (fg: int32) (bg: int32) (attrs: uint16) (text: string) buf =
    if y >= 0 && y < buf.Height then
      let mutable col = x
      for rune in text.EnumerateRunes() do
        let w = RuneWidth.getColumnWidth rune
        if col >= 0 && col < buf.Width then
          buf.Cells[y * buf.Width + col] <-
            { Rune = rune.Value; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us }
          if w = 2 && col + 1 < buf.Width then
            buf.Cells[y * buf.Width + col + 1] <- PackedCell.wideContinuation
        col <- col + w

  /// Zero-allocation text write from a pre-allocated char array slice.
  /// Used by ArenaRender to avoid creating intermediate strings per frame.
  let writeCharSpan x y (fg: int32) (bg: int32) (attrs: uint16) (chars: char array) (start: int) (len: int) (maxWidth: int) buf =
    // Hoist the y bounds check — y never changes inside the rune loop.
    if y >= 0 && y < buf.Height then
      let span = System.ReadOnlySpan<char>(chars, start, len)
      let mutable col = x
      for rune in span.EnumerateRunes() do
        let w = RuneWidth.getColumnWidth rune
        // Use full-fit check (col - x + w <= maxWidth) to match Render.fs's behaviour:
        // a wide character that would partially overflow the area is not written.
        if col - x + w <= maxWidth && col >= 0 && col < buf.Width then
          buf.Cells[y * buf.Width + col] <-
            { Rune = rune.Value; Fg = fg; Bg = bg; Attrs = attrs; _pad = 0us }
          if w = 2 && col + 1 < buf.Width then
            buf.Cells[y * buf.Width + col + 1] <- PackedCell.wideContinuation
        col <- col + w

  let clear buf =
    System.Array.Fill(buf.Cells, emptyCell)

  /// SIMD-accelerated diff via chunk-skip pattern. Fills a pre-allocated ResizeArray.
  /// Span.SequenceEqual is JIT-vectorized in .NET 8+.
  /// Pre-allocating `changes` in the caller keeps this allocation-free between frames.
  let diffInto (changes: ResizeArray<int>) (prev: Buffer) (curr: Buffer) =
    changes.Clear()
    let prevBytes = MemoryMarshal.AsBytes(System.ReadOnlySpan(prev.Cells))
    let currBytes = MemoryMarshal.AsBytes(System.ReadOnlySpan(curr.Cells))
    let cellSz = sizeof<PackedCell>
    let chunkCells = 16
    let chunkBytes = chunkCells * cellSz
    let totalCells = curr.Cells.Length
    let mutable i = 0
    while i + chunkCells <= totalCells do
      let s = i * cellSz
      if not (prevBytes.Slice(s, chunkBytes).SequenceEqual(currBytes.Slice(s, chunkBytes))) then
        for j in i .. i + chunkCells - 1 do
          let bs = j * cellSz
          if not (prevBytes.Slice(bs, cellSz).SequenceEqual(currBytes.Slice(bs, cellSz))) then
            changes.Add(j)
      i <- i + chunkCells
    while i < totalCells do
      let bs = i * cellSz
      if not (prevBytes.Slice(bs, cellSz).SequenceEqual(currBytes.Slice(bs, cellSz))) then
        changes.Add(i)
      i <- i + 1

  /// SIMD-accelerated diff via chunk-skip pattern.
  /// Allocates a new ResizeArray on every call. Prefer diffInto for hot paths.
  let diff (prev: Buffer) (curr: Buffer) =
    let changes = ResizeArray<int>()
    diffInto changes prev curr
    changes

  let toString buf =
    let sb = StringBuilder()
    for y in 0 .. buf.Height - 1 do
      for x in 0 .. buf.Width - 1 do
        let cell = buf.Cells[y * buf.Width + x]
        match cell.Rune with
        | 0 -> sb.Append(' ') |> ignore  // continuation cell for wide char — render as space
        | v -> sb.Append(Rune(v).ToString()) |> ignore
      if y < buf.Height - 1 then sb.Append('\n') |> ignore
    sb.ToString()
