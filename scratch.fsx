// Scratch script for interactive SageFs validation.
// Run via sagefs-load_fsharp_script or `dotnet fsi scratch.fsx`

#r @"bin/Debug/net10.0/SageTUI.Library.dll"

open SageTUI

// ── SlideIn smoke tests ──────────────────────────────────────────────────────
let checkSlideIn () =
  let buf = Buffer.create 4 1
  let snap = Array.init 4 (fun _ -> PackedCell.create (int 'O') 0 0 0us)
  let cur  = Array.init 4 (fun _ -> PackedCell.create (int 'N') 0 0 0us)

  TransitionFx.applySlideIn 0.0 Direction.Right snap cur 0 4 1 buf
  let r0 = buf.Cells |> Array.map (fun c -> char c.Rune) |> System.String
  printfn "SlideIn Right t=0  (expect OOOO): %s  %s" r0 (if r0 = "OOOO" then "✓" else "✗ FAIL")

  TransitionFx.applySlideIn 1.0 Direction.Right snap cur 0 4 1 buf
  let r1 = buf.Cells |> Array.map (fun c -> char c.Rune) |> System.String
  printfn "SlideIn Right t=1  (expect NNNN): %s  %s" r1 (if r1 = "NNNN" then "✓" else "✗ FAIL")

  let cur2 = [| PackedCell.create (int 'A') 0 0 0us; PackedCell.create (int 'B') 0 0 0us
                PackedCell.create (int 'C') 0 0 0us; PackedCell.create (int 'D') 0 0 0us |]
  TransitionFx.applySlideIn 0.5 Direction.Right snap cur2 0 4 1 buf
  let r2 = buf.Cells |> Array.map (fun c -> char c.Rune) |> System.String
  printfn "SlideIn Right t=.5 (expect OOAB): %s  %s" r2 (if r2 = "OOAB" then "✓" else "✗ FAIL")

  TransitionFx.applySlideIn 0.0 Direction.Left snap cur 0 4 1 buf
  let r3 = buf.Cells |> Array.map (fun c -> char c.Rune) |> System.String
  printfn "SlideIn Left  t=0  (expect OOOO): %s  %s" r3 (if r3 = "OOOO" then "✓" else "✗ FAIL")

  TransitionFx.applySlideIn 1.0 Direction.Left snap cur 0 4 1 buf
  let r4 = buf.Cells |> Array.map (fun c -> char c.Rune) |> System.String
  printfn "SlideIn Left  t=1  (expect NNNN): %s  %s" r4 (if r4 = "NNNN" then "✓" else "✗ FAIL")

checkSlideIn ()

// ── Grow smoke tests ─────────────────────────────────────────────────────────
let checkGrow () =
  let snap4 = Array.init 16 (fun _ -> PackedCell.create (int 'O') 0 0 0us)
  let cur4  = Array.init 16 (fun _ -> PackedCell.create (int 'N') 0 0 0us)

  let buf4 = Buffer.create 4 4
  TransitionFx.applyGrow 1.0 snap4 cur4 0 4 4 buf4
  let allN = buf4.Cells |> Array.forall (fun c -> c.Rune = int 'N')
  printfn "Grow t=1 all current:  %s" (if allN then "✓" else "✗ FAIL")

  let buf4b = Buffer.create 4 4
  TransitionFx.applyGrow 0.0 snap4 cur4 0 4 4 buf4b
  let cornersO = [0; 3; 12; 15] |> List.forall (fun i -> buf4b.Cells.[i].Rune = int 'O')
  printfn "Grow t=0 corners=snap: %s" (if cornersO then "✓" else "✗ FAIL")

checkGrow ()

// ── Sequence duration ────────────────────────────────────────────────────────
let dur = TransitionFx.applySequenceDuration [ Fade 200<ms>; Wipe(Direction.Right, 300<ms>) ]
printfn "SequenceDuration(200+300): %d  %s" dur (if dur = 500 then "✓" else "✗ FAIL")
