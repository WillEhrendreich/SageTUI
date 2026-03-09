#r "bin/Debug/net10.0/SageTUI.Library.dll"
open SageTUI
let mutable written = ResizeArray<string>()
let backend: TerminalBackend = {
  Size = fun () -> 20, 5
  Write = fun s -> written.Add(s)
  Flush = fun () -> ()
  PollEvent = fun _ -> None
  EnterRawMode = fun () -> ()
  LeaveRawMode = fun () -> ()
  Profile = SafeProfile.minimum 20 5
}
let prog: Program<int, Key> = {
  Init = fun () -> 0, Cmd.none
  Update = fun msg m -> m, Cmd.quit
  View = fun m -> El.text (sprintf "count:%d" m)
  Subscribe = fun _ -> [ KeySub (fun (k,_) -> Some k) ]
}
App.runWithBackend backend prog
printfn "Writes: %d" written.Count
for s in written do printfn "  '%s'" (s.Replace(char 0x1b, 'E'))
