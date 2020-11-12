type t =
  | Load
  | Quit
  | Reload
  | Refine
  | Debug;

let names = [|
  (Reload, "reload"),
  (Refine, "refine"),
  (Debug, "debug-gcl"),
|];
