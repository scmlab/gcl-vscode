type t =
  | Load
  | Quit
  | Reload
  | Refine
  | InsertAssertion
  | Debug;

let names = [|
  (Reload, "reload"),
  (Refine, "refine"),
  (InsertAssertion, "insert-assertion"),
  (Debug, "debug-gcl"),
|];