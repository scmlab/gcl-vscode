type t =
  | Load
  | Quit
  | Reload
  | Refine
  | InsertAssertion;

let names = [|
  (Reload, "reload"),
  (Refine, "refine"),
  (InsertAssertion, "insert-assertion"),
|];