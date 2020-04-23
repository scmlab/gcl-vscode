type t =
  | Reload
  | Refine
  | InsertAssertion
  | Debug;

let names = [|
  (Reload, "reload"),
  (Refine, "refine"),
  (InsertAssertion, "insert-assertion"),
  (Debug, "debug"),
|];