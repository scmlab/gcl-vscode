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

let parse =
  fun
  | "reload" => Reload
  | "refine" => Refine
  | "insert-assertion" => InsertAssertion
  | "debug" => Debug
  | _ => Reload;