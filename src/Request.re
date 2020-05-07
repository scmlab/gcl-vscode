type t =
  | Load(string, GCL.mode)
  | Refine(int, string)
  | InsertAssertion(int)
  | Debug;

module Encode = {
  open Json.Encode;
  let request: encoder(t) =
    fun
    | Load(filepath, GCL.WP1) =>
      object_([
        ("tag", string("ReqLoad")),
        ("contents", (filepath, false) |> pair(string, bool)),
      ])
    | Load(filepath, GCL.WP2) =>
      object_([
        ("tag", string("ReqLoad")),
        ("contents", (filepath, true) |> pair(string, bool)),
      ])
    | Refine(id, payload) =>
      object_([
        ("tag", string("ReqRefine")),
        ("contents", (id, payload) |> pair(int, string)),
      ])
    | InsertAssertion(n) =>
      object_([
        ("tag", string("ReqInsertAssertion")),
        ("contents", int(n)),
      ])
    | Debug => object_([("tag", string("ReqDebug"))]);
};

let encode: t => string = x => x->Encode.request->Json.stringify;