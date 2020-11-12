type t =
  | Load(string)
  | Refine(int, string)
  | InsertAssertion(int)
  | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
  | Debug;

module Encode = {
  open Json.Encode;
  let request: encoder(t) =
    fun
    | Load(filepath) =>
      object_([
        ("tag", string("ReqLoad")),
        ("contents", filepath |> string),
      ])
    | Refine(id, payload) =>
      object_([
        ("tag", string("ReqRefine")),
        ("contents", (id, payload) |> pair(int, string)),
      ])
    | Substitute(i, expr, subst) =>
      object_([
        ("tag", string("ReqSubstitute")),
        (
          "contents",
          (i, expr, subst)
          |> tuple3(int, GCL.Syntax.Expr.encode, GCL.Syntax.Expr.encodeSubst),
        ),
      ])
    | InsertAssertion(n) =>
      object_([
        ("tag", string("ReqInsertAssertion")),
        ("contents", int(n)),
      ])
    | Debug => object_([("tag", string("ReqDebug"))]);
};

let encode: t => Js.Json.t = x => x->Encode.request;
