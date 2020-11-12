type filepath = string;
type t =
  | Load(filepath)
  | Refine(filepath, int, string)
  | Substitute(filepath, int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
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
    | Refine(filepath, id, payload) =>
      object_([
        ("tag", string("ReqRefine")),
        (
          "contents",
          (filepath, id, payload) |> tuple3(string, int, string),
        ),
      ])
    | Substitute(filepath, i, expr, subst) =>
      object_([
        ("tag", string("ReqSubstitute")),
        (
          "contents",
          (filepath, i, expr, subst)
          |> tuple4(
               string,
               int,
               GCL.Syntax.Expr.encode,
               GCL.Syntax.Expr.encodeSubst,
             ),
        ),
      ])
    | Debug => object_([("tag", string("ReqDebug"))]);
};

let encode: t => Js.Json.t = x => x->Encode.request;
