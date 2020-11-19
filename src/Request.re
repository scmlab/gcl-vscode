type filepath = string;

module Kind = {
  type t =
    | Load
    | Inspect(int, int)
    | Refine(int, string)
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | Debug;

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Load => object_([("tag", string("ReqLoad"))])
    | Inspect(start, end_) =>
      object_([
        ("tag", string("ReqInspect")),
        ("contents", (start, end_) |> tuple2(int, int)),
      ])
    | Refine(id, payload) =>
      object_([
        ("tag", string("ReqRefine")),
        ("contents", (id, payload) |> tuple2(int, string)),
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
    | Debug => object_([("tag", string("ReqDebug"))]);
};

type t =
  | Req(filepath, Kind.t);

open Json.Encode;
let encode: encoder(t) =
  fun
  | Req(filepath, kind) => (filepath, kind) |> pair(string, Kind.encode);
