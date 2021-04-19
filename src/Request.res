type filepath = string
type source = string

module Kind = {
  type t =
    | Inspect(int, int)
    | Refine(int, int)
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | ExportProofObligations
    | Debug

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Inspect(start, end_) =>
      object_(list{("tag", string("ReqInspect")), ("contents", (start, end_) |> tuple2(int, int))})
    | Refine(start, end_) => object_(list{("tag", string("ReqRefine")), ("contents", (start, end_) |> tuple2(int, int))})
    | Substitute(i, expr, subst) =>
      object_(list{
        ("tag", string("ReqSubstitute")),
        (
          "contents",
          (i, expr, subst) |> tuple3(int, GCL.Syntax.Expr.encode, GCL.Syntax.Expr.encodeSubst),
        ),
      })
    | Debug => object_(list{("tag", string("ReqDebug"))})
    | ExportProofObligations => object_(list{("tag", string("ReqExportProofObligations"))})
    }
}

type t = Req(filepath, Kind.t)
open Json.Encode
let encode: encoder<t> = x =>
  switch x {
  | Req(filepath, kind) => (filepath, kind) |> pair(string, Kind.encode)
  }
