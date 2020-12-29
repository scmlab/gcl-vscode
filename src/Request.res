type filepath = string
type source = string

module Kind = {
  type t =
    | Load
    | Inspect(int, int)
    | Refine(int, string)
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | ExportProofObligations
    | Debug

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Load => object_(list{("tag", string("ReqLoad"))})
    | Inspect(start, end_) =>
      object_(list{("tag", string("ReqInspect")), ("contents", (start, end_) |> tuple2(int, int))})
    | Refine(id, payload) =>
      object_(list{
        ("tag", string("ReqRefine")),
        ("contents", (id, payload) |> tuple2(int, string)),
      })
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

type t = Req(filepath, source, Kind.t)
open Json.Encode
let encode: encoder<t> = x =>
  switch x {
  | Req(filepath, source, kind) => (filepath, source, kind) |> tuple3(string, string, Kind.encode)
  }
