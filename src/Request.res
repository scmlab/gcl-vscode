type filepath = string
type source = string

module Kind = {
  type t =
    | Inspect(int, int)
    | Inspect2(GCL.Range.t)
    | Refine(int, int)
    | ExportProofObligations
    | Debug

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Inspect(start, end_) =>
      object_(list{("tag", string("ReqInspect")), ("contents", (start, end_) |> tuple2(int, int))})
    | Inspect2(selection) =>
      object_(list{("tag", string("ReqInspect2")), ("contents", selection |> GCL.Range.encode)})
    | Refine(start, end_) =>
      object_(list{("tag", string("ReqRefine")), ("contents", (start, end_) |> tuple2(int, int))})
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
