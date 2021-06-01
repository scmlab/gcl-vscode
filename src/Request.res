type filepath = string
type source = string

module Kind = {
  type t =
    | Inspect(SrcLoc.Range.t)
    | Refine(SrcLoc.Range.t)
    | ExportProofObligations
    | Debug

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Inspect(range) =>
      object_(list{("tag", string("ReqInspect")), ("contents", range |> SrcLoc.Range.encode)})
    | Refine(range) =>
      object_(list{("tag", string("ReqRefine")), ("contents", range |> SrcLoc.Range.encode)})
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
