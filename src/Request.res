type filepath = string
type source = string

module Kind = {
  type t =
    | Inspect(SrcLoc.Range.t)
    | Refine(SrcLoc.Range.t)
    | InsertAnchor(string)
    | Eval(string)
    | Debug

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Inspect(range) =>
      object_(list{("tag", string("ReqInspect")), ("contents", range |> SrcLoc.Range.encode)})
    | Refine(range) =>
      object_(list{("tag", string("ReqRefine")), ("contents", range |> SrcLoc.Range.encode)})
    | InsertAnchor(hash) =>
      object_(list{("tag", string("ReqInsertAnchor")), ("contents", hash |> string)})
    | Eval(hash) =>
      object_(list{("tag", string("ReqEval")), ("contents", hash |> string)})
    | Debug => object_(list{("tag", string("ReqDebug"))})
    }
  }
}

type t = Req(filepath, Kind.t)

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  switch x {
  | Req(filepath, kind) => (filepath, kind) |> pair(string, Kind.encode)
  }
}
