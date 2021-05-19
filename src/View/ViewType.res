module ConnectionMethod = {
  open Json.Decode
  open Util.Decode

  let decode: decoder<Connection.method> = sum(x =>
    switch x {
    | "ViaTCP" => Contents(int |> map(port => Connection.Client.ViaTCP(port)))
    | "ViaStdIO" =>
      Contents(
        pair(string, string) |> map(((name, path)) => Connection.Client.ViaStdIO(name, path)),
      )
    | "ViaPrebuilt" =>
      Contents(
        pair(string, string) |> map(((version, path)) => Connection.Client.ViaPrebuilt(
          version,
          path,
        )),
      )
    | tag => raise(DecodeError("[ConnectionMethod] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode

  let encode: encoder<Connection.method> = x =>
    switch x {
    | ViaStdIO(name, path) =>
      object_(list{("tag", string("ViaStdIO")), ("contents", (name, path) |> pair(string, string))})
    | ViaTCP(port) => object_(list{("tag", string("ViaTCP")), ("contents", port |> int)})
    | ViaPrebuilt(version, path) =>
      object_(list{
        ("tag", string("ViaPrebuilt")),
        ("contents", (version, path) |> pair(string, string)),
      })
    }
}
module Request = {
  type t =
    | UpdateConnection(option<Connection.method>)
    | Display(int, array<Element.Block.t>)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "UpdateConnection" =>
      Contents(optional(ConnectionMethod.decode) |> map(method => UpdateConnection(method)))
    | "Display" =>
      Contents(
        tuple2(int, array(Element.Block.decode)) |> map(((id, blocks)) => Display(id, blocks)),
      )
    | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | UpdateConnection(method) =>
      object_(list{
        ("tag", string("UpdateConnection")),
        ("contents", method |> nullable(ConnectionMethod.encode)),
      })
    | Display(id, ws) =>
      object_(list{
        ("tag", string("Display")),
        ("contents", (id, ws) |> tuple2(int, array(Element.Block.encode))),
      })
    }
}

module Response = {
  type t =
    | Link(Link.Event.t)
    | ExportProofObligations
    | Initialized
    | Destroyed

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Initialized" => TagOnly(_ => Initialized)
    | "ExportProofObligations" => TagOnly(_ => ExportProofObligations)
    | "Destroyed" => TagOnly(_ => Destroyed)
    | "Link" => Contents(json => Link(Link.Event.decode(json)))
    | tag => raise(DecodeError("[Response.t] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode

  let encode: encoder<t> = x =>
    switch x {
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | ExportProofObligations => object_(list{("tag", string("ExportProofObligations"))})
    | Link(e) => object_(list{("tag", string("Link")), ("contents", Link.Event.encode(e))})
    }
}
