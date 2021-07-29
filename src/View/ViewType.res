// module ConnectionMethod = {
//   let decode: Json.Decode.decoder<Connection.method> = {
//     open Json.Decode
//     open Util.Decode
//     sum(x =>
//       switch x {
//       | "ViaTCP" => Contents(int |> map(port => Connection.Client.ViaTCP(port)))
//       | "ViaStdIO" =>
//         Contents(
//           pair(string, string) |> map(((name, path)) => Connection.Client.ViaStdIO(name, path)),
//         )
//       | "ViaPrebuilt" =>
//         Contents(
//           pair(string, string) |> map(((version, path)) => Connection.Client.ViaPrebuilt(
//             version,
//             path,
//           )),
//         )
//       | tag => raise(DecodeError("[ConnectionMethod] Unknown constructor: " ++ tag))
//       }
//     )
//   }

//   let encode: Json.Encode.encoder<Connection.method> = x => {
//     open Json.Encode
//     switch x {
//     | ViaStdIO(name, path) =>
//       object_(list{("tag", string("ViaStdIO")), ("contents", (name, path) |> pair(string, string))})
//     | ViaTCP(port) => object_(list{("tag", string("ViaTCP")), ("contents", port |> int)})
//     | ViaPrebuilt(version, path) =>
//       object_(list{
//         ("tag", string("ViaPrebuilt")),
//         ("contents", (version, path) |> pair(string, string)),
//       })
//     }
//   }
// }
module Request = {
  type t =
    | UpdateConnection(option<string>)
    | Display(int, array<Element.Section.t>)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "UpdateConnection" =>
        Contents(optional(string) |> map(method => UpdateConnection(method)))
      | "Display" =>
        Contents(
          tuple2(int, array(Element.Section.decode)) |> map(((id, sections)) => Display(
            id,
            sections,
          )),
        )
      | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
      }
    )
  }

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | UpdateConnection(method) =>
      object_(list{
        ("tag", string("UpdateConnection")),
        ("contents", method |> nullable(string)),
      })
    | Display(id, ws) =>
      object_(list{
        ("tag", string("Display")),
        ("contents", (id, ws) |> tuple2(int, array(Element.Section.encode))),
      })
    }
  }
}

module Response = {
  type t =
    | Link(Link.Event.t)
    | InsertAnchor(string)
    | Initialized
    | Destroyed

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "Initialized" => TagOnly(_ => Initialized)
      | "Destroyed" => TagOnly(_ => Destroyed)
      | "Link" => Contents(json => Link(Link.Event.decode(json)))
      | "InsertAnchor" => Contents(json => InsertAnchor(string(json)))
      | tag => raise(DecodeError("[Response.t] Unknown constructor: " ++ tag))
      }
    )
  }

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | Link(e) => object_(list{("tag", string("Link")), ("contents", Link.Event.encode(e))})
    | InsertAnchor(e) => object_(list{("tag", string("InsertAnchor")), ("contents", string(e))})
    }
  }
}
