module Request = {
  type t =
    | UpdateConnectionStatus(string)
    | Substitute(int, Element.Inlines.t)
    | Display(int, array<Element.Section.t>)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "UpdateConnectionStatus" =>
        Contents(string |> map(method => UpdateConnectionStatus(method)))
      | "Substitute" =>
        Contents(tuple2(int, Element.Inlines.decode) |> map(((id, expr)) => Substitute(id, expr)))
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
    | UpdateConnectionStatus(method) =>
      object_(list{("tag", string("UpdateConnectionStatus")), ("contents", method |> string)})
    | Substitute(id, expr) =>
      object_(list{
        ("tag", string("Substitute")),
        ("contents", (id, expr) |> tuple2(int, Element.Inlines.encode)),
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
    | Solve(string)
    | Substitute(int)
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
      | "Solve" => Contents(json => Solve(string(json)))
      | "Substitute" => Contents(json => Substitute(int(json)))
      | tag => raise(DecodeError("[Response.t] Unknown constructor: " ++ tag))
      }
    )
  }

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | Solve(e) => object_(list{("tag", string("Solve")), ("contents", string(e))})
    | Link(e) => object_(list{("tag", string("Link")), ("contents", Link.Event.encode(e))})
    | InsertAnchor(e) => object_(list{("tag", string("InsertAnchor")), ("contents", string(e))})
    | Substitute(e) => object_(list{("tag", string("Substitute")), ("contents", int(e))})
    }
  }
}
