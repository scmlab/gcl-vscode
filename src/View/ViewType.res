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
    | Substitute(int, GCL.Syntax.Expr.t)
    | SetErrorMessages(array<(string, string)>)
    | DisplayBlocks(array<Element.Block.t>)
    | Display(
        int,
        array<Element.Block.t>,
      )
    // | UpdatePOs(array<Response.ProofObligation.t>)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "UpdateConnection" =>
      Contents(optional(ConnectionMethod.decode) |> map(method => UpdateConnection(method)))
    | "Substitute" =>
      Contents(pair(int, GCL.Syntax.Expr.decode) |> map(((x, y)) => Substitute(x, y)))
    | "SetErrorMessages" =>
      Contents(array(pair(string, string)) |> map(msgs => SetErrorMessages(msgs)))
    | "DisplayBlocks" =>
      Contents(array(Element.Block.decode) |> map(blocks => DisplayBlocks(blocks)))
    | "Display" =>
      Contents(
        tuple2(
          int,
          array(Element.Block.decode),
        ) |> map(((id, blocks)) => Display(id, blocks)),
      )
    // | "UpdatePOs" => Contents(array(Response.ProofObligation.decode) |> map(pos => UpdatePOs(pos)))
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
    | Substitute(i, expr) =>
      object_(list{
        ("tag", string("Substitute")),
        ("contents", (i, expr) |> pair(int, GCL.Syntax.Expr.encode)),
      })
    | SetErrorMessages(msgs) =>
      object_(list{
        ("tag", string("SetErrorMessages")),
        ("contents", msgs |> array(pair(string, string))),
      })
    | DisplayBlocks(blocks) =>
      object_(list{
        ("tag", string("DisplayBlocks")),
        ("contents", blocks |> array(Element.Block.encode)),
      })
    | Display(id, ws) =>
      object_(list{
        ("tag", string("Display")),
        (
          "contents",
          (id, ws) |> tuple2(
            int,
            array(Element.Block.encode),
          ),
        ),
      })
    // | UpdatePOs(pos) =>
    //   object_(list{
    //     ("tag", string("UpdatePOs")),
    //     ("contents", pos |> array(Response.ProofObligation.encode)),
    //   })
    }
}

module Response = {
  type t =
    | Link(Link.Event.t)
    | ExportProofObligations
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
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
    | "Substitute" =>
      Contents(
        tuple3(
          int,
          GCL.Syntax.Expr.decode,
          array(pair(GCL.Syntax.Name.decode, GCL.Syntax.Expr.decode)),
        ) |> map(((i, expr, subst)) => Substitute(i, expr, subst)),
      )
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
    | Substitute(i, expr, subst) =>
      object_(list{
        ("tag", string("Substitute")),
        (
          "contents",
          (i, expr, subst) |> tuple3(
            int,
            GCL.Syntax.Expr.encode,
            array(pair(GCL.Syntax.Name.encode, GCL.Syntax.Expr.encode)),
          ),
        ),
      })
    }
}
