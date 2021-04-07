module ConnectionMethod = {
  open Json.Decode
  open Util.Decode

  let decode: decoder<Connection.LSP.method> = sum(x =>
    switch x {
    | "ViaTCP" => Contents(int |> map(port => Connection.LSP.ViaTCP(port)))
    | "ViaStdIO" =>
      Contents(pair(string, string) |> map(((name, path)) => Connection.LSP.ViaStdIO(name, path)))
    | tag => raise(DecodeError("[ConnectionMethod] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode

  let encode: encoder<Connection.LSP.method> = x =>
    switch x {
    | ViaStdIO(name, path) =>
      object_(list{("tag", string("ViaStdIO")), ("contents", (name, path) |> pair(string, string))})
    | ViaTCP(port) => object_(list{("tag", string("ViaTCP")), ("contents", port |> int)})
    }
}
module Request = {
  type t =
    | UpdateConnection(option<Connection.LSP.method>)
    | Substitute(int, GCL.Syntax.Expr.t)
    | SetErrorMessages(array<(string, string)>)
    | Display(
        int,
        array<Response.ProofObligation.t>,
        array<Response.GlobalProp.t>,
        array<Response.Warning.t>,
      )
    | UpdatePOs(array<Response.ProofObligation.t>)

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
    | "Display" =>
      Contents(
        tuple4(
          int,
          array(Response.ProofObligation.decode),
          array(Response.GlobalProp.decode),
          array(Response.Warning.decode),
        ) |> map(((id, xs, ys, ws)) => Display(id, xs, ys, ws)),
      )
    | "UpdatePOs" =>
      Contents(array(Response.ProofObligation.decode) |> map(pos => UpdatePOs(pos)))
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
    | Display(id, pos, globalProps, ws) =>
      object_(list{
        ("tag", string("Display")),
        (
          "contents",
          (id, pos, globalProps, ws) |> tuple4(
            int,
            array(Response.ProofObligation.encode),
            array(Response.GlobalProp.encode),
            array(Response.Warning.encode),
          ),
        ),
      })
    | UpdatePOs(pos) =>
      object_(list{
        ("tag", string("UpdatePOs")),
        ("contents", pos |> array(Response.ProofObligation.encode)),
      })
    }
}

module Response = {
  type linkEvent =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc)

  type t =
    | Link(linkEvent)
    | ExportProofObligations
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | Initialized
    | Destroyed

  open Json.Decode
  open Util.Decode

  module LinkEvent = {
    let decode: decoder<linkEvent> = sum(x =>
      switch x {
      | "MouseOver" => Contents(loc => MouseOver(GCL.Loc.decode(loc)))
      | "MouseOut" => Contents(loc => MouseOut(GCL.Loc.decode(loc)))
      | "MouseClick" => Contents(loc => MouseClick(GCL.Loc.decode(loc)))
      | tag => raise(DecodeError("[Response.linkEvent] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<linkEvent> = x =>
      switch x {
      | MouseOver(loc) =>
        object_(list{("tag", string("MouseOver")), ("contents", GCL.Loc.encode(loc))})
      | MouseOut(loc) =>
        object_(list{("tag", string("MouseOut")), ("contents", GCL.Loc.encode(loc))})
      | MouseClick(loc) =>
        object_(list{("tag", string("MouseClick")), ("contents", GCL.Loc.encode(loc))})
      }
  }

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Initialized" => TagOnly(_ => Initialized)
    | "ExportProofObligations" => TagOnly(_ => ExportProofObligations)
    | "Destroyed" => TagOnly(_ => Destroyed)
    | "Link" => Contents(json => Link(LinkEvent.decode(json)))
    | "Substitute" =>
      Contents(
        tuple3(int, GCL.Syntax.Expr.decode, GCL.Syntax.Expr.decodeSubst) |> map(((
          i,
          expr,
          subst,
        )) => Substitute(i, expr, subst)),
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
    | Link(e) => object_(list{("tag", string("Link")), ("contents", LinkEvent.encode(e))})
    | Substitute(i, expr, subst) =>
      object_(list{
        ("tag", string("Substitute")),
        (
          "contents",
          (i, expr, subst) |> tuple3(int, GCL.Syntax.Expr.encode, GCL.Syntax.Expr.encodeSubst),
        ),
      })
    }
}
