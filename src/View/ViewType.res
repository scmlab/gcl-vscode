module Request = {
  module LSPClientStatus = {
    open Json.Decode
    open Util.Decode
    let decode: decoder<LSP.Client.status> = sum(x =>
      switch x {
      | "Disconnected" => TagOnly(_ => LSP.Client.Disconnected)
      | "Connecting" => TagOnly(_ => Connecting)
      | "Connected" => TagOnly(_ => Connected)
      | tag => raise(DecodeError("[LSPClientStatus] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<LSP.Client.status> = x =>
      switch x {
      | Disconnected => object_(list{("tag", string("Disconnected"))})
      | Connecting => object_(list{("tag", string("Connecting"))})
      | Connected => object_(list{("tag", string("Connected"))})
      }
  }

  type t =
    | UpdateDevMode(bool)
    | UpdateConnectionStatus(LSP.Client.status)
    | Substitute(int, GCL.Syntax.Expr.t)
    | SetErrorMessages(array<(string, string)>)
    | Display(int, array<Response.ProofObligation.t>, array<Response.GlobalProp.t>)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "UpdateDevMode" => Contents(bool |> map(devMode => UpdateDevMode(devMode)))
    | "UpdateConnectionStatus" =>
      Contents(LSPClientStatus.decode |> map(status => UpdateConnectionStatus(status)))
    | "Substitute" =>
      Contents(pair(int, GCL.Syntax.Expr.decode) |> map(((x, y)) => Substitute(x, y)))
    | "SetErrorMessages" =>
      Contents(array(pair(string, string)) |> map(msgs => SetErrorMessages(msgs)))
    | "Display" =>
      Contents(
        tuple3(
          int,
          array(Response.ProofObligation.decode),
          array(Response.GlobalProp.decode),
        ) |> map(((id, xs, ys)) => Display(id, xs, ys)),
      )
    | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | UpdateDevMode(devMode) =>
      object_(list{("tag", string("UpdateDevMode")), ("contents", devMode |> bool)})
    | UpdateConnectionStatus(status) =>
      object_(list{
        ("tag", string("UpdateConnectionStatus")),
        ("contents", status |> LSPClientStatus.encode),
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
    | Display(id, pos, globalProps) =>
      object_(list{
        ("tag", string("Display")),
        (
          "contents",
          (id, pos, globalProps) |> tuple3(
            int,
            array(Response.ProofObligation.encode),
            array(Response.GlobalProp.encode),
          ),
        ),
      })
    }
}

module Response = {
  type linkEvent =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc)

  type t =
    | Connect
    | Disconnect
    | Link(linkEvent)
    | ExportProofObligations
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | Initialized
    | Destroyed

  open Json.Decode
  open Util.Decode

  let decodeLinkEvent: decoder<linkEvent> = sum(x =>
    switch x {
    | "MouseOver" => Contents(loc => MouseOver(GCL.Loc.decode(loc)))
    | "MouseOut" => Contents(loc => MouseOut(GCL.Loc.decode(loc)))
    | "MouseClick" => Contents(loc => MouseClick(GCL.Loc.decode(loc)))
    | tag => raise(DecodeError("[Response.linkEvent] Unknown constructor: " ++ tag))
    }
  )

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Connect" => TagOnly(_ => Connect)
    | "Disconnect" => TagOnly(_ => Disconnect)
    | "Initialized" => TagOnly(_ => Initialized)
    | "ExportProofObligations" => TagOnly(_ => ExportProofObligations)
    | "Destroyed" => TagOnly(_ => Destroyed)
    | "Link" => Contents(json => Link(decodeLinkEvent(json)))
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

  let encodeLinkEvent: encoder<linkEvent> = x =>
    switch x {
    | MouseOver(loc) =>
      object_(list{("tag", string("MouseOver")), ("contents", GCL.Loc.encode(loc))})
    | MouseOut(loc) => object_(list{("tag", string("MouseOut")), ("contents", GCL.Loc.encode(loc))})
    | MouseClick(loc) =>
      object_(list{("tag", string("MouseClick")), ("contents", GCL.Loc.encode(loc))})
    }

  let encode: encoder<t> = x =>
    switch x {
    | Connect => object_(list{("tag", string("Connect"))})
    | Disconnect => object_(list{("tag", string("Disconnect"))})
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | ExportProofObligations => object_(list{("tag", string("ExportProofObligations"))})
    | Link(e) => object_(list{("tag", string("Link")), ("contents", encodeLinkEvent(e))})
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
