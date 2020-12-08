module Request = {
  type t =
    | Show
    | Hide
    | Substitute(int, GCL.Syntax.Expr.t)
    | Display(int, array<Response.ProofObligation.t>, array<Response.GlobalProp.t>)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "Show" => Contents(_ => Show)
    | "Hide" => Contents(_ => Hide)
    | "Substitute" =>
      Contents(pair(int, GCL.Syntax.Expr.decode) |> map(((x, y)) => Substitute(x, y)))
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
    | Show => object_(list{("tag", string("Show"))})
    | Hide => object_(list{("tag", string("Hide"))})
    | Substitute(i, expr) =>
      object_(list{
        ("tag", string("Substitute")),
        ("contents", (i, expr) |> pair(int, GCL.Syntax.Expr.encode)),
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
    | Link(linkEvent)
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
    | "Initialized" => TagOnly(_ => Initialized)
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
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
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
