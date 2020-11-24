module Request = {
  module Header = {
    type t =
      | Loading
      | Plain(string)
      | Error(string)

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Loading" => Contents(_ => Loading)
      | "Plain" => Contents(json => Plain(string(json)))
      | "Error" => Contents(json => Error(string(json)))
      | tag => raise(DecodeError("[Request.Header] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Loading => object_(list{("tag", string("Loading"))})
      | Plain(s) => object_(list{("tag", string("Plain")), ("contents", string(s))})
      | Error(s) => object_(list{("tag", string("Error")), ("contents", string(s))})
      }
  }
  module Body = {
    type id = int
    type t =
      | Nothing
      | ProofObligations(id, array<Response.ProofObligation.t>, array<Response.GlobalProp.t>)
      | Plain(string)

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Nothing" => Contents(_ => Nothing)
      | "ProofObligations" =>
        Contents(
          tuple3(
            int,
            array(Response.ProofObligation.decode),
            array(Response.GlobalProp.decode),
          ) |> map(((id, xs, ys)) => ProofObligations(id, xs, ys)),
        )
      | "Plain" => Contents(json => Plain(string(json)))
      | tag => raise(DecodeError("[Request.Header] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Nothing => object_(list{("tag", string("Nothing"))})
      | ProofObligations(id, xs, ys) =>
        object_(list{
          ("tag", string("ProofObligations")),
          (
            "contents",
            (id, xs, ys) |> tuple3(
              int,
              array(Response.ProofObligation.encode),
              array(Response.GlobalProp.encode),
            ),
          ),
        })
      | Plain(x) => object_(list{("tag", string("Plain")), ("contents", string(x))})
      }
  }

  type t =
    | Show
    | Hide
    | Substitute(int, GCL.Syntax.Expr.t)
    | Display(Header.t, Body.t)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "Show" => Contents(_ => Show)
    | "Hide" => Contents(_ => Hide)
    | "Substitute" =>
      Contents(pair(int, GCL.Syntax.Expr.decode) |> map(((x, y)) => Substitute(x, y)))
    | "Display" => Contents(pair(Header.decode, Body.decode) |> map(((x, y)) => Display(x, y)))
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
    | Display(header, body) =>
      object_(list{
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
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
