module Request = {
  module Header = {
    type t =
      | Loading
      | Plain(string)
      | Error(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Loading" => Contents(_ => Loading)
        | "Plain" => Contents(json => Plain(string(json)))
        | "Error" => Contents(json => Error(string(json)))
        | tag =>
          raise(
            DecodeError("[Request.Header] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Loading => object_([("tag", string("Loading"))])
      | Plain(s) =>
        object_([("tag", string("Plain")), ("contents", string(s))])
      | Error(s) =>
        object_([("tag", string("Error")), ("contents", string(s))]);
  };
  module Body = {
    type t =
      | Nothing
      | ProofObligations(array(Response.ProofObligation.t))
      | Plain(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Nothing" => Contents(_ => Nothing)
        | "ProofObligations" =>
          Contents(
            json =>
              ProofObligations(
                json |> array(Response.ProofObligation.decode),
              ),
          )
        | "Plain" => Contents(json => Plain(string(json)))
        | tag =>
          raise(
            DecodeError("[Request.Header] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Nothing => object_([("tag", string("Nothing"))])
      | ProofObligations(x) =>
        object_([
          ("tag", string("ProofObligations")),
          ("contents", x |> array(Response.ProofObligation.encode)),
        ])
      | Plain(x) =>
        object_([("tag", string("Plain")), ("contents", string(x))]);
  };

  type t =
    | Show
    | Hide
    | Display(Header.t, Body.t);

  open Json.Decode;
  open Util.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Show" => Contents(_ => Show)
      | "Hide" => Contents(_ => Hide)
      // | "Display" => Contents(_json => Display(Loading, Nothing))
      | "Display" =>
        Contents(
          pair(Header.decode, Body.decode)
          |> map(((x, y)) => Display(x, y)),
        )
      | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Show => object_([("tag", string("Show"))])
    | Hide => object_([("tag", string("Hide"))])
    | Display(header, body) =>
      object_([
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      ]);
};

module Response = {
  type mode =
    | WP1
    | WP2;

  type linkEvent =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc);

  type t =
    | SetMode(mode)
    | Link(linkEvent)
    | Initialized
    | Destroyed;

  open Json.Decode;
  open Util.Decode;

  let decodeMode: decoder(mode) =
    sum(
      fun
      | "WP1" => TagOnly(_ => WP1)
      | "WP2" => TagOnly(_ => WP2)
      | tag =>
        raise(
          DecodeError("[View.Response.mode] Unknown constructor: " ++ tag),
        ),
    );

  let decodeLinkEvent: decoder(linkEvent) =
    sum(
      fun
      | "MouseOver" => Contents(loc => MouseOver(GCL.Loc.decode(loc)))
      | "MouseOut" => Contents(loc => MouseOut(GCL.Loc.decode(loc)))
      | "MouseClick" => Contents(loc => MouseClick(GCL.Loc.decode(loc)))
      | tag =>
        raise(
          DecodeError(
            "[View.Response.linkEvent] Unknown constructor: " ++ tag,
          ),
        ),
    );

  let decode: decoder(t) =
    sum(
      fun
      | "Initialized" => TagOnly(_ => Initialized)
      | "Destroyed" => TagOnly(_ => Destroyed)
      | "SetMode" => Contents(json => SetMode(decodeMode(json)))
      | "Link" => Contents(json => Link(decodeLinkEvent(json)))
      | tag =>
        raise(DecodeError("[View.Response.t] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;

  let encodeMode: encoder(mode) =
    fun
    | WP1 => object_([("tag", string("WP1"))])
    | WP2 => object_([("tag", string("WP2"))]);

  let encodeLinkEvent: encoder(linkEvent) =
    fun
    | MouseOver(loc) =>
      object_([
        ("tag", string("MouseOver")),
        ("contents", GCL.Loc.encode(loc)),
      ])
    | MouseOut(loc) =>
      object_([
        ("tag", string("MouseOut")),
        ("contents", GCL.Loc.encode(loc)),
      ])
    | MouseClick(loc) =>
      object_([
        ("tag", string("MouseClick")),
        ("contents", GCL.Loc.encode(loc)),
      ]);

  let encode: encoder(t) =
    fun
    | Initialized => object_([("tag", string("Initialized"))])
    | Destroyed => object_([("tag", string("Destroyed"))])
    | Link(e) =>
      object_([("tag", string("Link")), ("contents", encodeLinkEvent(e))])
    | SetMode(mode) =>
      object_([
        ("tag", string("SetMode")),
        ("contents", encodeMode(mode)),
      ]);
};