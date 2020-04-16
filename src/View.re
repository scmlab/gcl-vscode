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
      | ProofObligations(array(GCL.Response.ProofObligation.t))
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
                json |> array(GCL.Response.ProofObligation.decode),
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
          ("contents", x |> array(GCL.Response.ProofObligation.encode)),
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
    // | Display(_header, _body) =>
    //   object_([
    //     ("tag", string("Display")),
    //     (
    //       "contents",
    //       (Header.Loading, Body.Nothing) |> pair(Header.encode, Body.encode),
    //     ),
    //   ]);
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
    | Destroy;
};