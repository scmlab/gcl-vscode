open Belt;
open GCL;
open GCL.Syntax;
module Origin = {
  type t =
    | AtAbort(loc)
    | AtSkip(loc)
    | AtSpec(loc)
    | AtAssignment(loc)
    | AtAssertion(loc)
    | AtLoopInvariant(loc)
    | AtIf(loc)
    | AtLoop(loc)
    | AtTermination(loc)
    | AtBoundDecrement(loc);

  let toString =
    fun
    | AtAbort(_) => "Abort"
    | AtSkip(_) => "Skip"
    | AtSpec(_) => "Spec"
    | AtAssignment(_) => "Assignment"
    | AtAssertion(_) => "Assertion"
    | AtLoopInvariant(_) => "Loop Invariant"
    | AtIf(_) => "Conditional"
    | AtLoop(_) => "Loop"
    | AtTermination(_) => "Termination"
    | AtBoundDecrement(_) => "Bound Decrement";

  let locOf =
    fun
    | AtAbort(loc) => loc
    | AtSkip(loc) => loc
    | AtSpec(loc) => loc
    | AtAssignment(loc) => loc
    | AtAssertion(loc) => loc
    | AtLoopInvariant(loc) => loc
    | AtIf(loc) => loc
    | AtLoop(loc) => loc
    | AtTermination(loc) => loc
    | AtBoundDecrement(loc) => loc;

  open Util.Decode;
  open! Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "AtAbort" => Contents(Loc.decode |> map(x => AtAbort(x)))
      | "AtSkip" => Contents(Loc.decode |> map(x => AtSkip(x)))
      | "AtSpec" => Contents(Loc.decode |> map(x => AtSpec(x)))
      | "AtAssignment" => Contents(Loc.decode |> map(x => AtAssignment(x)))
      | "AtAssertion" => Contents(Loc.decode |> map(x => AtAssertion(x)))
      | "AtLoopInvariant" =>
        Contents(Loc.decode |> map(x => AtLoopInvariant(x)))
      | "AtIf" => Contents(Loc.decode |> map(x => AtIf(x)))
      | "AtLoop" => Contents(Loc.decode |> map(x => AtLoop(x)))
      | "AtTermination" => Contents(Loc.decode |> map(x => AtTermination(x)))
      | "AtBoundDecrement" =>
        Contents(Loc.decode |> map(x => AtBoundDecrement(x)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | AtAbort(loc) =>
      object_([
        ("tag", string("AtAbort")),
        ("contents", loc |> Loc.encode),
      ])
    | AtSkip(loc) =>
      object_([("tag", string("AtSkip")), ("contents", loc |> Loc.encode)])
    | AtSpec(loc) =>
      object_([("tag", string("AtSpec")), ("contents", loc |> Loc.encode)])
    | AtAssignment(loc) =>
      object_([
        ("tag", string("AtAssignment")),
        ("contents", loc |> Loc.encode),
      ])
    | AtAssertion(loc) =>
      object_([
        ("tag", string("AtAssertion")),
        ("contents", loc |> Loc.encode),
      ])
    | AtLoopInvariant(loc) =>
      object_([
        ("tag", string("AtLoopInvariant")),
        ("contents", loc |> Loc.encode),
      ])
    | AtIf(loc) =>
      object_([("tag", string("AtIf")), ("contents", loc |> Loc.encode)])
    | AtLoop(loc) =>
      object_([("tag", string("AtLoop")), ("contents", loc |> Loc.encode)])
    | AtTermination(loc) =>
      object_([
        ("tag", string("AtTermination")),
        ("contents", loc |> Loc.encode),
      ])
    | AtBoundDecrement(loc) =>
      object_([
        ("tag", string("AtBoundDecrement")),
        ("contents", loc |> Loc.encode),
      ]);
};

module ProofObligation = {
  type t =
    | ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t);

  open Json.Decode;
  let decode: decoder(t) =
    tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode)
    |> map(((i, p, q, o)) => ProofObligation(i, p, q, o));

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | ProofObligation(i, p, q, o) =>
      (i, p, q, o)
      |> tuple4(int, Syntax.Pred.encode, Syntax.Pred.encode, Origin.encode);
};

module Specification = {
  type t = {
    id: int,
    pre: Syntax.Pred.t,
    post: Syntax.Pred.t,
    loc,
  };

  open Json.Decode;
  let decode: decoder(t) =
    json => {
      id: json |> field("specID", int),
      pre: json |> field("specPreCond", Syntax.Pred.decode),
      post: json |> field("specPostCond", Syntax.Pred.decode),
      loc: json |> field("specLoc", Loc.decode),
    };
};

module GlobalProp = {
  type t = Syntax.Expr.t;

  open Json.Decode;
  let decode: decoder(t) = Syntax.Expr.decode;

  open Json.Encode;
  let encode: encoder(t) = Syntax.Expr.encode;
};

module Error = {
  module Site = {
    type t =
      | Source(loc) // the error happened somewhere in the source file
      | Hole(loc, int) // the error happened somewhere in a hole
      | Others; // the error happened elsewhere (e.g. when decoding JSON)

    let toLoc = (site, specifications): loc => {
      Specification.(
        switch (site) {
        | Source(loc) => loc
        | Hole(loc, i) =>
          let specs = specifications->Array.keep(spec => spec.id == i);

          specs[0]
          ->Option.mapWithDefault(loc, spec =>
              spec.loc |> Loc.translate(loc) |> Loc.translateBy(1, 0, 1, 0)
            );
        | Others => NoLoc
        }
      );
    };

    let toRange = (site, specifications, locToRange) =>
      toLoc(site, specifications) |> locToRange;

    let toString = site => {
      switch (site) {
      | Source(loc) => "at " ++ Loc.toString(loc)
      | Hole(loc, i) =>
        "at " ++ Loc.toString(loc) ++ " in #" ++ string_of_int(i)
      | Others => ""
      };
    };

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Global" => Contents(json => Source(json |> Loc.decode))
        | "Local" =>
          Contents(pair(Loc.decode, int) |> map(((r, i)) => Hole(r, i)))
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  module TypeError = {
    type t =
      | NotInScope(string)
      | UnifyFailed(Type.t, Type.t)
      | RecursiveType(int, Type.t)
      | NotFunction(Type.t);

    open Json.Decode;
    open Util.Decode;
    let decode: decoder(t) =
      sum(
        fun
        | "NotInScope" =>
          Contents(
            pair(string, Loc.decode) |> map(((s, _)) => NotInScope(s)),
          )
        | "UnifyFailed" =>
          Contents(
            tuple3(Type.decode, Type.decode, Loc.decode)
            |> map(((s, t, _)) => UnifyFailed(s, t)),
          )
        | "RecursiveType" =>
          Contents(
            tuple3(int, Type.decode, Loc.decode)
            |> map(((s, t, _)) => RecursiveType(s, t)),
          )
        | "NotFunction" =>
          Contents(
            pair(Type.decode, Loc.decode)
            |> map(((t, _)) => NotFunction(t)),
          )
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  module StructError = {
    type t =
      | MissingAssertion
      | MissingBound
      | ExcessBound
      | MissingPostcondition
      | DigHole;

    open Json.Decode;
    open Util.Decode;
    let decode: decoder(t) =
      sum(
        fun
        | "MissingAssertion" => Contents(_ => MissingAssertion)
        | "MissingBound" => Contents(_ => MissingBound)
        // | "MissingLoopInvariant" => Contents(_ => MissingLoopInvariant)
        | "ExcessBound" => Contents(_ => ExcessBound)
        // | "MissingPrecondition" => Contents(_ => MissingPrecondition)
        | "MissingPostcondition" => Contents(_ => MissingPostcondition)
        | "DigHole" => Contents(_ => DigHole)
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  type kind =
    // from server, GCL related
    | LexicalError
    | SyntacticError(array(string))
    | StructError(StructError.t)
    | TypeError(TypeError.t)
    // from server
    // | CannotDecodeRequest(string) // the server failed to decode request from the client
    | CannotReadFile(string)
    // from client
    // | CannotDecodeResponse(string, Js.Json.t) // the client failed to decode response from the server
    | CannotSendRequest(string)
    | NotLoaded;

  open Json.Decode;
  open Util.Decode;
  let decodeKind: decoder(kind) =
    sum(
      fun
      | "LexicalError" => TagOnly(_ => LexicalError)
      | "SyntacticError" =>
        Contents(
          array(pair(Loc.decode, string))
          |> map(pairs => SyntacticError(pairs->Array.map(snd))),
        )
      | "StructError" =>
        Contents(json => StructError(json |> StructError.decode))
      | "TypeError" => Contents(json => TypeError(json |> TypeError.decode))
      // | "CannotDecodeRequest" =>
      //   Contents(json => CannotDecodeRequest(json |> string))
      | "CannotReadFile" => Contents(json => CannotReadFile(json |> string))
      | "NotLoaded" => TagOnly(_ => NotLoaded)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  type t =
    | Error(Site.t, kind);

  let decode: decoder(t) =
    pair(Site.decode, decodeKind)
    |> map(((site, kind)) => Error(site, kind));

  let fromJsError = (error: 'a): string => {
    [%raw "function (e) {return e.toString()}"](error);
  };
};

module Kind = {
  type t =
    | Decorate(array(GCL.Loc.t))
    | Error(array(Error.t))
    | OK(
        int,
        array(ProofObligation.t),
        array(Specification.t),
        array(GlobalProp.t),
      )
    | Resolve(int)
    | Substitute(int, Syntax.Expr.t)
    | UnknownResponse(Js.Json.t);

  open Json.Decode;
  open Util.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "ResDecorate" =>
        Contents(array(GCL.Loc.decode) |> map(locs => Decorate(locs)))
      | "ResError" =>
        Contents(array(Error.decode) |> map(errors => Error(errors)))
      | "ResOK" =>
        Contents(
          tuple4(
            int,
            array(ProofObligation.decode),
            array(Specification.decode),
            array(GlobalProp.decode),
          )
          |> map(((id, obs, specs, globalProps)) =>
               OK(id, obs, specs, globalProps)
             ),
        )
      | "ResSubstitute" =>
        Contents(
          pair(int, Syntax.Expr.decode)
          |> map(((i, expr)) => Substitute(i, expr)),
        )
      | "ResResolve" => Contents(int |> map(i => Resolve(i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

type t =
  | Res(string, array(Kind.t))
  | CannotDecodeResponse(string, Js.Json.t)
  | CannotDecodeRequest(string)
  | CannotSendRequest(string);

open Json.Decode;
open Util.Decode;
let decode: decoder(t) =
  sum(
    fun
    | "Res" =>
      Contents(
        pair(string, array(Kind.decode))
        |> map(((filePath, kinds)) => Res(filePath, kinds)),
      )
    | "CannotDecodeRequest" =>
      Contents(string |> map(msg => CannotDecodeRequest(msg)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );
