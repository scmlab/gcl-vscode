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
};

module Error = {
  module Site = {
    type t =
      | Global(loc)
      | Local(loc, int);

    let toLoc = (site, specifications): loc => {
      Specification.(
        switch (site) {
        | Global(loc) => loc
        | Local(loc, i) =>
          let specs = specifications->Array.keep(spec => spec.id == i);

          specs[0]
          ->Option.mapWithDefault(loc, spec =>
              spec.loc |> Loc.translate(loc) |> Loc.translateBy(1, 0, 1, 0)
            );
        }
      );
    };

    let toRange = (site, specifications, locToRange) =>
      toLoc(site, specifications) |> locToRange;

    let toString = site => {
      switch (site) {
      | Global(loc) => "at " ++ Loc.toString(loc)
      | Local(loc, i) =>
        "at " ++ Loc.toString(loc) ++ " in #" ++ string_of_int(i)
      };
    };

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Global" => Contents(json => Global(json |> Loc.decode))
        | "Local" =>
          Contents(pair(Loc.decode, int) |> map(((r, i)) => Local(r, i)))
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

  module StructError2 = {
    type t =
      | MissingLoopInvariant
      // | MissingAssertion
      | MissingBound
      | MissingPrecondition
      | MissingPostcondition
      | PreconditionUnknown
      | DigHole;

    open Json.Decode;
    open Util.Decode;
    let decode: decoder(t) =
      sum(
        fun
        // | "MissingAssertion" => Contents(_ => MissingAssertion)
        // | "ExcessBound" => Contents(_ => ExcessBound)
        | "MissingBound" => Contents(_ => MissingBound)
        | "MissingLoopInvariant" => Contents(_ => MissingLoopInvariant)
        | "MissingPrecondition" => Contents(_ => MissingPrecondition)
        | "MissingPostcondition" => Contents(_ => MissingPostcondition)
        | "PreconditionUnknown" => Contents(_ => PreconditionUnknown)
        | "DigHole" => Contents(_ => DigHole)
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  type kind =
    | LexicalError
    | SyntacticError(array(string))
    | StructError(StructError.t)
    | StructError2(StructError2.t)
    | TypeError(TypeError.t)
    | CannotDecodeRequest(string)
    | CannotReadFile(string)
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
      | "StructError2" =>
        Contents(json => StructError2(json |> StructError2.decode))
      | "TypeError" => Contents(json => TypeError(json |> TypeError.decode))
      | "CannotDecodeRequest" =>
        Contents(json => CannotDecodeRequest(json |> string))
      | "CannotReadFile" => Contents(json => CannotReadFile(json |> string))
      | "NotLoaded" => TagOnly(_ => NotLoaded)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  type t =
    | Error(Site.t, kind);

  let decode: decoder(t) =
    pair(Site.decode, decodeKind)
    |> map(((site, kind)) => Error(site, kind));
};

type t =
  | Error(array(Error.t))
  | OK(
      array(ProofObligation.t),
      array(Specification.t),
      array(GlobalProp.t),
    )
  | Resolve(int)
  | Substitute(Syntax.Expr.t)
  | InsertAssertion(int, Syntax.Expr.t)
  | UnknownResponse(Js.Json.t);

open Json.Decode;
open Util.Decode;
let decode: decoder(t) =
  sum(
    fun
    | "ResError" =>
      Contents(array(Error.decode) |> map(errors => Error(errors)))
    | "ResOK" =>
      Contents(
        tuple3(
          array(ProofObligation.decode),
          array(Specification.decode),
          array(GlobalProp.decode),
        )
        |> map(((obs, specs, globalProps)) => OK(obs, specs, globalProps)),
      )
    | "ResSubstitute" =>
      Contents(Syntax.Expr.decode |> map(expr => Substitute(expr)))
    | "ResInsert" =>
      Contents(
        pair(int, Syntax.Expr.decode)
        |> map(((i, expr)) => InsertAssertion(i, expr)),
      )
    | "ResResolve" => Contents(int |> map(i => Resolve(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );