open Belt
open GCL
open GCL.Syntax
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
    | AtBoundDecrement(loc)

  let toString = x =>
    switch x {
    | AtAbort(_) => "Abort"
    | AtSkip(_) => "Skip"
    | AtSpec(_) => "Spec"
    | AtAssignment(_) => "Assignment"
    | AtAssertion(_) => "Assertion"
    | AtLoopInvariant(_) => "Loop Invariant"
    | AtIf(_) => "Conditional"
    | AtLoop(_) => "Loop"
    | AtTermination(_) => "Termination"
    | AtBoundDecrement(_) => "Bound Decrement"
    }

  let locOf = x =>
    switch x {
    | AtAbort(loc) => loc
    | AtSkip(loc) => loc
    | AtSpec(loc) => loc
    | AtAssignment(loc) => loc
    | AtAssertion(loc) => loc
    | AtLoopInvariant(loc) => loc
    | AtIf(loc) => loc
    | AtLoop(loc) => loc
    | AtTermination(loc) => loc
    | AtBoundDecrement(loc) => loc
    }

  open Util.Decode
  open! Json.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "AtAbort" => Contents(Loc.decode |> map(x => AtAbort(x)))
    | "AtSkip" => Contents(Loc.decode |> map(x => AtSkip(x)))
    | "AtSpec" => Contents(Loc.decode |> map(x => AtSpec(x)))
    | "AtAssignment" => Contents(Loc.decode |> map(x => AtAssignment(x)))
    | "AtAssertion" => Contents(Loc.decode |> map(x => AtAssertion(x)))
    | "AtLoopInvariant" => Contents(Loc.decode |> map(x => AtLoopInvariant(x)))
    | "AtIf" => Contents(Loc.decode |> map(x => AtIf(x)))
    | "AtLoop" => Contents(Loc.decode |> map(x => AtLoop(x)))
    | "AtTermination" => Contents(Loc.decode |> map(x => AtTermination(x)))
    | "AtBoundDecrement" => Contents(Loc.decode |> map(x => AtBoundDecrement(x)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag))
    }
  )
  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | AtAbort(loc) => object_(list{("tag", string("AtAbort")), ("contents", loc |> Loc.encode)})
    | AtSkip(loc) => object_(list{("tag", string("AtSkip")), ("contents", loc |> Loc.encode)})
    | AtSpec(loc) => object_(list{("tag", string("AtSpec")), ("contents", loc |> Loc.encode)})
    | AtAssignment(loc) =>
      object_(list{("tag", string("AtAssignment")), ("contents", loc |> Loc.encode)})
    | AtAssertion(loc) =>
      object_(list{("tag", string("AtAssertion")), ("contents", loc |> Loc.encode)})
    | AtLoopInvariant(loc) =>
      object_(list{("tag", string("AtLoopInvariant")), ("contents", loc |> Loc.encode)})
    | AtIf(loc) => object_(list{("tag", string("AtIf")), ("contents", loc |> Loc.encode)})
    | AtLoop(loc) => object_(list{("tag", string("AtLoop")), ("contents", loc |> Loc.encode)})
    | AtTermination(loc) =>
      object_(list{("tag", string("AtTermination")), ("contents", loc |> Loc.encode)})
    | AtBoundDecrement(loc) =>
      object_(list{("tag", string("AtBoundDecrement")), ("contents", loc |> Loc.encode)})
    }
}

module ProofObligation = {
  type t = ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t)

  open Json.Decode
  let decode: decoder<t> =
    tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode) |> map(((
      i,
      p,
      q,
      o,
    )) => ProofObligation(i, p, q, o))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | ProofObligation(i, p, q, o) =>
      (i, p, q, o) |> tuple4(int, Syntax.Pred.encode, Syntax.Pred.encode, Origin.encode)
    }
}

module Specification = {
  type t = {
    id: int,
    pre: Syntax.Pred.t,
    post: Syntax.Pred.t,
    mutable loc: loc,
    mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  }

  let destroy = self => self.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)

  open Json.Decode
  let decode: decoder<t> = json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Syntax.Pred.decode),
    post: json |> field("specPostCond", Syntax.Pred.decode),
    loc: json |> field("specLoc", Loc.decode),
    decorations: [],
  }
}

module GlobalProp = {
  type t = Syntax.Expr.t

  open Json.Decode
  let decode: decoder<t> = Syntax.Expr.decode

  open Json.Encode
  let encode: encoder<t> = Syntax.Expr.encode
}

module Error = {
  module Site = {
    type t =
      | Source(loc) // the error happened somewhere in the source file
      | Hole(loc, int) // the error happened somewhere in a hole
      | Others // the error happened elsewhere (e.g. when decoding JSON)

    let toLoc = (site, specifications): loc => {
      open Specification
      switch site {
      | Source(loc) => loc
      | Hole(loc, i) =>
        let specs = specifications->Array.keep(spec => spec.id == i)

        specs[0]->Option.mapWithDefault(loc, spec =>
          spec.loc |> Loc.translate(loc) |> Loc.translateBy(1, 0, 1, 0)
        )
      | Others => NoLoc
      }
    }

    let toRange = (site, specifications, locToRange) => toLoc(site, specifications) |> locToRange

    let toString = site =>
      switch site {
      | Source(loc) => "at " ++ Loc.toString(loc)
      | Hole(loc, i) => "at " ++ (Loc.toString(loc) ++ (" in #" ++ string_of_int(i)))
      | Others => ""
      }

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Global" => Contents(json => Source(json |> Loc.decode))
      | "Local" => Contents(pair(Loc.decode, int) |> map(((r, i)) => Hole(r, i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag))
      }
    )
  }

  module TypeError = {
    type t =
      | NotInScope(string)
      | UnifyFailed(Type.t, Type.t)
      | RecursiveType(int, Type.t)
      | NotFunction(Type.t)

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "NotInScope" => Contents(pair(string, Loc.decode) |> map(((s, _)) => NotInScope(s)))
      | "UnifyFailed" =>
        Contents(
          tuple3(Type.decode, Type.decode, Loc.decode) |> map(((s, t, _)) => UnifyFailed(s, t)),
        )
      | "RecursiveType" =>
        Contents(tuple3(int, Type.decode, Loc.decode) |> map(((s, t, _)) => RecursiveType(s, t)))
      | "NotFunction" => Contents(pair(Type.decode, Loc.decode) |> map(((t, _)) => NotFunction(t)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag))
      }
    )
  }

  module StructError = {
    type t =
      | MissingAssertion
      | MissingPostcondition
      | DigHole

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "MissingAssertion" => Contents(_ => MissingAssertion)
      | "MissingPostcondition" => Contents(_ => MissingPostcondition)
      | "DigHole" => Contents(_ => DigHole)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag))
      }
    )
  }

  type kind =
    // from server, GCL related
    | LexicalError
    | SyntacticError(array<string>)
    | StructError(StructError.t)
    | TypeError(TypeError.t)
    // from server
    | CannotReadFile(string)
    // from client
    | CannotSendRequest(string)

  open Json.Decode
  open Util.Decode
  let decodeKind: decoder<kind> = sum(x =>
    switch x {
    | "LexicalError" => TagOnly(_ => LexicalError)
    | "SyntacticError" =>
      Contents(
        array(pair(Loc.decode, string)) |> map(pairs => SyntacticError(pairs->Array.map(snd))),
      )
    | "StructError" => Contents(json => StructError(json |> StructError.decode))
    | "TypeError" => Contents(json => TypeError(json |> TypeError.decode))
    | "CannotReadFile" => Contents(json => CannotReadFile(json |> string))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag))
    }
  )

  type t = Error(Site.t, kind)

  let decode: decoder<t> = pair(Site.decode, decodeKind) |> map(((site, kind)) => Error(site, kind))

  let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

  let matchDigHole = error =>
    switch error {
    | Error(site, StructError(DigHole)) => Some(site)
    | _ => None
    }
}

module Warning = {
  type t =
    | MissingBound(loc)
    | ExcessBound(loc)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "MissingBound" => Contents(Loc.decode |> map(loc => MissingBound(loc)))
    | "ExcessBound" => Contents(Loc.decode |> map(loc => ExcessBound(loc)))
    | tag => raise(DecodeError("[Response.Warning] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | MissingBound(loc) =>
      object_(list{("tag", string("MissingBound")), ("contents", loc |> Loc.encode)})
    | ExcessBound(loc) =>
      object_(list{("tag", string("ExcessBound")), ("contents", loc |> Loc.encode)})
    }
}

module Kind = {
  type t =
    | Error(array<Error.t>)
    | OK(
        int,
        array<ProofObligation.t>,
        array<Specification.t>,
        array<GlobalProp.t>,
        array<Warning.t>,
      )
    | Inspect(array<ProofObligation.t>)
    | Resolve(int)
    | Substitute(int, Syntax.Expr.t)
    | UpdateSpecPositions(array<Loc.t>)
    | ConsoleLog(string)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "ResError" => Contents(array(Error.decode) |> map(errors => Error(errors)))
    | "ResOK" =>
      Contents(
        tuple5(
          int,
          array(ProofObligation.decode),
          array(Specification.decode),
          array(GlobalProp.decode),
          array(Warning.decode),
        ) |> map(((id, obs, specs, globalProps, warnings)) => OK(
          id,
          obs,
          specs,
          globalProps,
          warnings,
        )),
      )
    | "ResInspect" => Contents(array(ProofObligation.decode) |> map(pos => Inspect(pos)))
    | "ResSubstitute" =>
      Contents(pair(int, Syntax.Expr.decode) |> map(((i, expr)) => Substitute(i, expr)))
    | "ResUpdateSpecPositions" => Contents(array(Loc.decode) |> map(locs => UpdateSpecPositions(locs)))
    | "ResResolve" => Contents(int |> map(i => Resolve(i)))
    | "ResConsoleLog" => Contents(string |> map(i => ConsoleLog(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag))
    }
  )
}

type t =
  | Res(string, array<Kind.t>)
  | NotLoaded
  | CannotDecodeResponse(string, Js.Json.t)
  | CannotDecodeRequest(string)
  | CannotSendRequest(string)

open Json.Decode
open Util.Decode
let decode: decoder<t> = sum(x =>
  switch x {
  | "Res" =>
    Contents(pair(string, array(Kind.decode)) |> map(((filePath, kinds)) => Res(filePath, kinds)))
  | "NotLoaded" => TagOnly(_ => NotLoaded)
  | "CannotDecodeRequest" => Contents(string |> map(msg => CannotDecodeRequest(msg)))
  | tag => raise(DecodeError("Unknown constructor: " ++ tag))
  }
)
