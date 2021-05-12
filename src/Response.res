open Belt
open GCL

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

module Kind = {
  type t =
    | Display(array<Element.Block.t>)
    | OK(
        int,
        array<ProofObligation.t>,
        array<Element.Block.t>,
      )
    | Inspect(array<ProofObligation.t>)
    | Resolve(int)
    | Substitute(int, Syntax.Expr.t)
    | ConsoleLog(string)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "ResDisplay" => Contents(array(Element.Block.decode) |> map(errors => Display(errors)))
    | "ResOK" =>
      Contents(
        tuple3(
          int,
          array(ProofObligation.decode),
          array(Element.Block.decode),
        ) |> map(((id, obs, blocks)) => OK(
          id,
          obs,
          blocks
        )),
      )
    | "ResInspect" => Contents(array(ProofObligation.decode) |> map(pos => Inspect(pos)))
    | "ResSubstitute" =>
      Contents(pair(int, Syntax.Expr.decode) |> map(((i, expr)) => Substitute(i, expr)))
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
