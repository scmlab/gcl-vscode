open Belt;
module Pos = {
  type t =
    | Pos(string, int, int);

  let toString =
    fun
    | Pos(_, line, column) =>
      string_of_int(line) ++ ":" ++ string_of_int(column);

  let translate = by =>
    fun
    | Pos(path, line, column) => {
        let Pos(_, y, x) = by;
        Pos(path, line + y, column + x);
      };

  let translateBy = (y, x) =>
    fun
    | Pos(path, line, column) => Pos(path, line + y, column + x);

  open Json.Decode;
  let decode: decoder(t) =
    json =>
      Pos(
        field("filepath", string, json),
        field("line", int, json),
        field("column", int, json),
      );
};

module Loc = {
  type t =
    | NoLoc
    | Loc(Pos.t, Pos.t);

  let toString =
    fun
    | NoLoc => "NoLoc"

    | Loc(x, y) => Pos.toString(x) ++ "-" ++ Pos.toString(y);

  let translate = by =>
    fun
    | NoLoc => by

    | Loc(x, y) =>
      switch (by) {
      | NoLoc => Loc(x, y)
      | Loc(w, v) => Loc(Pos.translate(x, w), Pos.translate(y, v))
      };

  let translateBy = (startY, startX, endY, endX) =>
    fun
    | NoLoc => Loc(Pos("", startY, startX), Pos("", endY, endX))
    | Loc(x, y) =>
      Loc(
        Pos.translateBy(startY, startX, x),
        Pos.translateBy(endY, endX, y),
      );

  open Util.Decode;
  open Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Loc" =>
        Contents(
          json =>
            Loc(
              field("start", Pos.decode, json),
              field("end", Pos.decode, json),
            ),
        )
      | "NoLoc" => TagOnly(_ => NoLoc)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

type pos = Pos.t;
type loc = Loc.t;

module Syntax = {
  // for pretty printing
  module VarArg = {
    type t('a, 'b) =
      | Expect('a => t('a, 'b))
      | Complete('b);

    let return = x => Complete(x);
    let rec flatMap = (x, f) =>
      switch (x) {
      | Expect(g) => Expect(x => flatMap(g(x), f))
      | Complete(x) => f(x)
      };
    let let_ = flatMap;

    let var = Expect(x => Complete(x));
  };

  module Lit = {
    type t =
      | Num(int)
      | Bool(bool);

    let toString =
      fun
      | Num(i) => string_of_int(i)
      | Bool(true) => "True"
      | Bool(false) => "False";

    open Util.Decode;
    open Json.Decode;
    let decode: decoder(t) =
      json =>
        json
        |> sum(
             fun
             | "Num" => Contents(int |> map(x => Num(x)))
             | "Bol" => Contents(bool |> map(x => Bool(x)))
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           );
  };

  module Op = {
    type t =
      | EQ
      | NEQ
      | LTE
      | GTE
      | LT
      | GT
      | Implies
      | Conj
      | Disj
      | Neg
      | Add
      | Sub
      | Mul
      | Div
      | Mod;

    let toString =
      fun
      | EQ => "="
      | NEQ => {j|≠|j}
      | LTE => {j|≤|j}
      | GTE => {j|≥|j}
      | LT => "<"
      | GT => ">"
      | Implies => {j|→|j}
      | Disj => {j|∨|j}
      | Conj => {j|∧|j}
      | Neg => {j|¬|j}
      | Add => "+"
      | Sub => "-"
      | Mul => {j|×|j}
      | Div => {j|÷|j}
      | Mod => "%";

    open Json.Decode;
    let decode: decoder(t) =
      string
      |> map(
           fun
           | "EQ" => EQ
           | "NEQ" => NEQ
           | "LTE" => LTE
           | "GTE" => GTE
           | "LT" => LT
           | "GT" => GT
           | "Implies" => Implies
           | "Conj" => Conj
           | "Disj" => Disj
           | "Neg" => Neg
           | "Add" => Add
           | "Sub" => Sub
           | "Mul" => Mul
           | "Div" => Div
           | "Mod" => Mod
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         );
  };

  module Upper = {
    type t =
      | Upper(string, loc);
    let toString =
      fun
      | Upper(x, _) => x;
    open Json.Decode;
    let decode: decoder(t) =
      pair(string, Loc.decode) |> map(((x, r)) => Upper(x, r));
  };

  module Lower = {
    type t =
      | Lower(string, loc);
    let toString =
      fun
      | Lower(x, _) => x;
    open Json.Decode;
    let decode: decoder(t) =
      pair(string, Loc.decode) |> map(((x, r)) => Lower(x, r));
  };

  module Expr = {
    type t =
      | Var(string, loc)
      | Const(string, loc)
      | Lit(Lit.t, loc)
      | Op(Op.t, loc)
      | App(t, t, loc)
      // (+ i : 0 <= i && i < N : f i)
      | Quant(t, array(Lower.t), t, t, loc)
      | Hole(loc)
    and subst = Js.Dict.t(t);

    let locOf =
      fun
      | Var(_, loc) => loc
      | Const(_, loc) => loc
      | Lit(_, loc) => loc
      | Op(_, loc) => loc
      | App(_, _, loc) => loc
      | Quant(_, _, _, _, loc) => loc
      | Hole(loc) => loc;

    let negate = x => App(Op(Op.Neg, NoLoc), x, NoLoc);
    let disj = (x, y) => App(App(Op(Op.Disj, NoLoc), x, NoLoc), y, NoLoc);
    let conj = (x, y) => App(App(Op(Op.Conj, NoLoc), x, NoLoc), y, NoLoc);
    let rec disjunct' =
      fun
      | [] => Lit(Bool(true), NoLoc)
      | [x] => x
      | [x, ...xs] => disj(x, disjunct'(xs));
    let rec conjunct' =
      fun
      | [] => Lit(Bool(false), NoLoc)
      | [x] => x
      | [x, ...xs] => conj(x, conjunct'(xs));
    let disjunct = xs => xs->List.fromArray->disjunct';
    let conjunct = xs => xs->List.fromArray->conjunct';

    open Util.Decode;
    open Json.Decode;

    let rec decode: decoder(t) =
      json =>
        json
        |> sum(
             fun
             | "Var" =>
               Contents(
                 pair(Lower.decode, Loc.decode)
                 |> map(((x, r)) => Var(Lower.toString(x), r)),
               )
             | "Const" =>
               Contents(
                 pair(Upper.decode, Loc.decode)
                 |> map(((x, r)) => Const(Upper.toString(x), r)),
               )
             | "Lit" =>
               Contents(
                 pair(Lit.decode, Loc.decode) |> map(((x, r)) => Lit(x, r)),
               )
             | "Op" =>
               Contents(
                 pair(Op.decode, Loc.decode) |> map(((x, r)) => Op(x, r)),
               )
             | "App" =>
               Contents(
                 tuple3(decode, decode, Loc.decode)
                 |> map(((x, y, r)) => App(x, y, r)),
               )
             | "Quant" =>
               Contents(
                 Util.Decode.tuple5(
                   decode,
                   array(Lower.decode),
                   decode,
                   decode,
                   Loc.decode,
                 )
                 |> map(((op, vars, p, q, l)) => Quant(op, vars, p, q, l)),
               )
             | "Hole" => Contents(Loc.decode |> map(r => Hole(r)))
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           )
    and decodeSubst: decoder(subst) = json => json |> dict(decode);

    module Precedence = {
      open VarArg;

      open! Op;

      type fixity =
        | InfixL(int)
        | InfixR(int)
        | Infix(int)
        | Prefix(int)
        | Postfix(int);

      let classify =
        fun
        | Implies => InfixR(1)
        | Disj => InfixL(2)
        | Conj => InfixL(3)
        | Neg => Prefix(4)
        | EQ => Infix(5)
        | NEQ => Infix(6)
        | LTE => Infix(6)
        | GTE => Infix(6)
        | LT => Infix(6)
        | GT => Infix(6)
        | Add => InfixL(7)
        | Sub => InfixL(7)
        | Mul => InfixL(8)
        | Div => InfixL(8)
        | Mod => InfixL(9);

      // adds parentheses when True
      let parensIf = (p, s) =>
        if (p) {
          "(" ++ s ++ ")";
        } else {
          s;
        };

      let rec handleOperator = (n, op) =>
        switch (classify(op)) {
        | Infix(m) =>
          let%VarArg p = var;
          let%VarArg q = var;
          Complete(
            parensIf(
              n > m,
              toString(m + 1, p)
              ++ " "
              ++ Op.toString(op)
              ++ " "
              ++ toString(m + 1, q),
            ),
          );
        | InfixL(m) =>
          let%VarArg p = var;
          let%VarArg q = var;

          Complete(
            parensIf(
              n > m,
              toString(m, p)
              ++ " "
              ++ Op.toString(op)
              ++ " "
              ++ toString(m + 1, q),
            ),
          );
        | InfixR(m) =>
          let%VarArg p = var;
          let%VarArg q = var;
          Complete(
            parensIf(
              n > m,
              toString(m + 1, p)
              ++ " "
              ++ Op.toString(op)
              ++ " "
              ++ toString(m, q),
            ),
          );
        | Prefix(m) =>
          let%VarArg p = var;
          Complete(
            parensIf(n > m, Op.toString(op) ++ " " ++ toString(m, p)),
          );
        | Postfix(m) =>
          let%VarArg p = var;
          Complete(
            parensIf(n > m, toString(m, p) ++ " " ++ Op.toString(op)),
          );
        }
      and handleExpr = n =>
        fun
        | Var(s, _) => Complete(s)
        | Const(s, _) => Complete(s)
        | Lit(lit, _) => Complete(Lit.toString(lit))
        | Op(op, _) => handleOperator(n, op)
        | App(p, q, _) =>
          switch (handleExpr(n, p)) {
          | Expect(f) => f(q)
          | Complete(s) =>
            switch (handleExpr(n, q)) {
            | Expect(g) => Expect(g)
            | Complete(t) =>
              switch (q) {
              | App(_, _, _) => Complete(s ++ " " ++ parensIf(true, t))
              | _ => Complete(s ++ " " ++ t)
              }
            }
          }
        | Quant(op, vars, p, q, _) =>
          Complete(
            "< "
            ++ toString(0, op)
            ++ " "
            ++ Js.String.concatMany(Array.map(vars, Lower.toString), " ")
            ++ " : "
            ++ toString(0, p)
            ++ " : "
            ++ toString(0, q)
            ++ " >",
          )
        | Hole(_) => Complete("[?]")
      // | Hole(_) => Complete("[" ++ string_of_int(i) ++ "]")
      and toString = (n, p) =>
        switch (handleExpr(n, p)) {
        | Expect(_) => ""
        | Complete(s) => s
        };
    };

    let toString = Precedence.toString(0);
  };

  module Pred = {
    type t =
      | Constant(Expr.t)
      | Bound(Expr.t, Loc.t)
      | Assertion(Expr.t, Loc.t)
      | LoopInvariant(Expr.t, Expr.t, Loc.t)
      | GuardIf(Expr.t, Loc.t)
      | GuardLoop(Expr.t, Loc.t)
      | Conjunct(array(t))
      | Disjunct(array(t))
      | Negate(t);

    open Util.Decode;
    open Json.Decode;
    let rec decode: decoder(t) =
      json =>
        json
        |> sum(
             fun
             | "Constant" => Contents(Expr.decode |> map(x => Constant(x)))
             | "Bound" =>
               Contents(
                 pair(Expr.decode, Loc.decode)
                 |> map(((e, l)) => Bound(e, l)),
               )
             | "Assertion" =>
               Contents(
                 pair(Expr.decode, Loc.decode)
                 |> map(((e, l)) => Assertion(e, l)),
               )
             | "LoopInvariant" =>
               Contents(
                 tuple3(Expr.decode, Expr.decode, Loc.decode)
                 |> map(((e, bnd, l)) => LoopInvariant(e, bnd, l)),
               )
             | "GuardIf" =>
               Contents(
                 pair(Expr.decode, Loc.decode)
                 |> map(((e, l)) => GuardIf(e, l)),
               )
             | "GuardLoop" =>
               Contents(
                 pair(Expr.decode, Loc.decode)
                 |> map(((e, l)) => GuardLoop(e, l)),
               )
             | "Conjunct" =>
               Contents(array(decode) |> map(xs => Conjunct(xs)))
             | "Disjunct" =>
               Contents(array(decode) |> map(xs => Disjunct(xs)))
             | "Negate" => Contents(decode |> map(x => Negate(x)))
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           );

    let rec toExpr =
      fun
      | Constant(e) => e
      | Bound(e, _) => e
      | Assertion(e, _) => e
      | LoopInvariant(e, _, _) => e
      | GuardIf(e, _) => e
      | GuardLoop(e, _) => e
      | Conjunct(xs) => xs->Array.map(toExpr)->Expr.conjunct
      // xs |> Array.map(toExpr) |> Expr.conjunct
      | Disjunct(xs) => xs->Array.map(toExpr)->Expr.disjunct
      | Negate(x) => x->toExpr->Expr.negate;

    let toString = xs => xs->toExpr->Expr.toString;
  };

  module Type = {
    module Base = {
      type t =
        | Int
        | Bool;

      let toString =
        fun
        | Int => "Int"
        | Bool => "Bool";

      open Json.Decode;
      let decode: decoder(t) =
        string
        |> map(
             fun
             | "TInt" => Int
             | "TBool" => Bool
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           );
    };

    type t =
      | Base(Base.t)
      | Array(t)
      | Func(t, t)
      | Var(int);

    let rec toString =
      fun
      | Base(b) => Base.toString(b)
      | Array(t) => "Array " ++ toString(t)
      | Func(s, t) => toString(s) ++ " -> " ++ toString(t)
      | Var(i) => "Var " ++ string_of_int(i);

    open Util.Decode;
    open Json.Decode;
    let rec decode: decoder(t) =
      json =>
        json
        |> sum(
             fun
             | "TBase" => Contents(Base.decode |> map(x => Base(x)))
             | "TArray" => Contents(decode |> map(x => Array(x)))
             | "TFun" =>
               Contents(
                 pair(decode, decode) |> map(((x, y)) => Func(x, y)),
               )
             | "TVar" => Contents(int |> map(x => Var(x)))
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           );
  };
};

module Response = {
  open Syntax;
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
        | "AtTermination" =>
          Contents(Loc.decode |> map(x => AtTermination(x)))
        | "AtBoundDecrement" =>
          Contents(Loc.decode |> map(x => AtBoundDecrement(x)))
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );

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
  };

  module ProofObligation = {
    type t =
      | ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t);

    open Json.Decode;
    let decode: decoder(t) =
      tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode)
      |> map(((i, p, q, o)) => ProofObligation(i, p, q, o));
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

  module Error = {
    module Site = {
      type t =
        | Global(loc)
        | Local(loc, int);

      let toLoc = (site, specifications) => {
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
            Contents(
              pair(Loc.decode, int) |> map(((r, i)) => Local(r, i)),
            )
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
        | MissingBound
        | MissingAssertion
        | MissingLoopInvariant
        | ExcessBound
        | MissingPrecondition
        | MissingPostcondition
        | DigHole;

      open Json.Decode;
      open Util.Decode;
      let decode: decoder(t) =
        sum(
          fun
          | "MissingBound" => Contents(_ => MissingBound)
          | "MissingAssertion" => Contents(_ => MissingAssertion)
          | "MissingLoopInvariant" => Contents(_ => MissingLoopInvariant)
          | "ExcessBound" => Contents(_ => ExcessBound)
          | "MissingPrecondition" => Contents(_ => MissingPrecondition)
          | "MissingPostcondition" => Contents(_ => MissingPostcondition)
          | "DigHole" => Contents(_ => DigHole)
          | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
        );
    };

    type kind =
      | LexicalError
      | SyntacticError(array(string))
      | StructError(StructError.t)
      | TypeError(TypeError.t)
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
        | "StructError2" =>
          Contents(json => StructError(json |> StructError.decode))
        | "TypeError" =>
          Contents(json => TypeError(json |> TypeError.decode))
        | "CannotReadFile" =>
          Contents(json => CannotReadFile(json |> string))
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
    | OK(array(ProofObligation.t), array(Specification.t))
    | Resolve(int)
    | InsertAssertion(int, Syntax.Expr.t)
    | UnknownResponse(Js.Json.t);

  open Json.Decode;
  open Util.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Error" =>
        Contents(array(Error.decode) |> map(errors => Error(errors)))
      | "OK" =>
        Contents(
          pair(array(ProofObligation.decode), array(Specification.decode))
          |> map(((obs, specs)) => OK(obs, specs)),
        )
      | "Insert" =>
        Contents(
          pair(int, Syntax.Expr.decode)
          |> map(((i, expr)) => InsertAssertion(i, expr)),
        )
      | "Resolve" => Contents(int |> map(i => Resolve(i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};