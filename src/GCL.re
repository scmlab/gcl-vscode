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
    tuple4(string, int, int, int) |> map(((w, x, y, _)) => Pos(w, x, y));

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Pos(path, line, column) =>
      (path, line, column, 0) |> tuple4(string, int, int, int);
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
          pair(Pos.decode, Pos.decode) |> map(((x, y)) => Loc(x, y)),
        )
      | "NoLoc" => TagOnly(_ => NoLoc)
      | tag => raise(DecodeError("[Loc] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | NoLoc => object_([("tag", string("NoLoc"))])
    | Loc(x, y) =>
      object_([
        ("tag", string("Loc")),
        ("contents", (x, y) |> pair(Pos.encode, Pos.encode)),
      ]);
};

type pos = Pos.t;
type loc = Loc.t;

type mode =
  | WP1
  | WP2;

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
    // let let_ = flatMap;

    let var = Expect(x => Complete(x));
  };

  module Lit = {
    type t =
      | Num(int)
      | Bool(bool)
      | Unknown(Js.Json.t);

    let toString =
      fun
      | Num(i) => string_of_int(i)
      | Bool(true) => "True"
      | Bool(false) => "False"
      | Unknown(json) => Js.Json.stringify(json);

    open Util.Decode;
    open Json.Decode;
    let decode: decoder(t) =
      json =>
        json
        |> sum(
             fun
             | "Num" => Contents(int |> map(x => Num(x)))
             | "Bol" => Contents(bool |> map(x => Bool(x)))
             | _ => Contents(json => Unknown(json)),
           );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Num(i) => object_([("tag", string("Num")), ("contents", i |> int)])
      | Bool(x) =>
        object_([("tag", string("Bol")), ("contents", x |> bool)])
      | _ => object_([("tag", string("Num")), ("contents", 42 |> int)]);
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
      | Mod
      | Unknown(string);

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
      | Mod => "%"
      | Unknown(string) => string;

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
           | string => Unknown(string),
         );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | EQ => string("EQ")
      | NEQ => string("NEQ")
      | LTE => string("LTE")
      | GTE => string("GTE")
      | LT => string("LT")
      | GT => string("GT")
      | Implies => string("Implies")
      | Disj => string("Disj")
      | Conj => string("Conj")
      | Neg => string("Neg")
      | Add => string("Add")
      | Sub => string("Sub")
      | Mul => string("Mul")
      | Div => string("Div")
      | Mod => string("Mod")
      | Unknown(x) => string(x);
  };

  module Name = {
    type t =
      | Name(string, loc);
    let toString =
      fun
      | Name(x, _) => x;
    open Json.Decode;
    let decode: decoder(t) =
      pair(string, Loc.decode) |> map(((x, r)) => Name(x, r));
    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Name(s, loc) => (s, loc) |> pair(string, Loc.encode);
  };

  module Expr = {
    type t =
      | Lit(Lit.t, loc)
      | Var(Name.t, loc)
      | Const(Name.t, loc)
      | Op(Op.t, loc)
      | App(t, t, loc)
      | Lam(string, t, loc)
      | Hole(loc)
      // (+ i : 0 <= i && i < N : f i)
      | Quant(t, array(Name.t), t, t, loc)
      | Subst(t, subst)
      | Unknown(Js.Json.t)
    and subst = Js.Dict.t(t);

    let locOf =
      fun
      | Lit(_, loc) => loc
      | Var(_, loc) => loc
      | Const(_, loc) => loc
      | Op(_, loc) => loc
      | App(_, _, loc) => loc
      | Lam(_, _, loc) => loc
      | Hole(loc) => loc
      | Quant(_, _, _, _, loc) => loc
      | Subst(_, _) => Loc.NoLoc
      | Unknown(_) => Loc.NoLoc;

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
             | "Lit" =>
               Contents(
                 pair(Lit.decode, Loc.decode) |> map(((x, r)) => Lit(x, r)),
               )
             | "Var" =>
               Contents(
                 pair(Name.decode, Loc.decode)
                 |> map(((x, r)) => Var(x, r)),
               )
             | "Const" =>
               Contents(
                 pair(Name.decode, Loc.decode)
                 |> map(((x, r)) => Const(x, r)),
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
             | "Lam" =>
               Contents(
                 tuple3(string, decode, Loc.decode)
                 |> map(((x, y, r)) => Lam(x, y, r)),
               )
             | "Hole" => Contents(Loc.decode |> map(r => Hole(r)))
             | "Quant" =>
               Contents(
                 Util.Decode.tuple5(
                   decode,
                   array(Name.decode),
                   decode,
                   decode,
                   Loc.decode,
                 )
                 |> map(((op, vars, p, q, l)) => Quant(op, vars, p, q, l)),
               )
             | "Subst" =>
               Contents(
                 pair(decode, decodeSubst) |> map(((x, y)) => Subst(x, y)),
               )
             | _ => Contents(json => Unknown(json)),
           )
    and decodeSubst: decoder(subst) = json => json |> dict(decode);

    open! Json.Encode;
    open! Util.Encode;
    let rec encode: encoder(t) =
      fun
      | Lit(lit, loc) =>
        object_([
          ("tag", string("Lit")),
          ("contents", (lit, loc) |> pair(Lit.encode, Loc.encode)),
        ])
      | Var(s, loc) =>
        object_([
          ("tag", string("Var")),
          ("contents", (s, loc) |> pair(Name.encode, Loc.encode)),
        ])
      | Const(s, loc) =>
        object_([
          ("tag", string("Const")),
          ("contents", (s, loc) |> pair(Name.encode, Loc.encode)),
        ])
      | Op(op, loc) =>
        object_([
          ("tag", string("Op")),
          ("contents", (op, loc) |> pair(Op.encode, Loc.encode)),
        ])
      | App(e, f, loc) =>
        object_([
          ("tag", string("App")),
          ("contents", (e, f, loc) |> tuple3(encode, encode, Loc.encode)),
        ])
      | Lam(x, body, loc) =>
        object_([
          ("tag", string("Lam")),
          (
            "contents",
            (x, body, loc) |> tuple3(string, encode, Loc.encode),
          ),
        ])
      | Hole(loc) =>
        object_([("tag", string("Hole")), ("contents", loc |> Loc.encode)])
      | Quant(e, lowers, f, g, loc) =>
        object_([
          ("tag", string("Quant")),
          (
            "contents",
            (e, lowers, f, g, loc)
            |> tuple5(encode, array(Name.encode), encode, encode, Loc.encode),
          ),
        ])
      | Subst(e, subst) =>
        object_([
          ("tag", string("Subst")),
          ("contents", (e, subst) |> pair(encode, encodeSubst)),
        ])
      | _ =>
        object_([
          ("tag", string("Hole")),
          ("contents", Loc.NoLoc |> Loc.encode),
        ])
    and encodeSubst: encoder(subst) = json => json |> dict(encode);

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
        | Mod => InfixL(9)
        | Unknown(_) => InfixL(9);

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
          var->VarArg.flatMap(p =>
            var->VarArg.flatMap(q => {
              Complete(
                parensIf(
                  n > m,
                  toString(m + 1, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m + 1, q),
                ),
              )
            })
          )
        | InfixL(m) =>
          var->VarArg.flatMap(p =>
            var->VarArg.flatMap(q => {
              Complete(
                parensIf(
                  n > m,
                  toString(m, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m + 1, q),
                ),
              )
            })
          )
        | InfixR(m) =>
          var->VarArg.flatMap(p =>
            var->VarArg.flatMap(q => {
              Complete(
                parensIf(
                  n > m,
                  toString(m + 1, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m, q),
                ),
              )
            })
          )
        | Prefix(m) =>
          var->VarArg.flatMap(p =>
            Complete(
              parensIf(n > m, Op.toString(op) ++ " " ++ toString(m, p)),
            )
          )
        | Postfix(m) =>
          var->VarArg.flatMap(p =>
            Complete(
              parensIf(n > m, toString(m, p) ++ " " ++ Op.toString(op)),
            )
          )
        }
      and handleExpr = n =>
        fun
        | Lit(lit, _) => Complete(Lit.toString(lit))
        | Var(s, _) => Complete(Name.toString(s))
        | Const(s, _) => Complete(Name.toString(s))
        | Op(op, _) => handleOperator(n, op)
        | App(p, q, _) =>
          switch (handleExpr(n, p)) {
          // this branch happens when `p` is an `Op`
          | Expect(f) => f(q)
          // otherwise, examine `q`
          | Complete(s) =>
            switch (handleExpr(n, q)) {
            // this branch happens when `q` is also an `Op`
            | Expect(g) => Expect(g)
            | Complete(t) =>
              // otherwise, juxtapose both
              switch (q) {
              | App(_, _, _) => Complete(s ++ " " ++ parensIf(true, t))
              | _ => Complete(s ++ " " ++ t)
              }
            }
          }
        | Lam(x, body, _) =>
          Complete("\\" ++ x ++ " -> " ++ toString(0, body))
        | Hole(_) => Complete("[?]")
        | Quant(op, vars, p, q, _) =>
          Complete(
            "< "
            ++ toString(0, op)
            ++ " "
            ++ Js.String.concatMany(Array.map(vars, Name.toString), " ")
            ++ " : "
            ++ toString(0, p)
            ++ " : "
            ++ toString(0, q)
            ++ " >",
          )
        | Subst(expr, _subst) => handleExpr(n, expr)
        | Unknown(x) =>
          Complete("[Uknown expr: " ++ Js.Json.stringify(x) ++ "]")
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
             | tag =>
               raise(DecodeError("[Pred] Unknown constructor: " ++ tag)),
           );

    open! Json.Encode;
    let rec encode: encoder(t) =
      fun
      | Constant(e) =>
        object_([
          ("tag", string("Constant")),
          ("contents", e |> Expr.encode),
        ])
      | Bound(e, l) =>
        object_([
          ("tag", string("Bound")),
          ("contents", (e, l) |> pair(Expr.encode, Loc.encode)),
        ])
      | Assertion(e, l) =>
        object_([
          ("tag", string("Assertion")),
          ("contents", (e, l) |> pair(Expr.encode, Loc.encode)),
        ])
      | LoopInvariant(e, f, l) =>
        object_([
          ("tag", string("LoopInvariant")),
          (
            "contents",
            (e, f, l) |> tuple3(Expr.encode, Expr.encode, Loc.encode),
          ),
        ])
      | GuardIf(e, l) =>
        object_([
          ("tag", string("GuardIf")),
          ("contents", (e, l) |> pair(Expr.encode, Loc.encode)),
        ])
      | GuardLoop(e, l) =>
        object_([
          ("tag", string("GuardLoop")),
          ("contents", (e, l) |> pair(Expr.encode, Loc.encode)),
        ])
      | Conjunct(ts) =>
        object_([
          ("tag", string("Conjunct")),
          ("contents", ts |> array(encode)),
        ])
      | Disjunct(ts) =>
        object_([
          ("tag", string("Disjunct")),
          ("contents", ts |> array(encode)),
        ])
      | Negate(t) =>
        object_([("tag", string("Negate")), ("contents", t |> encode)]);

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
