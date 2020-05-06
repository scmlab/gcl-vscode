open React;

module Paren = {
  [@react.component]
  let make = (~activate, ~children) => {
    let (hovered, setHover) = React.useState(_ => false);
    let (collapsed, setCollapse) = React.useState(_ => false);
    let onMouseOver = _ => setHover(_ => true);
    let onMouseLeave = _ => setHover(_ => false);
    let onClick = _ => setCollapse(_ => collapsed ? false : true);
    let className =
      "expr-paren"
      ++ (hovered ? " hovered" : "")
      ++ (collapsed ? " collapsed" : "");

    if (activate) {
      if (collapsed) {
        <div className onMouseOver onMouseLeave onClick>
          {string("(...)")}
        </div>;
      } else {
        <>
          <div className onMouseOver onMouseLeave onClick>
            {string("(")}
          </div>
          children
          <div className onMouseOver onMouseLeave onClick>
            {string(")")}
          </div>
        </>;
      };
    } else {
      children;
    };
  };
};

module Space = {
  [@react.component]
  let make = () => <div> {string(" ")} </div>;
};

module Low = {
  [@react.component]
  let make = (~value: GCL.Syntax.Name.t) =>
    <div> {string(GCL.Syntax.Name.toString(value))} </div>;
};

module Operator = {
  open GCL.Syntax.Op;

  [@react.component]
  let make = (~value: t, ~loc: GCL.loc) =>
    <Link loc> {string(toString(value))} </Link>;
};

module Prec = {
  open GCL.Syntax;
  open VarArg;
  // module VarArg = Syntax.VarArg;

  let rec handleOperator = (n, op, loc) => {
    module Self = {
      let make = make;
      let makeProps = makeProps;
    };
    switch (GCL.Syntax.Expr.Precedence.classify(op)) {
    | Infix(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec={m + 1} value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec={m + 1} value=q />
        </Paren>,
      );
    | InfixL(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec=m value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec={m + 1} value=q />
        </Paren>,
      );
    | InfixR(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec={m + 1} value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec=m value=q />
        </Paren>,
      );
    | Prefix(m) =>
      let%VarArg p = var;
      Complete(
        <Paren activate={n > m}>
          <Operator value=op loc />
          <Space />
          <Self prec=m value=p />
        </Paren>,
      );
    | Postfix(m) =>
      let%VarArg p = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec=m value=p />
          <Space />
          <Operator value=op loc />
        </Paren>,
      );
    };
  }
  and handleExpr = n => {
    module Self = {
      let make = make;
      let makeProps = makeProps;
    };
    GCL.Syntax.Expr.(
      fun
      | Lit(lit, loc) =>
        Complete(<Link loc> {string(Lit.toString(lit))} </Link>)
      | Var(s, loc) =>
        Complete(<Link loc> {string(Name.toString(s))} </Link>)
      | Const(s, loc) =>
        Complete(<Link loc> {string(Name.toString(s))} </Link>)
      | Op(op, loc) =>
        // HACK: if the precedence is smaller than 0, display the operator directly
        n >= 0
          ? handleOperator(n, op, loc) : Complete(<Operator value=op loc />)
      | App(p, q, _) =>
        switch (handleExpr(n, p)) {
        | Expect(f) => f(q)
        | Complete(s) =>
          switch (handleExpr(n, q)) {
          | Expect(g) => Expect(g)
          | Complete(t) =>
            switch (q) {
            | App(_, _, _) =>
              Complete(<> s <Paren activate=true> t </Paren> </>)
            | _ => Complete(<> s <Space /> t </>)
            }
          }
        }
      | Lam(x, body, loc) =>
        Complete(
          <Link loc>
            {string("\\")}
            {string(x)}
            {string(" -> ")}
            <Self prec=0 value=body />
          </Link>,
        )
      | Quant(op, vars, p, q, loc) =>
        Complete(
          <Link loc>
            {string("<")}
            <Space />
            <Self prec=(-1) value=op />
            <Space />
            {Util.React.sepBy(
               <Space />,
               Array.map(value => <Low value />, vars),
             )}
            <Space />
            {string(":")}
            <Space />
            <Self prec=0 value=p />
            <Space />
            {string(":")}
            <Space />
            <Self prec=0 value=q />
            <Space />
            {string(">")}
          </Link>,
        )
      | Subst(x, _subst) =>
        Complete(<Link loc=GCL.Loc.NoLoc> <Self prec=0 value=x /> </Link>)
      | Hole(loc) => Complete(<Link loc> {string("[?]")} </Link>)
      | Unknown(x) =>
        Complete(
          <Link loc=GCL.Loc.NoLoc>
            {string("[? " ++ Js.Json.stringify(x) ++ " ?]")}
          </Link>,
        )
    );
  }
  [@react.component]
  and make = (~prec, ~value) =>
    switch (handleExpr(prec, value)) {
    | Expect(_) => <> </>
    | Complete(s) => s
    };
};

[@react.component]
let make = (~value: GCL.Syntax.Expr.t) =>
  <div className="expr"> <Prec prec=0 value /> </div>;