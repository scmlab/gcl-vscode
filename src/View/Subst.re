open React;

open Common;

type event =
  | Request(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
  | Response(int, GCL.Syntax.Expr.subst);

let emitter: Event.t(event) = Event.make();
let eventContext = React.createContext(emitter);

module Provider = {
  let makeProps = (~value, ~children, ()) => {
    "value": value,
    "children": children,
  };

  let make = React.Context.provider(eventContext);
};

let counter = ref(0);

[@react.component]
let make = (~expr, ~subst, ~makeExpr, ~makeExprProps) => {
  module Expr = {
    let make = makeExpr;
    let makeProps = makeExprProps;
  };

  let emitter = React.useContext(eventContext);

  let (hovered, setHover) = React.useState(_ => false);
  let (reduced, setReduced) = React.useState(_ => false);
  let onMouseOver = _ => setHover(_ => true);
  let onMouseLeave = _ => setHover(_ => false);
  let onClick = _ => {
    setReduced(_ => true);
    emitter.emit(Request(counter^, expr, subst));
    counter := counter^ + 1;
  };
  let className =
    "expr-subst" ++ (hovered ? " hovered" : "") ++ (reduced ? " reduced" : "");

  <>
    <Expr prec=0 value=expr />
    <Space />
    <div className onMouseOver onMouseLeave onClick>
      {string("[")}
      {subst
       ->Js.Dict.entries
       ->Belt.Array.map(((x, x')) =>
           <> <Expr prec=0 value=x' /> {string("/" ++ x)} </>
         )
       ->(
           e =>
             Util.React.sepBy(
               {
                 string(", ");
               },
               e,
             )
         )}
      {string("]")}
    </div>
  </>;
  // <div className onMouseOver onMouseLeave onClick> {string("(...)")} </div>;
};