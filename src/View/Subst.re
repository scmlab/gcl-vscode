open React;

open Common;

type event =
  | Request(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
  | Response(int, GCL.Syntax.Expr.t);

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
  let (reduced, setReduced) = React.useState(_ => None);

  let id = React.useRef(None);
  let onMouseOver = _ => setHover(_ => true);
  let onMouseLeave = _ => setHover(_ => false);
  let onClick = _ => {
    emitter.emit(Request(counter^, expr, subst));
    React.Ref.setCurrent(id, Some(counter^));
    counter := counter^ + 1;
  };
  let className =
    "expr-subst"
    ++ (hovered ? " hovered" : "")
    ++ (Belt.Option.isSome(reduced) ? " reduced" : "");

  React.useEffect1(
    () =>
      Some(
        emitter.on(
          fun
          | Request(_, _, _) => ()
          | Response(i, expr) =>
            if (Some(i) == React.Ref.current(id)) {
              setReduced(_ => Some(expr));
            },
        ),
      ),
    [||],
  );

  switch (reduced) {
  | None =>
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
    </>
  | Some(expr) => <Expr prec=0 value=expr />
  };
  // <div className onMouseOver onMouseLeave onClick> {string("(...)")} </div>;
};