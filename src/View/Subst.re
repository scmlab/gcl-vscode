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

//
type redux =
  | Unreduced(GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
  | Reducing
  | Reduced(option(int), GCL.Syntax.Expr.t);

// a global counter for generating fresh IDs for <Subst>
let counter = ref(0);

[@react.component]
let make = (~expr, ~subst, ~makeExpr, ~makeExprProps) => {
  module Expr = {
    let make = makeExpr;
    let makeProps = makeExprProps;
  };
  let emitter = React.useContext(eventContext);
  let (hovered, setHover) = React.useState(_ => false);
  let (redux, setRedux) = React.useState(_ => Unreduced(expr, subst));
  let id = React.useRef(None);
  let reqID = React.useRef(None);
  let onMouseOver = _ => setHover(_ => true);
  let onMouseLeave = _ => setHover(_ => false);

  let onClick = _ => {
    emitter.emit(Request(counter^, expr, subst));
    // bump the counter
    React.Ref.setCurrent(id, Some(counter^));
    counter := counter^ + 1;
  };

  let className =
    "expr-subst"
    ++ (hovered ? " hovered" : "")
    ++ (
      switch (redux) {
      | Reduced(_, _) => " reduced"
      | _ => ""
      }
    );

  React.Ref.setCurrent(reqID, React.useContext(ReqID.context));

  // cache invalidaction
  switch (redux) {
  | Reduced(cachedID, _) =>
    if (cachedID != React.Ref.current(reqID)) {
      Js.log("invalidate!! ");
      setRedux(_ => Unreduced(expr, subst));
    }
  | _ => ()
  };

  // listen to the response broadcast
  React.useEffect1(
    () =>
      Some(
        emitter.on(
          fun
          | Request(_, _, _) => ()
          | Response(i, expr) =>
            if (Some(i) == React.Ref.current(id)) {
              setRedux(_ => Reduced(React.Ref.current(reqID), expr));
            },
        ),
      ),
    [||],
  );

  switch (redux) {
  | Unreduced(expr, subst) =>
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
  | Reduced(_, expr) => <Expr prec=0 value=expr />
  | _ => <> {string("...")} </>
  };
  // <div className onMouseOver onMouseLeave onClick> {string("(...)")} </div>;
};