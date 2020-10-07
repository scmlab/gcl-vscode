open React;

open Common;

type event =
  | Request(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
  | Response(int, GCL.Syntax.Expr.t);

let emitter: AgdaModeVscode.Event.t(event) = AgdaModeVscode.Event.make();
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
    id.current = Some(counter^);
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

  reqID.current = React.useContext(ReqID.context);

  // cache invalidaction
  switch (redux) {
  | Reduced(cachedID, _) =>
    if (cachedID != reqID.current) {
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
            if (Some(i) == id.current) {
              setRedux(_ => Reduced(reqID.current, expr));
            },
        ),
      ),
    [||],
  );

  switch (redux) {
  | Unreduced(expr, subst) =>

    let expressions = Js.Dict.values(subst)->Belt.Array.map(value => <Expr prec=0 value />);
    let variables = Js.Dict.keys(subst)->Belt.Array.map(value => <> {React.string(value)} </>);
               
    <>
      <Expr prec=0 value=expr />
      <Space />
      <div className onMouseOver onMouseLeave onClick>
        {string("[")}
        {Util.React.sepBy(string(", "), expressions)}
        {string("/")}
        {Util.React.sepBy(string(", "), variables)}
        {string("]")}
      </div>
    </>
  | Reduced(_, expr) => <Expr prec=0 value=expr />
  | _ => <> {string("...")} </>
  };
  // <div className onMouseOver onMouseLeave onClick> {string("(...)")} </div>;
};
