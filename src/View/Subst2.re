open React;


// for identifying substitions
type substID = int;

// each request to the backend comes with a unique ID
// for invalidating the current reduction (restore it to `Unreduced`)
type reqID = int;

let emitter: AgdaModeVscode.Event.t(Subst.event) = AgdaModeVscode.Event.make();
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
  | Unreduced(GCL.Syntax.Expr.t)
  | Reducing
  | Reduced(option(reqID), GCL.Syntax.Expr.t);

// a global counter for generating fresh IDs for <Subst>
let counter = ref(0);

[@react.component]
let make = (~expr: GCL.Syntax.Expr.t, ~makePrec, ~makePrecProps) => {
    let subst = Js.Dict.empty();

  module Prec = {
    let make = makePrec;
    let makeProps = makePrecProps;
  };
  let emitter = React.useContext(eventContext);
  let (redux, setRedux) = React.useState(_ => Unreduced(expr));
  // for storing substition ID
  let id = React.useRef(None);
  // request ID from the backend
  let reqID = React.useRef(None);

  let (hovered, setHover) = React.useState(_ => false);
  let onMouseOver = _ => setHover(_ => true);
  let onMouseLeave = _ => setHover(_ => false);

  // initiate substition request
  let onClick = _ => {
    emitter.emit(Request(counter^, expr, subst));
    // store the substition ID 
    id.current = Some(counter^);
    // bump the counter
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

  // access and store the request ID
  reqID.current = React.useContext(ReqID.context);

  // cache invalidaction:
  //  if the current substition has been reduced
  //  restore the substition if the request ID is invalid
  switch (redux) {
  | Reduced(cachedReqID, _) =>
    if (cachedReqID != reqID.current) {
      setRedux(_ => Unreduced(expr));
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
  | Unreduced(expr) =>
    switch expr {
        | Var(s, loc) =>
          <Link loc><div onClick className onMouseOver onMouseLeave> {string(GCL.Syntax.Name.toString(s))} </div></Link>
        | Const(s, loc) =>
          <Link loc> <div onClick className onMouseOver onMouseLeave>{string(GCL.Syntax.Name.toString(s))} </div> </Link>
          | _ => <> </>
    };
  | Reducing => <> {string("...")} </>
  | Reduced(_, expr) => <Prec prec=0 value=expr />
  };
};
