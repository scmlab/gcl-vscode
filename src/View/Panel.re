[@react.component]
let make =
    (
      ~onRequest: Chan.t(ViewType.Request.t),
      ~onResponse: Chan.t(ViewType.Response.t),
    ) => {
  // let (reqID, setReqID) = React.useState(() => None);
  let (header, setHeader) =
    React.useState(() => ViewType.Request.Header.Loading);
  let (body, setBody) = React.useState(() => ViewType.Request.Body.Nothing);
  let (hidden, setHidden) = React.useState(_ => false);
  let onClickLink = React.useRef(Chan.make());
  let onSubstitute = React.useRef(Chan.make());

  // response with Initialized on mount
  React.useEffect1(
    () => {
      onResponse.emit(ViewType.Response.Initialized);
      None;
    },
    [||],
  );

  // for receiving requests from the extension
  React.useEffect1(
    () => {
      open ViewType.Request;
      let destructor =
        onRequest.on(req => {
          switch (req) {
          | ViewType.Request.Display(header, body) =>
            setHeader(_ => header);
            setBody(_ => body);
          | Substitute(i, expr) =>
            onSubstitute.current.emit(Subst.Response(i, expr))
          | Hide => setHidden(_ => true)
          | Show => setHidden(_ => false)
          }
        });
      Some(destructor);
    },
    [||],
  );

  // relay <Link> events to "onResponse"
  React.useEffect1(
    () => Some(onClickLink.current.on(ev => {onResponse.emit(Link(ev))})),
    [||],
  );

  // relay <Subst> substitution to "onResponse"
  React.useEffect1(
    () =>
      Some(
        onSubstitute.current.on(
          fun
          | Subst.Request(i, expr, subst) => {
              onResponse.emit(Substitute(i, expr, subst));
            }
          | Response(_, _) => (),
        ),
      ),
    [||],
  );

  let className = "gcl-panel native-key-bindings" ++ (hidden ? " hidden" : "");

  // <ReqID.Provider value=reqID>
  <Subst.Provider value={onSubstitute.current}>
    <Link.Provider value={onClickLink.current}>
      <section className tabIndex=(-1)>
        <Header header />
        <Body body />
      </section>
    </Link.Provider>
  </Subst.Provider>;
  // </ReqID.Provider>;
};
