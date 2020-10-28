[@react.component]
let make =
    (
      ~editorType: API.editorType,
      ~onRequest: AgdaModeVscode.Event.t(View.Request.t),
      ~onResponse: AgdaModeVscode.Event.t(View.Response.t),
    ) => {
  // let (reqID, setReqID) = React.useState(() => None);
  let (header, setHeader) = React.useState(() => View.Request.Header.Loading);
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);
  let (mode, setMode) = React.useState(_ => GCL.WP1);
  let (hidden, setHidden) = React.useState(_ => false);
  let onClickLink = React.useRef(AgdaModeVscode.Event.make());
  let onSubstitute = React.useRef(AgdaModeVscode.Event.make());

  // response with Initialized on mount
  React.useEffect1(
    () => {
      onResponse.emit(View.Response.Initialized);
      None;
    },
    [||],
  );

  let onChangeMode = mode => setMode(_ => mode);

  // for receiving requests from the extension
  React.useEffect1(
    () => {
      open View.Request;
      let destructor =
        onRequest.on(req => {
          switch (req) {
          | Display(header, body) =>
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

  // send "SetMode" back to the extension when the "mode" changed
  React.useEffect1(
    () => {
      onResponse.emit(SetMode(mode));
      None;
    },
    [|mode|],
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
        <Header header editorType mode onChangeMode />
        <Body body />
      </section>
    </Link.Provider>
  </Subst.Provider>;
  // </ReqID.Provider>;
};
