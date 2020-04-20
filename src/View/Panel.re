[@react.component]
let make =
    (
      ~editorType: Sig.editorType,
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
    ) => {
  let (header, setHeader) = React.useState(() => View.Request.Header.Loading);
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);
  let (mode, setMode) = React.useState(_ => View.Response.WP1);
  let (hidden, setHidden) = React.useState(_ => false);

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
        onRequest.on(
          fun
          | Display(header, body) => {
              setHeader(_ => header);
              setBody(_ => body);
            }
          | Hide => setHidden(_ => true)
          | Show => setHidden(_ => false),
        );
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

  // event relay
  let onClickLink = Event.make();
  let _ = onClickLink.on(ev => onResponse.emit(Link(ev)));

  let className = "gcl-panel native-key-bindings" ++ (hidden ? " hidden" : "");

  <Link.Provider value=onClickLink>
    <section className tabIndex=(-1)>
      <Header header editorType mode onChangeMode />
      <Body body />
    </section>
  </Link.Provider>;
};