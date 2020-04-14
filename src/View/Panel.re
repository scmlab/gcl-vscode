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

  let onChangeMode = mode => setMode(_ => mode);

  // for receiving requests from the extension
  React.useEffect1(
    () => {
      let destructor =
        onRequest.on(
          fun
          | View.Request.Display(header, body) => {
              setHeader(_ => header);
              setBody(_ => body);
            }
          | _ => (),
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

  <Link.Provider value=onClickLink>
    <section className="gcl-panel native-key-bindings" tabIndex=(-1)>
      <Header header editorType mode onChangeMode />
      <Body body />
    </section>
  </Link.Provider>;
};