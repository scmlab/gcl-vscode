[@react.component]
let make =
    (~editorType: Sig.editorType, ~onResponse: Event.t(View.Response.t)) => {
  let (header, setHeader) = React.useState(() => View.Request.Header.Loading);
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);
  let (mode, setMode) = React.useState(_ => View.Response.WP1);

  let onChangeMode = mode => setMode(_ => mode);

  React.useEffect1(
    () => {
      Vscode.Api.onMessage(stringified => {
        stringified
        |> Js.Json.parseExn
        |> View.Request.decode
        |> (
          fun
          | View.Request.Display(header, body) => {
              setHeader(_ => header);
              setBody(_ => body);
            }
          | _ => ()
        )
      });
      None;
    },
    [||],
  );

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