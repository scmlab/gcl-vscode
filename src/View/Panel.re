let vscode = Vscode.Api.acquireVsCodeApi();

[@react.component]
let make = (~editorType: Sig.editorType) => {
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
      vscode->Vscode.Api.postMessage(View.Response.SetMode(mode));
      None;
    },
    [|mode|],
  );

  <section className="gcl-panel native-key-bindings" tabIndex=(-1)>
    <Header header editorType mode onChangeMode />
    <Body body />
  </section>;
};