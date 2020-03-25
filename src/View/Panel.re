let vscode = Vscode.Api.acquireVsCodeApi();

[@react.component]
let make = () => {
  let (header, setHeader) = React.useState(() => "");
  let (body, setBody) = React.useState(() => "");

  React.useEffect1(
    () => {
      Js.log("init");
      vscode->Vscode.Api.postMessage("from view");
      Vscode.Api.onMessage((msg: View.message) => {
        Js.log2(" >>> ", msg);
        switch (msg) {
        | View.Display(header, body) =>
          setHeader(_ => header);
          setBody(_ => body);
        };
      });
      None;
    },
    [||],
  );

  <section className="gcl native-key-bindings" tabIndex=(-1)>
    <div className="gcl-header"> {ReasonReact.string(header)} </div>
    <div className="gcl-body"> {ReasonReact.string(body)} </div>
  </section>;
};