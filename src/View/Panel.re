let vscode = Vscode.Api.acquireVsCodeApi();

[@react.component]
let make = () => {
  open View.Request;
  let (header, setHeader) = React.useState(() => "");
  let (body, setBody) = React.useState(() => Nothing);

  React.useEffect1(
    () => {
      Vscode.Api.onMessage(Js.log);
      // Vscode.Api.onMessage(
      //   fun
      //   | Display(_header, body) => {
      //       // setHeader(_ => header);
      //       setBody(_ => body);
      //     }
      //   | _ => (),
      // );
      None;
    },
    [||],
  );

  <section className="gcl native-key-bindings" tabIndex=(-1)>
    <div className="gcl-header"> {ReasonReact.string("header")} </div>
    <div className="gcl-body"> {ReasonReact.string("body")} </div>
  </section>;
};