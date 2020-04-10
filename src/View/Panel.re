let vscode = Vscode.Api.acquireVsCodeApi();

[@react.component]
let make = () => {
  let (header, setHeader) = React.useState(() => "");
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);

  React.useEffect1(
    () => {
      Vscode.Api.onMessage(stringified => {
        let json = Js.Json.parseExn(stringified);
        let decoded =
          Js.String.substring(~from=0, ~to_=100, Js.Json.stringify(json));
        json
        |> View.Request.decode
        |> (
          fun
          | View.Request.Display(_header, body) => {
              // setHeader(_ => header);
              Js.log(body);
              setBody(_ => body);
            }
          | _ => ()
        );
      });
      None;
    },
    [||],
  );

  // <div className="gcl-body"> {ReasonReact.string("body")} </div>
  <section className="gcl native-key-bindings" tabIndex=(-1)>
    <div className="gcl-header"> {ReasonReact.string("header")} </div>
    <Body body />
  </section>;
};