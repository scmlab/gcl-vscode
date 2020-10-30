open Belt;

open AgdaModeVscode.VSCode;

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = Api.acquireVsCodeApi();

// relay VSCode.Api.onMessage => onRequest;
let onRequest = AgdaModeVscode.Event.make();
Api.onMessage(stringifiedJSON => {
  let request = Js.Json.parseExn(stringifiedJSON) -> View.Request.decode;
  onRequest.emit(request);
});

// relay onResponse => VSCode.Api.postMessage
let onResponse = AgdaModeVscode.Event.make();
onResponse.on(response => {
  vscode->Api.postMessage(View.Response.encode(response))
});

// mount the view at the "root" element
Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(
      <Panel onRequest onResponse />,
      element,
    )
  });
