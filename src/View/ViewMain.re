open Belt;

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = Vscode.Api.acquireVsCodeApi();

// relay Vscode.Api.onMessage => onRequest;
let onRequest = Event.make();
Vscode.Api.onMessage(stringifiedJSON => {
  let request = stringifiedJSON->Js.Json.parseExn->View.Request.decode;
  onRequest.emit(request);
});

// relay onResponse => Vscode.Api.postMessage
let onResponse = Event.make();
onResponse.on(vscode->Vscode.Api.postMessage);

// mount the view at the "root" element
Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(
      <Panel editorType=Sig.VsCode onRequest onResponse />,
      element,
    )
  });