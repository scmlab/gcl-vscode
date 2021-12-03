open Belt

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = VSCode.Api.acquireVsCodeApi()

// relay VSCode.Api.onMessage => onRequest;
let onRequest = Chan.make()
VSCode.Api.onMessage(stringifiedJSON => {
  let request = Js.Json.parseExn(stringifiedJSON)->ViewType.Request.decode
  onRequest->Chan.emit(request)
})

// relay onResponse => VSCode.Api.postMessage
let onResponse = Chan.make()
let _ =
  onResponse->Chan.on(response =>
    vscode->VSCode.Api.postMessage(ViewType.Response.encode(response))
  )

// mount the view at the "root" element
Webapi.Dom.Document.getElementById(Webapi.Dom.document, "root")->Option.forEach(element =>
  ReactDOM.render(<Panel onRequest onResponse />, element)
)
