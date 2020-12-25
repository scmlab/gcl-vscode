open VSCode

// Options to control the language client
module LanguageClientOptions = {
  type t
  let make: (DocumentSelector.t, FileSystemWatcher.t) => t = %raw(
    "function (documentSelector, synchronize) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize
      }
    }"
  )
}

// Options to control the language client
module ServerOptions = {
  type t
  let makeCommand: string => t = %raw(
    "function (command) {
      return { command: command }
    }"
  )
}


module WebviewEditorInset = {
  type t
  // properties
  @bs.get external editor: t => VSCode.TextEditor.t = "editor"
  @bs.get external line: t => int = "line"
  @bs.get external height: t => int = "height"
  @bs.get external webview: t => VSCode.Webview.t = "webview"
  @bs.get external onDidDispose: t => VSCode.Event.t<unit> = "onDidDispose"
  // methods
  @bs.send external dispose: (t) => unit = "dispose"
}


module WindowExt = {
  @bs.module("vscode") @bs.scope("window")
  external createWebviewTextEditorInset: ( VSCode.TextEditor.t,  int, int) => WebviewEditorInset.t = "createWebviewTextEditorInset"
  @bs.module("vscode") @bs.scope("window")
  external createWebviewTextEditorInsetWithOptions: ( VSCode.TextEditor.t,  int, int, VSCode.WebviewOptions.t) => WebviewEditorInset.t = "createWebviewTextEditorInset"
}

module LanguageClient = {
  type t
  // constructor
  @bs.module("vscode-languageclient") @bs.new
  external make: (string, string, ServerOptions.t, LanguageClientOptions.t) => t = "LanguageClient"
  // methods
  @bs.send external start: t => VSCode.Disposable.t = "start"
  @bs.send external stop: t => Promise.Js.t<unit, _> = "stop"
  @bs.send external onReady: t => Promise.Js.t<unit, _> = "onReady"
  @bs.send
  external onNotification: (t, string, 'a => unit) => unit = "onNotification"
  // [@bs.send]
  // external onNotification: (t, string, 'a => unit) => Disposable.t =
  //   "onNotification";
  @bs.send
  external sendNotification: (t, string, 'a) => unit = "sendNotification"
  @bs.send
  external sendRequest: (t, string, Js.Json.t) => Promise.Js.t<'result, _> = "sendRequest"
}
