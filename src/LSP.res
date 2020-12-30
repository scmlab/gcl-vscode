open VSCode

module Message = {
  type t = {jsonrpc: string}
}

module ErrorAction = {
  type t = Continue | Shutdown
  type raw = int
  let toEnum = x =>
    switch x {
    | Continue => 1
    | Shutdown => 2
    }
}

module CloseAction = {
  type t = DoNotRestart | Restart
  type raw = int
  let toEnum = x =>
    switch x {
    | DoNotRestart => 1
    | Restart => 2
    }
}

module ErrorHandler: {
  type t
  let make: (
    ~error: (Js.Exn.t, option<Message.t>, option<int>) => ErrorAction.t,
    ~closed: unit => CloseAction.t,
  ) => t
  let makeDefault: string => int => t
} = {
  type t = {
    error: (Js.Exn.t, option<Message.t>, option<int>) => ErrorAction.raw,
    closed: unit => CloseAction.raw,
  }

  let make = (~error, ~closed) => {
    let error = (a, b, c) => error(a, b, c)->ErrorAction.toEnum
    let closed = () => closed()->CloseAction.toEnum
    {
      error: error,
      closed: closed,
    }
  }

  // https://github.com/microsoft/vscode-languageserver-node/blob/20681d7632bb129def0c751be73cf76bd01f2f3a/client/src/common/client.ts#L275
  let makeDefault = (name, maxRestartCount) => {
    let restarts = []
    make(
      ~error=(_, _, count) =>
        switch count {
        | Some(count) =>
          if count <= 3 {
            Continue
          } else {
            Shutdown
          }
        | None => Shutdown
        },
      ~closed=() => {
        Js.Array.push(Js.Date.now(), restarts)->ignore
        let length = Js.Array.length(restarts)
        if length <= maxRestartCount {
          Restart
        } else {
          open Belt
          let diff =
            restarts[length - 1]->Option.flatMap(latest =>
              restarts[0]->Option.map(first => latest -. first)
            )
          switch diff {
          | Some(diff) => if int_of_float(diff) <= 3 * 60 * 1000 {
              let max = string_of_int(maxRestartCount + 1)
            	Window.showErrorMessage("The " ++ name ++ "server crashed " ++ max ++ " times in the last 3 minutes. The server will not be restarted.", [])->ignore
              DoNotRestart
          } else {
            Js.Array.shift(restarts)->ignore 
            Restart 
          }
          | None => Restart 
          }
        }
      },
    )
  }
}

// Options to control the language client
module LanguageClientOptions = {
  type t
  let make: (DocumentSelector.t, FileSystemWatcher.t, ErrorHandler.t) => t = %raw(
    "function (documentSelector, synchronize, errorHandler) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize,
        errorHandler: errorHandler
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
  @bs.send external dispose: t => unit = "dispose"
}

module WindowExt = {
  @bs.module("vscode") @bs.scope("window")
  external createWebviewTextEditorInset: (VSCode.TextEditor.t, int, int) => WebviewEditorInset.t =
    "createWebviewTextEditorInset"
  @bs.module("vscode") @bs.scope("window")
  external createWebviewTextEditorInsetWithOptions: (
    VSCode.TextEditor.t,
    int,
    int,
    VSCode.WebviewOptions.t,
  ) => WebviewEditorInset.t = "createWebviewTextEditorInset"
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



module type Client = {
  type t
  let start: unit => LanguageClient.t
  let stop: unit => Promise.t<unit>
  let on: (Response.t => unit) => unit
  let send: Request.t => Promise.t<Response.t>
}
module Client: Client = {
  type t = {
    mutable client: LanguageClient.t,
    mutable subscription: VSCode.Disposable.t,
  }
  let make = () => {
    let serverOptions = ServerOptions.makeCommand("gcl")

    let clientOptions = {
      // let makePattern = [%raw "function(filename) { return fileName }"];
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [
        StringOr.others({
          open DocumentFilter
          {
            scheme: Some("file"),
            pattern: None,
            // Some(makePattern(fileName)),
            language: Some("guacamole"),
          }
        }),
      ]

      // Notify the server about file changes to '.clientrc files contained in the workspace
      let synchronize: FileSystemWatcher.t = Workspace.createFileSystemWatcher(
        %raw("'**/.clientrc'"),
        ~ignoreCreateEvents=false,
        ~ignoreChangeEvents=false,
        ~ignoreDeleteEvents=false,
      )

      LanguageClientOptions.make(
        documentSelector,
        synchronize,
        ErrorHandler.makeDefault("Guacamole", 3),
      )
    }
    // Create the language client
    LanguageClient.make(
      "guacamoleLanguageServer",
      "Guacamole Language Server",
      serverOptions,
      clientOptions,
    )
  }

  let handle: ref<option<t>> = ref(None)

  // make and start the LSP client
  let start = () =>
    switch handle.contents {
    | None =>
      let client = make()
      let subscription = client->LanguageClient.start
      handle := Some({client: client, subscription: subscription})
      client
    | Some({client}) => client
    }
  // stop the LSP client
  let stop = () =>
    switch handle.contents {
    | None => Promise.resolved()
    | Some({client, subscription}) =>
      handle := None
      subscription->VSCode.Disposable.dispose->ignore
      client->LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
    }

  let decodeResponse = (json: Js.Json.t): Response.t =>
    switch // catching exceptions occured when decoding JSON values
    Response.decode(json) {
    | response => response
    | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
    }

  let on = handler => handle.contents->Belt.Option.forEach(({client}) => {
      client
      ->LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.getOk(() =>
        client->LanguageClient.onNotification("guacamole", json =>
          handler(decodeResponse(json))
        )
      )
    })

  let send = request => {
    let client = start()

    client->LanguageClient.onReady->Promise.Js.toResult->Promise.flatMapOk(() => {
      let value = Request.encode(request)
      client->LanguageClient.sendRequest("guacamole", value)->Promise.Js.toResult
    })->Promise.map(x =>
      switch x {
      | Ok(json) => decodeResponse(json)
      | Error(error) => Response.CannotSendRequest(Response.Error.fromJsError(error))
      }
    )
  }
}

