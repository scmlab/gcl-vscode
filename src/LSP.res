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
  let makeDefault: (string, int) => t
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
          | Some(diff) =>
            if int_of_float(diff) <= 3 * 60 * 1000 {
              let max = string_of_int(maxRestartCount + 1)
              Window.showErrorMessage(
                "The " ++
                name ++
                "server crashed " ++
                max ++ " times in the last 3 minutes. The server will not be restarted.",
                [],
              )->ignore
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
  let make: (
    DocumentSelector.t,
    FileSystemWatcher.t,
    ErrorHandler.t,
  ) => t = %raw("function (documentSelector, synchronize, errorHandler) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize,
        errorHandler: errorHandler
      }
    }")
}

// Options to control the language client
module ServerOptions = {
  type t
  let makeWithCommand: string => t = %raw("function (command) {
      return { command: command }
    }")

  let makeWithStreamInfo: int => t = %raw("function (port) {
      const net = require('net');
      const socket = net.createConnection({ port: port })
      return (() => { return new Promise(resolve => resolve({
        writer: socket,
        reader: socket
      })
      )})
    }")
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
  type status = Disconnected | Connecting | Connected

  type t
  let start: bool => Promise.t<unit>
  let stop: unit => Promise.t<unit>
  let isConnected: unit => bool
  let on: (Response.t => unit) => unit
  let onChangeConnectionStatus: (status => unit) => VSCode.Disposable.t
  let send: Request.t => Promise.t<option<Response.t>>
}
module Client: Client = {
  type t = {
    mutable client: LanguageClient.t,
    queue: array<(Request.t, Response.t => unit)>,
    subscription: VSCode.Disposable.t,
  }
  // for emitting events
  type status = Disconnected | Connecting | Connected
  let statusChan: Chan.t<status> = Chan.make()
  // for internal bookkeeping
  type state = Disconnected | Connecting(t) | Connected(t)
  let singleton: ref<state> = ref(Disconnected)

  let make = viaTCP => {
    let serverOptions = viaTCP
      ? ServerOptions.makeWithStreamInfo(3000)
      : ServerOptions.makeWithCommand("gcl")
    // let serverOptions = ServerOptions.makeWithCommand("gcl")

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

      let _errorHandler: ErrorHandler.t = ErrorHandler.make(
        ~error=(exn, msg, count) => {
          Js.log4("error", exn, msg, count)
          Shutdown
        },
        ~closed=() => {
          Js.log("closed")
          DoNotRestart
        },
      )

      LanguageClientOptions.make(
        documentSelector,
        synchronize,
        // errorHandler
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

  // make and start the LSP client
  let start = (viaTCP) =>
    switch singleton.contents {
    | Disconnected =>
      let client = make(viaTCP)
      let subscription = client->LanguageClient.start
      let state = {client: client, queue: [], subscription: subscription}
      singleton := Connecting(state)
      statusChan->Chan.emit(Connecting)
      // emit `Connected` after ready
      client
      ->LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.map(result =>
        switch result {
        | Error(_) => ()
        | Ok() =>
          statusChan->Chan.emit(Connected)
          singleton := Connected(state)
        }
      )
    | Connecting(_) => Promise.resolved()
    | Connected(_) => Promise.resolved()
    }
  // stop the LSP client
  let stop = () =>
    switch singleton.contents {
    | Disconnected => Promise.resolved()
    | Connecting({client, subscription})
    | Connected({client, subscription}) =>
      singleton := Disconnected
      statusChan->Chan.emit(Disconnected)
      subscription->VSCode.Disposable.dispose->ignore
      client->LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
    }

  let isConnected =  () =>
    switch singleton.contents {
    | Disconnected => false
    | Connecting(_)
    | Connected(_) => true 
    }

  let decodeResponse = (json: Js.Json.t): Response.t =>
    switch // catching exceptions occured when decoding JSON values
    Response.decode(json) {
    | response => response
    | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
    }

  let on = handler =>
    switch singleton.contents {
    | Disconnected => ()
    | Connecting({client})
    | Connected({client}) =>
      client
      ->LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.getOk(() =>
        client->LanguageClient.onNotification("guacamole", json => handler(decodeResponse(json)))
      )
    }

  let onChangeConnectionStatus = callback => statusChan->Chan.on(callback)->VSCode.Disposable.make

  let send = request =>
    switch singleton.contents {
    | Connected({client}) =>
      client
      ->LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.flatMapOk(() => {
        let value = Request.encode(request)
        client->LanguageClient.sendRequest("guacamole", value)->Promise.Js.toResult
      })
      ->Promise.map(x =>
        switch x {
        | Ok(json) => Some(decodeResponse(json))
        | Error(error) =>
          statusChan->Chan.emit(Disconnected)
          Some(Response.CannotSendRequest(Response.Error.fromJsError(error)))
        }
      )
    | Connecting({queue}) =>
      Js.log("queued")
      let (promise, resolve) = Promise.pending()
      Js.Array.push((request, resolve), queue)->ignore
      promise->Promise.map(x => Some(x))
    | Disconnected => Promise.resolved(None)
    }
}
