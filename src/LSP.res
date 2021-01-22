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

open Belt

module type Client = {
  type t

  let onError: (Js.Exn.t => unit) => VSCode.Disposable.t
  let onData: (Js.Json.t => unit) => VSCode.Disposable.t

  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, 'a>>

  let destroy: t => Promise.t<unit>
  let make: (bool, bool) => Promise.t<option<t>>
}

module Client: Client = {
  type t = {
    mutable client: LanguageClient.t,
    queue: array<(Request.t, Response.t => unit)>,
    subscription: VSCode.Disposable.t,
  }
  // for emitting errors
  let errorChan: Chan.t<Js.Exn.t> = Chan.make()
  // for emitting data
  let dataChan: Chan.t<Js.Json.t> = Chan.make()

  let onError = callback => errorChan->Chan.on(callback)->VSCode.Disposable.make
  let onData = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

  let sendRequest = (self, data) =>
    self.client
    ->LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LanguageClient.sendRequest("guacamole", data)->Promise.Js.toResult
    })

  let destroy = self => {
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = (devMode, viaTCP) => {
    // let emittedError = ref(false)

    let serverOptions = viaTCP
      ? ServerOptions.makeWithStreamInfo(3000)
      : ServerOptions.makeWithCommand("gcl")

    let clientOptions = {
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [
        StringOr.others({
          open DocumentFilter
          {
            scheme: Some("file"),
            pattern: None,
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

      let errorHandler: ErrorHandler.t = devMode
        ? ErrorHandler.make(
            ~error=(exn, _msg, _count) => {
              errorChan->Chan.emit(exn)

              // if !emittedError.contents {
              //   destroy()->ignore
              //   errorChan->Chan.emit(exn)
              //   emittedError := true
              // }
              Shutdown
            },
            ~closed=() => {
              DoNotRestart
            },
          )
        : ErrorHandler.makeDefault("Guacamole", 3)

      LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
    }

    // Create the language client
    let languageClient = LanguageClient.make(
      "guacamoleLanguageServer",
      "Guacamole Language Server",
      serverOptions,
      clientOptions,
    )

    let self = {
      client: languageClient,
      queue: [],
      subscription: languageClient->LanguageClient.start,
    }

    self.client
    ->LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.map(result =>
      switch result {
      | Error(_) => None
      | Ok() =>
        self.client->LanguageClient.onNotification("guacamole", json => dataChan->Chan.emit(json))
        Some(self)
      }
    )
  }
}

module type Module = {
  type status = Disconnected | Connecting | Connected

  let start: bool => bool => Promise.t<bool>
  let stop: unit => Promise.t<unit>
  let isConnected: unit => bool
  let onResponse: (Response.t => unit) => VSCode.Disposable.t
  let onError: (Js.Exn.t => unit) => VSCode.Disposable.t
  let onChangeConnectionStatus: (status => unit) => VSCode.Disposable.t
  let sendRequest: Request.t => Promise.t<option<Response.t>>
}

module Module: Module = {
  // for emitting events
  type status = Disconnected | Connecting | Connected
  let statusChan: Chan.t<status> = Chan.make()
  // for internal bookkeeping
  type state = Disconnected | Connecting(array<(Request.t, option<Response.t> => unit)>, Promise.t<bool>) | Connected(Client.t)
  let singleton: ref<state> = ref(Disconnected)

  // stop the LSP client
  let stop = () =>
    switch singleton.contents {
    | Disconnected => Promise.resolved()
    | Connecting(_) => 
      // update the status
      singleton := Disconnected
      statusChan->Chan.emit(Disconnected)
      Promise.resolved()
    | Connected(client) =>
      // update the status
      singleton := Disconnected
      statusChan->Chan.emit(Disconnected)
      // destroy the client
      client->Client.destroy
    }

  let decodeResponse = (json: Js.Json.t): Response.t =>
    switch // catching exceptions occured when decoding JSON values
    Response.decode(json) {
    | response => response
    | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
    }

  let sendRequestWithClient = client => request =>
      client
      ->Client.sendRequest(Request.encode(request))
      ->Promise.map(x =>
        switch x {
        | Ok(json) => Some(decodeResponse(json))
        | Error(error) =>
          statusChan->Chan.emit(Disconnected)
          Some(Response.CannotSendRequest(Response.Error.fromJsError(error)))
        }
      )

  // make and start the LSP client
  let start = (devMode, viaTCP) =>
    switch singleton.contents {
    | Disconnected =>
      // update the status
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)
      statusChan->Chan.emit(Connecting)

      Client.make(devMode, viaTCP)->Promise.flatMap(result =>
        switch result {
        | None => 
          resolve(false)
          Promise.resolved(false)
        | Some(client) =>
          let queuedRequest = switch singleton.contents {
          | Disconnected => []
          | Connecting(queued, _) => queued
          | Connected(_) => []
          }
          // resolve the `Connecting` status
          resolve(true)
          // update the status
          singleton := Connected(client)
          statusChan->Chan.emit(Connected)
          // handle the requests queued up when connecting  
          queuedRequest->Array.map(((request, resolve)) => {
            sendRequestWithClient(client, request)->Promise.tap(resolve)
          })->Util.Promise.oneByOne->Promise.map(_ => true)
        }
      )
    | Connecting(_, promise) => promise
    | Connected(_) => Promise.resolved(true)
    }

  let isConnected = () =>
    switch singleton.contents {
    | Disconnected => false
    | Connecting(_, _) => false
    | Connected(_) => true
    }

  let onResponse = handler => Client.onData(json => handler(decodeResponse(json)))
  let onError = Client.onError
  let onChangeConnectionStatus = callback => statusChan->Chan.on(callback)->VSCode.Disposable.make

  let sendRequest = request =>
    switch singleton.contents {
    | Connected(client) => sendRequestWithClient(client, request)
    | Connecting(queue, _) =>
      let (promise, resolve) = Promise.pending()
      Js.Array.push((request, resolve), queue)->ignore
      promise
    | Disconnected => Promise.resolved(None)
    }
}

include Module
