module Error = {
  type t =
    // probe
    | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
    | CannotConnectViaTCP(Js.Exn.t)
    // connection
    | ConnectionError(Js.Exn.t)
    | NotConnectedYet
    | CannotSendRequest(Js.Exn.t)
    // decoding
    | CannotDecodeResponse(string, Js.Json.t)
}

type method = ViaStdIO(string, string) | ViaTCP(int)

module type Module = {
  // lifecycle
  let make: bool => Promise.t<result<unit, Error.t>>
  let destroy: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: Request.t => Promise.t<result<Response.t, Error.t>>
  let onResponse: (result<Response.t, Error.t> => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t
  // properties
  let getMethod: unit => option<method>
}

module Module: Module = {
  module StdIO = {
    // see if "gcl" is available
    let probe = name => {
      AgdaModeVscode.Process.PathSearch.run(name)
      ->Promise.mapOk(path => ViaStdIO(name, Js.String.trim(path)))
      ->Promise.mapError(e => Error.CannotConnectViaStdIO(e))
    }
  }

  module TCP = {
    module Socket = {
      type t
      // methods
      @bs.send external destroy: t => t = "destroy"
      // events
      @bs.send
      external on: (t, @bs.string [#error(Js.Exn.t => unit) | #timeout(unit => unit)]) => t = "on"
    }

    @bs.module("net")
    external connect: (int, unit => unit) => Socket.t = "connect"

    // see if the TCP port is available
    let probe = port => {
      let (promise, resolve) = Promise.pending()
      // connect and resolve `Ok()`` on success
      let socket = connect(port, () => resolve(Ok()))
      // resolve `Error(CannotConnect(Js.Exn.t))` on error
      socket->Socket.on(#error(exn => resolve(Error(Error.CannotConnectViaTCP(exn)))))->ignore
      // destroy the connection afterwards
      promise->Promise.mapOk(() => {
        Socket.destroy(socket)->ignore
        ViaTCP(port)
      })
    }
  }

  module Client = {
    open VSCode

    type t = {
      client: LSP.LanguageClient.t,
      queue: array<(Request.t, Response.t => unit)>,
      subscription: VSCode.Disposable.t,
      method: method,
    }

    // for emitting errors
    let errorChan: Chan.t<Js.Exn.t> = Chan.make()
    // for emitting data
    let dataChan: Chan.t<Js.Json.t> = Chan.make()

    let onError = callback =>
      errorChan->Chan.on(e => callback(Error.ConnectionError(e)))->VSCode.Disposable.make
    let onResponse = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

    let sendRequest = (self, data) =>
      self.client
      ->LSP.LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.flatMapOk(() => {
        self.client->LSP.LanguageClient.sendRequest("guacamole", data)->Promise.Js.toResult
      })
      ->Promise.mapError(exn => Error.CannotSendRequest(exn))

    let destroy = self => {
      self.subscription->VSCode.Disposable.dispose->ignore
      self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
    }

    let make = method => {
      // let emittedError = ref(false)

      let serverOptions = switch method {
      | ViaTCP(port) => LSP.ServerOptions.makeWithStreamInfo(port)
      | ViaStdIO(name, _path) => LSP.ServerOptions.makeWithCommand(name)
      }

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

        let errorHandler: LSP.ErrorHandler.t = LSP.ErrorHandler.make(
          ~error=(exn, _msg, _count) => {
            errorChan->Chan.emit(exn)
            Shutdown
          },
          ~closed=() => {
            DoNotRestart
          },
        )
        LSP.LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
      }

      // Create the language client
      let languageClient = LSP.LanguageClient.make(
        "guacamoleLanguageServer",
        "Guacamole Language Server",
        serverOptions,
        clientOptions,
      )

      let self = {
        client: languageClient,
        queue: [],
        subscription: languageClient->LSP.LanguageClient.start,
        method: method,
      }

      // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
      Promise.race(list{
        self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
        errorChan->Chan.once->Promise.map(err => Error(err)),
      })
      ->Promise.map(result =>
        switch result {
        | Error(error) => Error(error)
        | Ok() =>
          self.client->LSP.LanguageClient.onNotification("guacamole", json =>
            dataChan->Chan.emit(json)
          )
          Ok(self)
        }
      )
      ->Promise.mapError(e => Error.ConnectionError(e))
    }
  }

  // see if the server is available
  let probe = tryTCP => {
    let port = 3000
    let name = "gcl"
    if tryTCP {
      TCP.probe(port)->Promise.flatMapError(_ => StdIO.probe(name))
    } else {
      StdIO.probe(name)
    }
  }

  let singleton = ref(None)

  let make = tryTCP =>
    switch singleton.contents {
    | Some(_client) => Promise.resolved(Ok())
    | None =>
      probe(tryTCP)
      ->Promise.flatMapOk(Client.make)
      ->Promise.mapOk(client => singleton := Some(client))
    }

  let destroy = () =>
    switch singleton.contents {
    | Some(client) => Client.destroy(client)
    | None => Promise.resolved()
    }

  // catches exceptions occured when decoding JSON values
  let decodeResponse = (json: Js.Json.t): result<Response.t, Error.t> =>
    switch Response.decode(json) {
    | response => Ok(response)
    | exception Json.Decode.DecodeError(msg) => Error(Error.CannotDecodeResponse(msg, json))
    }

  let sendRequest = request =>
    switch singleton.contents {
    | None => Promise.resolved(Error(Error.NotConnectedYet))
    | Some(client) =>
      client
      ->Client.sendRequest(Request.encode(request))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

  let onResponse = handler => Client.onResponse(json => handler(decodeResponse(json)))

  let onError = Client.onError

  let getMethod = () =>
    switch singleton.contents {
    | Some(client) => Some(client.method)
    | None => None
    }
}

include Module
