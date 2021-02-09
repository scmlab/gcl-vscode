open Belt
module Error = {
  type t =
    // probe
    | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
    | CannotConnectViaTCP(Js.Exn.t)
    // connection
    | ConnectionError(Js.Exn.t)
    | CannotSendRequest(Js.Exn.t)
    // decoding
    | CannotDecodeResponse(string, Js.Json.t)

  let toString = error =>
    switch error {
    | CannotConnectViaStdIO(e) =>
      let (_header, body) = AgdaModeVscode.Process.PathSearch.Error.toString(e)
      ("Cannot locate \"gcl\"", body ++ "\nPlease make sure that the executable is in the path")
    | CannotConnectViaTCP(_) => (
        "Cannot connect with the server",
        "Please enter \":main -d\" in ghci",
      )
    | ConnectionError(exn) =>
      let isECONNREFUSED =
        Js.Exn.message(exn)->Option.mapWithDefault(
          false,
          Js.String.startsWith("connect ECONNREFUSED"),
        )

      isECONNREFUSED
        ? ("LSP Connection Error", "Please enter \":main -d\" in ghci")
        : ("LSP Client Error", Js.Exn.message(exn)->Option.getWithDefault(""))
    | CannotSendRequest(exn) => (
        "PANIC: Cannot send request",
        "Please file an issue\n" ++ Js.Exn.message(exn)->Option.getWithDefault(""),
      )
    | CannotDecodeResponse(msg, json) => (
        "PANIC: Cannot decode response",
        "Please file an issue\n\n" ++ msg ++ "\n" ++ Json.stringify(json),
      )
    }
}

type method = ViaStdIO(string, string) | ViaTCP(int)

module type Module = {
  // lifecycle
  let make: bool => Promise.t<result<method, Error.t>>
  let destroy: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: (bool, Request.t) => Promise.t<result<Response.t, Error.t>>
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
          // NOTE: somehow `onNotification` gets called TWICE everytime
          // This flag is for filtering out half of the Notifications
          let flag = ref(true)
          self.client->LSP.LanguageClient.onNotification("guacamole", json => {
            if flag.contents {
              dataChan->Chan.emit(json)
              flag := false
            } else {
              flag := true
            }
          })
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

  // for internal bookkeeping
  type state =
    | Disconnected
    | Connecting(
        // pending requests & callbacks
        array<(Request.t, result<Response.t, Error.t> => unit)>,
        Promise.t<result<method, Error.t>>,
      )
    | Connected(Client.t)

  let singleton: ref<state> = ref(Disconnected)

  let getPendingRequests = () =>
    switch singleton.contents {
    | Connecting(queue, _) => queue
    | _ => []
    }

  // catches exceptions occured when decoding JSON values
  let decodeResponse = (json: Js.Json.t): result<Response.t, Error.t> =>
    switch Response.decode(json) {
    | response => Ok(response)
    | exception Json.Decode.DecodeError(msg) => Error(Error.CannotDecodeResponse(msg, json))
    }

  let make = tryTCP =>
    switch singleton.contents {
    | Connected(client) => Promise.resolved(Ok(client.method))
    | Connecting(_, promise) => promise
    | Disconnected =>
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)
      probe(tryTCP)
      ->Promise.flatMapOk(Client.make)
      ->Promise.map(result =>
        switch result {
        | Error(error) =>
          singleton := Disconnected
          getPendingRequests()->Array.forEach(((_req, callback)) => callback(Error(error)))
          resolve(Error(error))
          Error(error)
        | Ok(client) =>
          singleton := Connected(client)
          getPendingRequests()->Array.forEach(((request, callback)) => {
            client
            ->Client.sendRequest(Request.encode(request))
            ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
            ->Promise.get(callback)
          })
          resolve(Ok(client.method))
          Ok(client.method)
        }
      )
    }

  let rec destroy = () =>
    switch singleton.contents {
    | Connected(client) =>
      // update the status
      singleton := Disconnected
      Client.destroy(client)
    | Connecting(_, promise) => promise->Promise.flatMap(_ => destroy())
    | Disconnected => Promise.resolved()
    }

  let rec sendRequest = (tryTCP, request) =>
    switch singleton.contents {
    | Disconnected => make(tryTCP)->Promise.flatMapOk(_ => sendRequest(tryTCP, request))
    | Connecting(queue, _) =>
      let (promise, resolve) = Promise.pending()
      Js.Array.push((request, resolve), queue)->ignore
      promise
    | Connected(client) =>
      client
      ->Client.sendRequest(Request.encode(request))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

  let onResponse = handler => Client.onResponse(json => handler(decodeResponse(json)))

  let onError = Client.onError

  let getMethod = () =>
    switch singleton.contents {
    | Connected(client) => Some(client.method)
    | _ => None
    }
}

include Module
