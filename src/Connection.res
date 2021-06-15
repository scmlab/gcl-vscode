module Client = Connection__Client
module Error = Connection__Error

open Belt

module type Module = {
  type method = Client.method
  // lifecycle
  let start: string => Promise.t<result<Client.method, Error.t>>
  let stop: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: (string, Request.t) => Promise.t<result<Response.t, Error.t>>
  let onResponse: (result<Response.t, Error.t> => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t
}

module Module: Module = {
  type method = Client.method

  module Probe = {
    module StdIO = {
      // see if "gcl" is available
      let probe = name => {
        LanguageServerMule.Connection__Process.PathSearch.run(name, "Cannot find the executable")
        ->Promise.mapOk(path => Client.ViaStdIO(name, Js.String.trim(path)))
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
          Client.ViaTCP(port)
        })
      }
    }

    module Prebuilt = {
      // see if the prebuilt is available
      let probe = context => {
        Connection__Prebuilt.get(context)
        ->Promise.mapOk(path => Client.ViaPrebuilt(Config.version, Js.String.trim(path)))
        ->Promise.mapError(e => Error.CannotConnectViaPrebuilt(e))
      }
    }

    // see if the server is available
    // priorities: TCP => Prebuilt => StdIO
    let probe = globalStoragePath => {
      let port = 3000
      let name = "gcl"
      TCP.probe(port)
      ->Promise.flatMapError(_ => Prebuilt.probe(globalStoragePath))
      ->Promise.flatMapError(_ => StdIO.probe(name))
    }
  }

  // internal singleton
  type state =
    | Disconnected
    | Connecting(
        // pending requests & callbacks
        array<(Request.t, result<Response.t, Error.t> => unit)>,
        Promise.t<result<Client.method, Error.t>>,
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

  let start = globalStoragePath =>
    switch singleton.contents {
    | Connected(client) => Promise.resolved(Ok(Client.getMethod(client)))
    | Connecting(_, promise) => promise
    | Disconnected =>
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)
      Probe.probe(globalStoragePath)
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
          resolve(Ok(Client.getMethod(client)))
          Js.log("[ connection ] established")
          Ok(Client.getMethod(client))
        }
      )
    }

  let rec stop = () =>
    switch singleton.contents {
    | Connected(client) =>
      // update the status
      singleton := Disconnected
      Js.log("[ connection ] severed")
      Client.destroy(client)
    | Connecting(_, promise) => promise->Promise.flatMap(_ => stop())
    | Disconnected => Promise.resolved()
    }

  let rec sendRequest = (globalStoragePath, request) =>
    switch singleton.contents {
    | Disconnected =>
      start(globalStoragePath)->Promise.flatMapOk(_ => sendRequest(globalStoragePath, request))
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
}

include Module
