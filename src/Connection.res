module Client = LanguageServerMule.Client.LSP
module Error = Connection__Error
module Probe = Connection__Probe

open Belt

module type Module = {
  // lifecycle
  let start: string => Promise.t<result<LanguageServerMule.Handle.t, Error.t>>
  let stop: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: (string, Request.t) => Promise.t<result<Response.t, Error.t>>
  // let onNotification: (result<Response.t, Error.t> => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t

  let methodToString: LanguageServerMule.Handle.t => string
}

module Module: Module = {

  // internal singleton
  type state =
    | Disconnected
    | Connecting(
        // here queues all the requests & callbacks accumulated before the connection is established
        array<(Request.t, result<Response.t, Error.t> => unit)>,
        Promise.t<result<LanguageServerMule.Handle.t, Error.t>>,
      )
    | Connected(LanguageServerMule.Client.LSP.t, array<VSCode.Disposable.t>)
  let singleton: ref<state> = ref(Disconnected)
  let errorChan: Chan.t<Error.t> = Chan.make()
  let notificationChan: Chan.t<result<Response.t, Error.t>> = Chan.make()

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
    | Connected(client, _subsriptions) =>
      Promise.resolved(Ok(LanguageServerMule.Client.LSP.getHandle(client)))
    | Connecting(_, promise) =>
      promise
    | Disconnected =>
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)
      Probe.probe(globalStoragePath)
      ->Promise.flatMapOk(handle => {
        LanguageServerMule.Client.LSP.make("guabao", "Guabao Language Server", handle)
        ->Promise.mapError(e => Error.ConnectionError(e))
      })
      ->Promise.map(result =>
        switch result {
        | Error(error) =>
          singleton := Disconnected
          getPendingRequests()->Array.forEach(((_req, callback)) => callback(Error(error)))
          resolve(Error(error))
          Error(error)
        | Ok(client) =>
          let subsriptions = []
          singleton := Connected(client, subsriptions)
          getPendingRequests()->Array.forEach(((request, callback)) => {
            client
            ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
            ->Promise.mapError(e => Error.ConnectionError(e))
            ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
            ->Promise.get(callback)
          })
          resolve(Ok(LanguageServerMule.Client.LSP.getHandle(client)))
          // pipe error and notifications
          client
          ->Client.onNotification(json => notificationChan->Chan.emit(decodeResponse(json)))
          ->Js.Array.push(subsriptions)
          ->ignore
          client->Client.onError(error => errorChan->Chan.emit(Error.ConnectionError(error)))
          ->Js.Array.push(subsriptions)
          ->ignore

          Ok(LanguageServerMule.Client.LSP.getHandle(client))
        }
      )
    }

  let rec stop = () =>
    switch singleton.contents {
    | Connected(client, subscriptions) =>
      // update the status
      singleton := Disconnected
      Js.log("[ connection ] severed")
      subscriptions->Array.forEach(VSCode.Disposable.dispose)
      LanguageServerMule.Client.LSP.destroy(client)
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
    | Connected(client, _) =>
      client
      ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
      ->Promise.mapError(e => Error.ConnectionError(e))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

  // let onNotification = handler => notificationChan->Chan.on(handler)->VSCode.Disposable.make
  let onError = handler => errorChan->Chan.on(handler)->VSCode.Disposable.make

  let methodToString = x =>
    switch x {
    | LanguageServerMule.Handle.ViaTCP(_, _) => "ViaTCP"
    | ViaStdIO(_) => "ViaStdIO"
    }
}

include Module
