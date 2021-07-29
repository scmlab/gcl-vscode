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
        LanguageServerMule.Source.Path.search(name)
        ->Promise.mapOk(path => Client.ViaStdIO(name, Js.String.trim(path)))
        ->Promise.mapError(e => Error.CannotConnectViaStdIO(e))
      }
    }

    module Prebuilt = {
      // see if the prebuilt is available
      let probe = context => {
        LanguageServerMule.Source.Prebuilt.get(context)
        ->Promise.mapOk(path => Client.ViaPrebuilt(Config.version, Js.String.trim(path)))
        ->Promise.mapError(e => Error.CannotConnectViaPrebuilt(e))
      }
    }

    // see if the server is available
    // priorities: TCP => Prebuilt => StdIO
    let probe = globalStoragePath => {
      let port = 3000
      let name = "gcl"
      LanguageServerMule.Source.Port.probe(port, "localhost")
      ->Promise.map(result =>
        switch result {
        | Ok() => Ok(Client.ViaTCP(port))
        | Error(exn) => Error(Error.CannotConnectViaTCP(exn))
        }
      )
      ->Promise.flatMapError(error => {
        Js.log(Error.toString(error))
        let chooseFromReleases = (
          releases: array<LanguageServerMule.Source.Prebuilt.Release.t>,
        ): option<LanguageServerMule.Source.Prebuilt.Target.t> => {
          open LanguageServerMule.Source.Prebuilt
          let getRelease = (releases: array<Release.t>) => {
            let matched = releases->Array.keep(release => release.tagName == Config.version)
            matched[0]
          }

          let toFileName = (release: Release.t, asset: Asset.t) => {
            // take the "macos" part from names like "gcl-macos.zip"
            let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
            // the file name of the language server
            release.tagName ++ "-" ++ osName
          }

          let getAsset = (release: Release.t) => {
            // expected asset name
            let os = Node_process.process["platform"]
            let expectedName = switch os {
            | "darwin" => Some("gcl-macos.zip")
            | "linux" => Some("gcl-ubuntu.zip")
            | "win32" => Some("gcl-windows.zip")
            | _others => None
            }

            // find the corresponding asset
            expectedName
            ->Option.flatMap(name => {
              let matched = release.assets->Array.keep(asset => asset.name == name)
              matched[0]
            })
            ->Option.map(asset => {
              Target.srcUrl: asset.url,
              fileName: toFileName(release, asset),
            })
          }

          let result = getRelease(releases)->Option.flatMap(getAsset)
          result 
        }

        Prebuilt.probe({
          username: "scmlab",
          repository: "gcl",
          userAgent: "gcl-vscode",
          globalStoragePath: globalStoragePath,
          chooseFromReleases: chooseFromReleases,
        })
      })
      ->Promise.flatMapError(error => {
        Js.log(Error.toString(error))
        StdIO.probe(name)
      })
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
