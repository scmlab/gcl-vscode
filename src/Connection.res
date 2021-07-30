module Client = LanguageServerMule.Client.LSP
module Error = Connection__Error

open Belt

module type Module = {
  // lifecycle
  let start: string => Promise.t<result<LanguageServerMule.Handle.t, Error.t>>
  let stop: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: (string, Request.t) => Promise.t<result<Response.t, Error.t>>
  let onNotification: (result<Response.t, Error.t> => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t

  let methodToString: LanguageServerMule.Handle.t => string
}

module Module: Module = {
  module Probe = {
    let chooseFromReleases = (releases: array<LanguageServerMule.Source.GitHub.Release.t>): option<
      LanguageServerMule.Source.GitHub.Target.t,
    > => {
      open LanguageServerMule.Source.GitHub
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

    // see if the server is available
    // priorities: TCP => Prebuilt => StdIO
    let probe = globalStoragePath => {
      let port = 3000
      let name = "gcl"

      LanguageServerMule.Source.searchUntilSuccess([
        LanguageServerMule.Source.FromTCP(port, "localhost"),
        LanguageServerMule.Source.FromPath(name),
        LanguageServerMule.Source.FromGitHub({
          username: "scmlab",
          repository: "gcl",
          userAgent: "gcl-vscode",
          globalStoragePath: globalStoragePath,
          chooseFromReleases: chooseFromReleases,
        }),
      ])->Promise.mapError(e => {
        Js.log(
          "LanguageServerMule.Source.searchUntilSuccess " ++
          LanguageServerMule.Source.Error.toString(e),
        )
        Error.CannotAcquireHandle(e)
      })
    }
  }

  // internal singleton
  type state =
    | Disconnected
    | Connecting(
        // here queues all the requests & callbacks accumulated before the connection is established
        array<(Request.t, result<Response.t, Error.t> => unit)>,
        Promise.t<result<LanguageServerMule.Handle.t, Error.t>>,
      )
    | Connected(LanguageServerMule.Client.LSP.t)
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
    | Connected(client) =>
      Js.log2("[ connection ] already established", client)
      Promise.resolved(Ok(LanguageServerMule.Client.LSP.getHandle(client)))
    | Connecting(_, promise) =>
      Js.log("[ connection ] still connecting ...")
      promise
    | Disconnected =>
      Js.log("[ connection ] not established")
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)
      Probe.probe(globalStoragePath)
      ->Promise.flatMapOk(handle => {
        Js.log2("[ connection ] got handle", handle)
        LanguageServerMule.Client.LSP.make(
          "guabao",
          "Guabao Language Server",
          handle,
        )->Promise.tap(Js.log2("WTF"))->Promise.mapError(e => Error.LSPClientError(e))
      })
      ->Promise.map(result =>
        switch result {
        | Error(error) =>
          Js.log2("[ connection ] error when connecting", Error.toString(error))
          singleton := Disconnected
          getPendingRequests()->Array.forEach(((_req, callback)) => callback(Error(error)))
          resolve(Error(error))
          Error(error)
        | Ok(client) =>
          Js.log("[ connection ] established")
          singleton := Connected(client)
          getPendingRequests()->Array.forEach(((request, callback)) => {
            client
            ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
            ->Promise.mapError(e => Error.LSPClientError(e))
            ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
            ->Promise.get(callback)
          })
          resolve(Ok(LanguageServerMule.Client.LSP.getHandle(client)))
          Ok(LanguageServerMule.Client.LSP.getHandle(client))
        }
      )
    }

  let rec stop = () =>
    switch singleton.contents {
    | Connected(client) =>
      // update the status
      singleton := Disconnected
      Js.log("[ connection ] severed")
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
    | Connected(client) =>
      client
      ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
      ->Promise.mapError(e => Error.LSPClientError(e))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

  let onNotification = handler => notificationChan->Chan.on(handler)->VSCode.Disposable.make
  let onError = handler => errorChan->Chan.on(handler)->VSCode.Disposable.make

  let methodToString = x =>
    switch x {
    | LanguageServerMule.Handle.ViaTCP(_, _) => "ViaTCP"
    | ViaStdIO(_) => "ViaStdIO"
    }
}

include Module
