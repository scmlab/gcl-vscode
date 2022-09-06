module Client = LanguageServerMule.Client.LSP
module Error = Connection__Error
module Probe = Connection__Probe

open Belt

module Method = {
  type t = 
    | GithubPrebuilt(string, LanguageServerMule.Source.GitHub.Release.t, NodeJs.ChildProcess.t)
      // the binary path, github release, childprocess 
    | Command(string, NodeJs.ChildProcess.t)
      // the binary path, childprocess
    | TCP(int, string) 
      //connection port, host
}




module type Module = {
  // lifecycle
  let start: (
    string,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
  ) => Promise.t<result<Method.t, Error.t>>
  let stop: unit => Promise.t<unit>
  // input / output / event
  let sendRequest: (
    string,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
    Request.t,
  ) => Promise.t<result<Response.t, Error.t>>
  let onNotification: (result<Response.t, Error.t> => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t

  let methodToString: Method.t => string
  let isTurnedOn: ref<bool>
}

module Module: Module = {
  // internal singleton
  type state =
    | Disconnected
    | Connecting(
        // here queues all the requests & callbacks accumulated before the connection is established
        array<(Request.t, result<Response.t, Error.t> => unit)>,
        Promise.t<result<Method.t, Error.t>>,
      )
    | Connected(LanguageServerMule.Client.LSP.t, Method.t, array<VSCode.Disposable.t>)
      //(about the 2nd. argument) Because even that the extension user already has the prebuilt binary, 
      // we're still establishing the connection via TCP, through the downloaded binary.
      // Therefore we need to explicitly record this information, for it being inconsistent with 'getMethod(client)'.

  let singleton: ref<state> = ref(Disconnected)
  let isTurnedOn = ref(true)
   //Become true by activate the command Guabao.stop, 
   //and false again by activate the command Guabao.start
  let errorChan: Chan.t<Error.t> = Chan.make()
  let notificationChan: Chan.t<result<Response.t, Error.t>> = Chan.make()
  // let downloadChan: Chan.t<result<Response.t, Error.t>> = Chan.make()

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

  let start = (globalStoragePath, onDownload) =>
    switch singleton.contents {
    | Connected(_, method, _subsriptions) =>
      Promise.resolved(Ok(method))
    | Connecting(_, promise) => promise
    | Disconnected =>
      let (promise, resolve) = Promise.pending()
      singleton := Connecting([], promise)

      Probe.probe(globalStoragePath, onDownload)
      ->Promise.mapError(error => Error.CannotAcquireHandle(error))
      ->Promise.flatMapOk(probedMethod => {
        
        //realMethod : LanguageServerMule.Method.t
        //shownMethod : Method.t
        let (realMethod, shownMethod) = switch probedMethod{
          | ViaCommand(commandPath,_,_,source) => {

            let (port,host) = (3000, "localhost")
            //Launch the backend server first then make the connection through TCP (create realMethod for LanguageServerMule.Client.LSP.make)
            open NodeJs.ChildProcess
            let backendChildProcess = spawn(commandPath,["-d"]) //Currently use development mode, should discard "-d" in later updates.
            // note: If in any case you need to manually turn off the backend server,
            //  use "lsof -i TCP:[portNumber]" in commandline to see the PID of the backend server, then use "kill [PID]" command.
            Js.log("[ connection ] spawned the backend server")
            spawnSync("sleep",["0.5"],spawnSyncOptions(()))->ignore //wait 0.5 seconds to ensure that the backend is launched

            let shownMethod = switch source{
              | FromGitHub(_,release,_) =>{
                //update the setting variable "guabao.gclPath"
                VSCode.Workspace.getConfiguration(None,None)
                ->VSCode.WorkspaceConfiguration.updateGlobalSettings("guabao.gclPath", commandPath, None)
                ->ignore
                
                Method.GithubPrebuilt(commandPath, release, backendChildProcess)
                }
              | _ => Method.Command(commandPath, backendChildProcess)
            }
            ( LanguageServerMule.Method.ViaTCP(port, host, LanguageServerMule.Method.FromTCP(port,host))
            , shownMethod
            )
          }

          | ViaTCP(port, host, _) => (probedMethod, Method.TCP(port,host))
        }

        LanguageServerMule.Client.LSP.make(
          "guabao",
          "Guabao Language Server",
          realMethod,
          Js.Json.null  // command line options
        )->Promise.mapError(e => Error.ConnectionError(e))
        ->Promise.mapOk(client => (client, shownMethod))
      })
      ->Promise.map(result =>
        switch result {
        | Error(error) =>
          singleton := Disconnected
          getPendingRequests()->Array.forEach(((_req, callback)) => callback(Error(error)))
          resolve(Error(error))
          Error(error)
        | Ok((client,shownMethod)) =>
          let subsriptions = []
          singleton := Connected(client, shownMethod, subsriptions)
          getPendingRequests()->Array.forEach(((request, callback)) => {
            client
            ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
            ->Promise.mapError(e => Error.ConnectionError(e))
            ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
            ->Promise.get(callback)
          })
          resolve(Ok(shownMethod))
          // pipe error and notifications(backend to frontend)
          client
          ->Client.onNotification(json => {
            notificationChan->Chan.emit(decodeResponse(json))
          })
          ->Js.Array.push(subsriptions)
          ->ignore
          client
          ->Client.onError(error => errorChan->Chan.emit(Error.ConnectionError(error)))
          ->Js.Array.push(subsriptions)
          ->ignore

          Ok(shownMethod)
        }
      )
    }

  let rec stop = () =>
    switch singleton.contents {
    | Connected(client, method, subscriptions) =>
      // update the status
      singleton := Disconnected
      Js.log("[ connection ] stop")
      subscriptions->Array.forEach(VSCode.Disposable.dispose)
      LanguageServerMule.Client.LSP.destroy(client)
      ->Promise.map(_=>{
        switch method{
        | GithubPrebuilt(_,_, cp) => cp->NodeJs.ChildProcess.kill("SIGTERM")->ignore
        | Command(_, cp) => cp->NodeJs.ChildProcess.kill("SIGTERM")->ignore
        | _ => () 
        }
        open NodeJs.ChildProcess
        spawnSync("sleep",["0.5"],spawnSyncOptions(()))->ignore
      })
    | Connecting(_, promise) => promise->Promise.flatMap(_ => stop())
    | Disconnected => Promise.resolved()
    }

  let rec sendRequest = (globalStoragePath, onDownload, request) =>
    switch singleton.contents {
    | Disconnected =>
      start(globalStoragePath, onDownload)
      ->Promise.flatMapOk(_ =>
          sendRequest(globalStoragePath, onDownload, request)
        )
    | Connecting(queue, _) =>
      let (promise, resolve) = Promise.pending()
      Js.Array.push((request, resolve), queue)->ignore
      promise
    | Connected(client, _, _) =>
      client
      ->LanguageServerMule.Client.LSP.sendRequest(Request.encode(request))
      ->Promise.mapError(e => Error.ConnectionError(e))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

  let onNotification = handler => notificationChan->Chan.on(handler)->VSCode.Disposable.make
  let onError = handler => errorChan->Chan.on(handler)->VSCode.Disposable.make

  let methodToString = method => {
    switch method {
      | Method.GithubPrebuilt(_,release,_) => "Prebuilt " ++ release.tagName
      | Command(_,_) => "ViaStdIO"
      | TCP(_,_) => "TCP"
    }
  }
}

include Module
