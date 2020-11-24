open Belt

let isGCL = Js.Re.test_(%re("/\\.gcl$/i"))

let handleResponse = response =>
  switch response {
  | Response.Res(filePath, kinds) =>
    Registry.get(filePath)->Option.mapWithDefault(Promise.resolved(), state =>
      kinds->Array.map(State.handleResponseKind(state))->Util.Promise.oneByOne->Promise.map(_ => ())
    )
  | CannotSendRequest(message) =>
    Js.Console.error("Client Internal Error\nCannot send request to the server\n" ++ message)
    Promise.resolved()
  // state->State.display(
  //   Error("Client Internal Error"),
  //   Plain("Cannot send request to the server\n" ++ message),
  // )
  | CannotDecodeRequest(message) =>
    Js.Console.error("Server Internal Error\nCannot decode request from the client\n" ++ message)
    Promise.resolved()
  // state->State.display(
  //   Error("Server Internal Error"),
  //   Plain("Cannot decode request from the client\n" ++ message),
  // )
  | CannotDecodeResponse(message, json) =>
    Js.Console.error2(
      "Client Internal Error\nCannot decode response from the server\n" ++ message,
      json,
    )
    Promise.resolved()
  // state->State.display(
  //   Error("Client Internal Error"),
  //   Plain("Cannot decode response from the server\n" ++ message),
  // )
  }

module Client: {
  type t
  let start: unit => unit
  let stop: unit => Promise.t<unit>
  let onNotification: (Response.t => unit) => unit
  let sendRequest: Request.t => Promise.t<Response.t>
} = {
  type t = LSP.LanguageClient.t
  let make = () => {
    open LSP
    open VSCode
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
      LanguageClientOptions.make(documentSelector, synchronize)
    }
    // Create the language client
    LanguageClient.make(
      "guacamoleLanguageServer",
      "Guacamole Language Server",
      serverOptions,
      clientOptions,
    )
  }
  let client = ref(make())
  let subscription = ref(None)

  // start the LSP client
  let start = () =>
    if subscription.contents->Option.isNone {
      subscription.contents = Some(client.contents->LSP.LanguageClient.start)
    }
  // stop the LSP client
  let stop = () =>
    switch subscription.contents {
    | None => Promise.resolved()
    | Some(subscription') => client.contents->LSP.LanguageClient.stop->Promise.tap(_ => {
        subscription.contents = None
        subscription'->VSCode.Disposable.dispose
      })
    }

  let decodeResponse = (json: Js.Json.t): Response.t =>
    switch // catching exceptions occured when decoding JSON values
    Response.decode(json) {
    | response => response
    | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
    }

  let onNotification = handler =>
    client.contents
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.getOk(() =>
      client.contents->LSP.LanguageClient.onNotification("guacamole", json =>
        handler(decodeResponse(json))
      )
    )

  let sendRequest = request => {
    start()

    client.contents->LSP.LanguageClient.onReady->Promise.Js.toResult->Promise.flatMapOk(() => {
      let value = Request.encode(request)
      client.contents->LSP.LanguageClient.sendRequest("guacamole", value)->Promise.Js.toResult
    })->Promise.map(x =>
      switch x {
      | Ok(json) => decodeResponse(json)
      | Error(error) => Response.CannotSendRequest(Response.Error.fromJsError(error))
      }
    )
  }
}

module Handler = {
  let onSelect = event => {
    let selections = event->VSCode.TextEditorSelectionChangeEvent.selections
    let editor = event->VSCode.TextEditorSelectionChangeEvent.textEditor
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName

    Registry.get(filePath)->Option.forEach(state =>
      // TODO, there may be multiple selections at once
      selections[0]->Option.forEach(selection => {
        let start = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.start(selection))
        let end_ = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.end_(selection))
        ()

        Client.sendRequest(Req(state.filePath, Inspect(start, end_)))
        ->Promise.flatMap(handleResponse)
        ->ignore
      })
    )
  }

  let onActivateExtension = callback => {
    // number of visible GCL file in the workplace
    let visibleCount =
      VSCode.Window.visibleTextEditors
      ->Array.keep(editor =>
        isGCL(editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName)
      )
      ->Array.length
    // should activate the view when there's a visible GCL file
    let shouldAcitvateView = visibleCount > 0 && !View.isActivated()

    if shouldAcitvateView {
      callback()
    }
  }

  let onDeactivateExtension = callback => {
    // number of GCL States in the Registry
    let openedCount = Registry.size()
    // should deacitvate the view when all GCL States have been destroyed
    let shouldDeacitvateView = openedCount === 0 && View.isActivated()

    if shouldDeacitvateView {
      callback()
    }
  }

  let onOpenEditor = (context, editor) => {
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    // filter out ".gcl.git" files
    if isGCL(filePath) {
      let state = switch Registry.get(filePath) {
      | None =>
        // state initialization
        let state = State.make(editor)
        Registry.add(filePath, state)
        state
      | Some(state) =>
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        state.document = editor->VSCode.TextEditor.document
        state.filePath = state.document->VSCode.TextDocument.fileName
        state
      }

      View.wire(state)
      Client.sendRequest(Req(state.filePath, Load))->Promise.flatMap(handleResponse)->ignore

      onActivateExtension(() => {
        View.activate(context->VSCode.ExtensionContext.extensionPath)
        Registry.get(filePath)->Option.forEach(View.wire)
        Client.start()
      })
    }
  }

  let onCloseEditor = doc => {
    let filePath = VSCode.TextDocument.fileName(doc)
    if isGCL(filePath) {
      Registry.destroy(filePath)
      onDeactivateExtension(() => {
        View.deactivate()
        Client.stop()->ignore
      })
    }
  }

  let onNotification = response => handleResponse(response)->ignore
}

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x => x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore

  // on open
  VSCode.Window.activeTextEditor->Option.forEach(Handler.onOpenEditor(context))
  VSCode.Window.onDidChangeActiveTextEditor(next =>
    next->Option.forEach(Handler.onOpenEditor(context))
  )->subscribe

  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onCloseEditor)->subscribe

  // on change selection
  VSCode.Window.onDidChangeTextEditorSelection(Handler.onSelect)->subscribe
  // on notification from the server
  Client.onNotification(Handler.onNotification)
}

let deactivate = () => ()
