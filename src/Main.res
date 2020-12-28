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
  | CannotDecodeRequest(message) =>
    Js.Console.error("Server Internal Error\nCannot decode request from the client\n" ++ message)
    Promise.resolved()
  | CannotDecodeResponse(message, json) =>
    Js.Console.error2(
      "Client Internal Error\nCannot decode response from the server\n" ++ message,
      json,
    )
    Promise.resolved()
  }

module type Client = {
  type t
  let start: unit => LSP.LanguageClient.t
  let stop: unit => Promise.t<unit>
  let on: (Response.t => unit) => unit
  let send: Request.t => Promise.t<Response.t>
}
module Client: Client = {
  type t = {
    mutable client: LSP.LanguageClient.t,
    mutable subscription: VSCode.Disposable.t,
  }
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

  let handle: ref<option<t>> = ref(None)

  // make and start the LSP client
  let start = () =>
    switch handle.contents {
    | None =>
      let client = make()
      let subscription = client->LSP.LanguageClient.start
      handle := Some({client: client, subscription: subscription})
      client
    | Some({client}) => client
    }
  // stop the LSP client
  let stop = () =>
    switch handle.contents {
    | None => Promise.resolved()
    | Some({client, subscription}) =>
      handle := None
      subscription->VSCode.Disposable.dispose->ignore
      client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
    }

  let decodeResponse = (json: Js.Json.t): Response.t =>
    switch // catching exceptions occured when decoding JSON values
    Response.decode(json) {
    | response => response
    | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
    }

  let on = handler =>
    handle.contents->Option.forEach(({client}) => {
      client
      ->LSP.LanguageClient.onReady
      ->Promise.Js.toResult
      ->Promise.getOk(() =>
        client->LSP.LanguageClient.onNotification("guacamole", json =>
          handler(decodeResponse(json))
        )
      )
    })

  let send = request => {
    let client = start()

    client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      let value = Request.encode(request)
      client->LSP.LanguageClient.sendRequest("guacamole", value)->Promise.Js.toResult
    })
    ->Promise.map(x =>
      switch x {
      | Ok(json) => decodeResponse(json)
      | Error(error) => Response.CannotSendRequest(Response.Error.fromJsError(error))
      }
    )
  }
}

let registerInset = () => {
  // let extensionPath = context->VSCode.ExtensionContext.extensionPath
  // let distPath = Node.Path.join2(extensionPath, "dist")
  // let options = {
  //         VSCode.WebviewOptions.enableCommandUris:  Some(true),
  //         enableScripts: Some(true),
  //         // And restrict the webview to only loading content from our extension's `dist` directory.
  //         localResourceRoots: Some([VSCode.Uri.file(distPath)]),
  //         portMapping: None
  //       }
  // let inset = LSP.WindowExt.createWebviewTextEditorInsetWithOptions(editor, 2, 5, options)
  // let webview = inset->LSP.WebviewEditorInset.webview
  // let html = View.Panel.makeHTML(webview, extensionPath)

  // Js.log(html)
  // webview->VSCode.Webview.setHtml(html)

  // let inset = LSP.WindowExt.createWebviewTextEditorInset(editor, 5, 2)
  // let html = "<p>WEBVIEW INSET HERE !!!</p>"
  // inset->LSP.WebviewEditorInset.webview->VSCode.Webview.setHtml(html)
  ()
}

module Handler = {
  let onSelect = event => {
    let selections = event->VSCode.TextEditorSelectionChangeEvent.selections
    let editor = event->VSCode.TextEditorSelectionChangeEvent.textEditor
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName

    // filter selection events out when we are modifying the editor programatically
    let shouldTrigger = switch event->VSCode.TextEditorSelectionChangeEvent.kind {
    | Some(VSCode.TextEditorSelectionChangeKind.Mouse)
    | Some(VSCode.TextEditorSelectionChangeKind.Keyboard) => true
    | _ => false
    }
    if shouldTrigger {
      Registry.get(filePath)->Option.forEach(state =>
        // TODO, there may be multiple selections at once
        selections[0]->Option.forEach(selection => {
          let start = VSCode.TextDocument.offsetAt(
            state.document,
            VSCode.Selection.start(selection),
          )
          let end_ = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.end_(selection))

          Client.send(Req(state.filePath, Inspect(start, end_)))
          ->Promise.flatMap(handleResponse)
          ->ignore
        })
      )
    }
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
      // this callback will be invoked when the first editor is opened
      onActivateExtension(() => {
        View.activate(context->VSCode.ExtensionContext.extensionPath)
        Client.start()->ignore
      })

      let state = switch Registry.get(filePath) {
      | None =>
        // state initialization
        let state = State.make(editor, View.send, View.on, Client.send)
        Registry.add(filePath, state)

        // registerInset()

        state
      | Some(state) =>
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        state.document = editor->VSCode.TextEditor.document
        state.filePath = filePath
        state
      }
      Client.send(Req(state.filePath, Load))->Promise.flatMap(handleResponse)->ignore
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
  VSCode.Window.onDidChangeActiveTextEditor(.next => {
    next->Option.forEach(Handler.onOpenEditor(context))
  })->subscribe

  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onCloseEditor)->subscribe

  // on change selection
  VSCode.Window.onDidChangeTextEditorSelection(. Handler.onSelect)->subscribe
  // on notification from the server
  Client.on(Handler.onNotification)

  // on command
  VSCode.Commands.registerCommand("guacamole.refine", () =>
    VSCode.Window.activeTextEditor->Option.map(editor => {
      let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
      Registry.get(filePath)->Option.mapWithDefault(Promise.resolved(), state => {
        state
        ->State.Spec.fromCursorPosition
        ->Option.mapWithDefault(Promise.resolved(), spec => {
          let payload = State.Spec.getPayload(state.document, spec)
          state.lspSendRequest(Req(filePath, Refine(spec.id, payload)))->Promise.flatMap(
            State.handleResponseWithState(state),
          )
        })
      })
    })
  )->subscribe
}

let deactivate = () => ()
