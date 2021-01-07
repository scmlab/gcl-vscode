open Belt

let isGCL = Js.Re.test_(%re("/\\.gcl$/i"))

let handleResponse = response =>
  switch response {
  | Response.Res(filePath, kinds) =>
    Registry.get(filePath)->Option.mapWithDefault(Promise.resolved(), state =>
      kinds->Array.map(State.handleResponseKind(state))->Util.Promise.oneByOne->Promise.map(_ => ())
    )
  | CannotSendRequest(message) =>
    State.displayErrorMessages([
      ("Client Internal Error", "Cannot send request to the server\n" ++ message),
    ])
  | CannotDecodeRequest(message) =>
    State.displayErrorMessages([
      ("Server Internal Error", "Cannot decode request from the client\n" ++ message),
    ])
  | CannotDecodeResponse(message, json) =>
    State.displayErrorMessages([
      (
        "Client Internal Error",
        "Cannot decode response from the server\n" ++ message ++ "\n" ++ Js.Json.stringify(json),
      ),
    ])
  }

let getState = () =>
  VSCode.Window.activeTextEditor
  ->Option.map(VSCode.TextEditor.document)
  ->Option.map(VSCode.TextDocument.fileName)
  ->Option.flatMap(Registry.get)

let handleViewResponse = response => getState()->Option.forEach(state => {
    switch response {
    | ViewType.Response.Link(MouseOver(loc)) =>
      let key = GCL.Loc.toString(loc)
      let range = GCL.Loc.toRange(loc)
      State.Decoration.addBackground(state, key, range, "statusBar.debuggingBackground")
    | Link(MouseOut(loc)) =>
      let key = GCL.Loc.toString(loc)
      State.Decoration.remove(key)
    | Link(MouseClick(loc)) =>
      ()
      let key = GCL.Loc.toString(loc)
      State.Decoration.remove(key)
    // let key = GCL.Loc.toString(loc)
    // let range = GCL.Loc.toRange(loc)
    // // focus on the editor
    // focus(state)
    // // select the source on the editor
    // let selection = VSCode.Selection.make(VSCode.Range.start(range), VSCode.Range.end_(range))
    // state.editor->VSCode.TextEditor.setSelection(selection)
    // Decoration.remove(key)
    | Substitute(id, expr, subst) =>
      // remove all decorations
      State.Decoration.removeAll()
      // send request to the server
      State.sendLSPRequest(state, Request.Kind.Substitute(id, expr, subst))
      ->Promise.flatMap(handleResponse)
      ->ignore
    | ExportProofObligations =>
      State.sendLSPRequest(state, Request.Kind.ExportProofObligations)
      ->Promise.flatMap(handleResponse)
      ->ignore
    | Initialized => ()
    | Destroyed => ()
    }
  })

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

module Events = {
  let isGCL = editor =>
    Js.Re.test_(%re("/\\.gcl$/i"), editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName)
  let isGCL' = document => Js.Re.test_(%re("/\\.gcl$/i"), document->VSCode.TextDocument.fileName)

  // callback only gets invoked when:
  //  1. an editor is opened or reactivated
  //  1. the opened file extension is "gcl"
  let onOpenEditor = callback => {
    let f = editor =>
      if isGCL(editor) {
        callback(editor)
      }

    VSCode.Window.activeTextEditor->Option.forEach(f)
    VSCode.Window.onDidChangeActiveTextEditor(.next => {
      next->Option.forEach(f)
    })
  }
  let onCloseEditor = callback => VSCode.Workspace.onDidCloseTextDocument(.document =>
      if isGCL'(document) {
        callback(document)
      }
    )

  // callback only gets invoked when:
  //  1. no GCL files was opened
  //  2. the view is closed
  let onActivateExtension = callback => onOpenEditor(_ => {
      // number of visible GCL file in the workplace
      let visibleCount = VSCode.Window.visibleTextEditors->Array.keep(isGCL)->Array.length
      // should activate the view when there's a visible GCL file
      let shouldAcitvateView = visibleCount > 0 && !View.isActivated()

      if shouldAcitvateView {
        callback()
      }
    })

  // callback only gets invoked when:
  //  1. no GCL files was opened
  //  2. the view is opened
  let onDeactivateExtension = callback => onCloseEditor(_ => {
      // number of GCL States in the Registry
      let openedCount = Registry.size()
      // should deacitvate the view when all GCL States have been destroyed
      let shouldDeacitvateView = openedCount === 0 && View.isActivated()

      if shouldDeacitvateView {
        callback()
      }
    })

  let onChangeCursorPosition = callback => VSCode.Window.onDidChangeTextEditorSelection(. callback)
}

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x => x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore

  // on open
  Events.onOpenEditor(editor => {
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName

    let state = switch Registry.get(filePath) {
    | None =>
      let state = State.make(editor)
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
    State.sendLSPRequest(state, Load)->Promise.flatMap(handleResponse)->ignore
  })->subscribe

  // on close
  Events.onCloseEditor(document => {
    let filePath = VSCode.TextDocument.fileName(document)
    Registry.destroy(filePath)
  })->subscribe

  // on extension activation
  Events.onActivateExtension(() => {
    View.activate(context->VSCode.ExtensionContext.extensionPath)
    LSP.Client.start()->ignore
  })->subscribe

  // on extension deactivation
  Events.onDeactivateExtension(_ => {
    View.deactivate()
    LSP.Client.stop()->ignore
  })->subscribe

  // on change cursor position/selection
  Events.onChangeCursorPosition(event => {
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

          State.sendLSPRequest(state, Inspect(start, end_))->Promise.flatMap(handleResponse)->ignore
        })
      )
    }
  })->subscribe

  // on response/notification from the server
  LSP.Client.on(response => handleResponse(response)->ignore)

  // on events from the view
  View.on(handleViewResponse)->subscribe

  // on refine
  VSCode.Commands.registerCommand("guacamole.refine", () =>
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      state->State.Spec.fromCursorPosition->Option.mapWithDefault(Promise.resolved(), spec => {
        let payload = State.Spec.getPayload(state.document, spec)
        State.sendLSPRequest(state, Refine(spec.id, payload))->Promise.flatMap(handleResponse)
      })
    })
  )->subscribe
}

let deactivate = () => ()
