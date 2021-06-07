open Belt

let isGCL = Js.Re.test_(%re("/\\.gcl$/i"))

let previouslyActivatedState: ref<option<State.t>> = ref(None)

let handleResponse = response =>
  switch response {
  | Response.Res(filePath, kinds) =>
    Registry.get(filePath)->Option.mapWithDefault(Promise.resolved(), state => {
      kinds->Array.map(State.handleResponseKind(state))->Util.Promise.oneByOne->Promise.map(_ => ())
    })
  | CannotSendRequest(message) =>
    State.display(
      0,
      [
        Element.Block.block(
          Some("Client Internal Error"),
          None,
          Element.Inlines.string("Cannot send request to the server\n" ++ message),
        ),
      ],
    )
  | CannotDecodeRequest(message) =>
    State.display(
      0,
      [
        Element.Block.block(
          Some("Server Internal Error"),
          None,
          Element.Inlines.string("Cannot decode request from the client\n" ++ message),
        ),
      ],
    )
  | CannotDecodeResponse(message, json) =>
    State.display(
      0,
      [
        Element.Block.block(
          Some("Client Internal Error"),
          None,
          Element.Inlines.string(
            "Cannot decode response from the server\n" ++
            message ++
            "\n" ++
            Js.Json.stringify(json),
          ),
        ),
      ],
    )
  }

let sendLSPRequest = (state, kind) => {
  State.sendLSPRequest(state, kind)->Promise.flatMap(result =>
    switch result {
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      State.display(0, [Element.Block.block(Some(header), None, Element.Inlines.string(body))])
    | Ok(response) => handleResponse(response)
    }
  )
}

let getState = () => previouslyActivatedState.contents

let handleViewResponse = response => {
  getState()->Option.forEach(state => {
    switch response {
    | ViewType.Response.Link(MouseOver(range)) =>
      let key = SrcLoc.Range.toString(range)
      let range = SrcLoc.Range.toVSCodeRange(range)
      State.Decoration.addBackground(state, key, range, "statusBar.debuggingBackground")
    | Link(MouseOut(range)) =>
      let key = SrcLoc.Range.toString(range)
      State.Decoration.remove(key)
    | Link(MouseClick(range)) =>
      let key = SrcLoc.Range.toString(range)
      State.Decoration.remove(key)
      // focus on the editor
      State.focus(state)
      // select the source on the editor
      let range = SrcLoc.Range.toVSCodeRange(range)
      let selection = VSCode.Selection.make(VSCode.Range.start(range), VSCode.Range.end_(range))
      state.editor->VSCode.TextEditor.setSelection(selection)
    // State.Decoration.remove(key)
    | ExportProofObligations => sendLSPRequest(state, Request.Kind.ExportProofObligations)->ignore
    | Initialized => ()
    | Destroyed => ()
    }
  })
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
  let onCloseEditor = callback =>
    VSCode.Workspace.onDidCloseTextDocument(.document =>
      if isGCL'(document) {
        callback(document)
      }
    )

  // callback only gets invoked when:
  //  1. no GCL files was opened
  //  2. the view is closed
  let onActivateExtension = callback =>
    onOpenEditor(_ => {
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
  let onDeactivateExtension = callback =>
    onCloseEditor(_ => {
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
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  let subscribe = x => x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore

  // on response/notification from the server
  Connection.onResponse(result =>
    switch result {
    | Ok(response) => handleResponse(response)->ignore
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      State.display(
        0,
        [Element.Block.block(Some(header), None, Element.Inlines.string(body))],
      )->ignore
    }
  )->subscribe

  // on LSP client-server error
  Connection.onError(error => {
    let (header, body) = Connection.Error.toString(error)
    State.display(
      0,
      [Element.Block.block(Some(header), None, Element.Inlines.string(body))],
    )->ignore
  })->subscribe

  // on open
  Events.onOpenEditor(editor => {
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName

    let state = switch Registry.get(filePath) {
    | None =>
      let state = State.make(globalStoragePath, editor)
      Registry.add(filePath, state)
      state
    | Some(state) =>
      // after switching tabs, the old editor would be "_disposed"
      // we need to replace it with this new one
      state.editor = editor
      state.document = editor->VSCode.TextEditor.document
      state.filePath = filePath
      State.Spec.redecorate(state, state.specifications)
      state
    }

    previouslyActivatedState := Some(state)
  })->subscribe

  // on close
  Events.onCloseEditor(document => {
    let filePath = VSCode.TextDocument.fileName(document)
    Registry.destroy(filePath)
  })->subscribe

  // on extension activation
  Events.onActivateExtension(() => {
    // 1. activate the view
    let extensionPath = VSCode.ExtensionContext.extensionPath(context)
    View.activate(extensionPath)
    ->Promise.flatMap(() => Connection.start(globalStoragePath))
    ->Promise.get(result =>
      switch result {
      | Ok(method) => State.updateConnection(Some(method))->ignore
      | Error(error) => Js.log(error)
      }
    )
  })->subscribe

  // on extension deactivation
  Events.onDeactivateExtension(_ => {
    View.deactivate()
    previouslyActivatedState := None
    Connection.stop()->ignore
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
          let start = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.start(selection), state.document)
          let end = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.end_(selection), state.document)
          sendLSPRequest(state, Inspect(Range(start, end)))->ignore
        })
      )
    }
  })->subscribe

  // on events from the view
  View.on(handleViewResponse)->Promise.get(subscribe)

  // on refine
  VSCode.Commands.registerCommand("guabao.refine", () =>
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      let selection = state.editor->VSCode.TextEditor.selection
      let start = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.start(selection), state.document)
      let end = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.end_(selection), state.document)
      sendLSPRequest(state, Refine(Range(start, end)))
    })
  )->subscribe

  // on restart
  VSCode.Commands.registerCommand("guabao.restart", () =>
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      previouslyActivatedState.contents = None
      let editor = state.editor
      let filePath = state.filePath
      // destroy
      Registry.destroy(filePath)
      // make
      let state = State.make(globalStoragePath, editor)
      Registry.add(filePath, state)
      previouslyActivatedState := Some(state)
      // reactivate the view
      let extensionPath = VSCode.ExtensionContext.extensionPath(context)
      View.deactivate()
      View.activate(extensionPath)
      // reconnect with GCL
      ->Promise.flatMap(Connection.stop)
      ->Promise.flatMap(() => Connection.start(globalStoragePath))
      ->Promise.get(result =>
        switch result {
        | Ok(method) => State.updateConnection(Some(method))->ignore
        | Error(error) => Js.log(error)
        }
      )

      Promise.resolved()
    })
  )->subscribe

  // on debug
  VSCode.Commands.registerCommand("guabao.debug", () =>
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      sendLSPRequest(state, Debug)
    })
  )->subscribe
}

let deactivate = () => ()
