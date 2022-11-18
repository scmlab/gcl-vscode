open Belt

/* the interfaces to backend:
  front=>back: see `View.on(handleViewResponse)`
  back=>front: see `Connection.onNotification(...`
*/

let isGCL = Js.Re.test_(%re("/\\.gcl$/i"))

let previouslyActivatedState: ref<option<State.t>> = ref(None)

let handleResponse = response =>
  switch response {
  | Response.Res(filePath, kinds) =>
    Registry.get(filePath)->Option.mapWithDefault(Promise.resolved(), state => {
      kinds->Array.map(State.handleResponseKind(state))->Util.Promise.oneByOne->Promise.map(_ => ())
    })
  | CannotSendRequest(message) =>
    State.displayError("Client Internal Error", "Cannot send request to the server\n" ++ message)
  | CannotDecodeRequest(message) =>
    State.displayError(
      "Server Internal Error",
      "Cannot decode request from the client\n" ++ message,
    )
  | CannotDecodeResponse(message, json) =>
    State.displayError(
      "Client Internal Error",
      "Cannot decode response from the server\n" ++ message ++ "\n" ++ Js.Json.stringify(json),
    )
  }

let sendLSPRequest = (state, kind) => {
  State.sendLSPRequest(state, kind)->Promise.flatMap(result =>
    switch result {
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      State.displayError(header, body)
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
    | InsertAnchor(hash) => sendLSPRequest(state, InsertAnchor(hash))->ignore
    | Substitute(id) => sendLSPRequest(state, Substitute(id))->ignore
    | Initialized => ()
    | Solve(hash) => sendLSPRequest(state, Solve(hash))->ignore
    | Destroyed => ()
    }
  })
}


module Events = {
  let isGCL = editor =>
    Js.Re.test_(%re("/\\.gcl$/i"), editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName)
  let isGCL' = document => Js.Re.test_(%re("/\\.gcl$/i"), document->VSCode.TextDocument.fileName)

  // callback only gets invoked when:
  //  0. Connection.isTurnedOff.contents == false
  //  1. An editor is opened or reactivated.
  //  2. The opened file extension is "gcl".
  let onOpenEditor = callback => {
    let f = editor =>
      if isGCL(editor) && Connection.isTurnedOn.contents {
        callback(editor)
      }

    VSCode.Window.activeTextEditor->Option.forEach(f)
    VSCode.Window.onDidChangeActiveTextEditor(.next => {
      next->Option.forEach(f)
    })
  }
  // When the editor of a gcl file is closed.
  // Plus the condition that Connection.isTurnedOff.contents == false
  let onCloseEditor = callback =>
    VSCode.Workspace.onDidCloseTextDocument(.document =>
      if isGCL'(document) && Connection.isTurnedOn.contents {
        callback(document)
      }
    )
    

  // callback only gets invoked when:
  //  0. Connection.isTurnedOff.contents == false (this is implemented by onOpenEditor)
  //  1. An editor is opened and no GCL files was opened before.
  //  2. The view panel is closed.
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
  //  0. Connection.isTurnedOff.contents == false (this is implemented by onCloseEditor)
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

  let onChangeCursorPosition = callback => 
    if Connection.isTurnedOn.contents {
      VSCode.Window.onDidChangeTextEditorSelection(. callback)
    } else {
      VSCode.Disposable.make(_=>())
    }
                     
  
  let onChangeTextDocument = callback => 
    if Connection.isTurnedOn.contents {
      VSCode.Workspace.onDidChangeTextDocument(. callback)
    } else {
      VSCode.Disposable.make(_=>())
    }
}

// we wish that the keybindings we use are only in effect when needed
// see https://code.visualstudio.com/api/references/when-clause-contexts
// see if the cursor is placed within a Spec (hole), and set the keybinding context accordingly
let updateCursorInHoleStatus = filePath =>
  // see if the cursor is placed within some Spec (hole)
  Registry.get(filePath)->Option.forEach(state => {
    let selections = state.editor->VSCode.TextEditor.selections

    // keep the TimeoutID so that we can clear it later
    let setTimeoutIDRef = ref(None)
    // wait for 100ms before inspecting the position of cursor 
    let setTimeoutID = Js.Global.setTimeout(() => {
      // see if there's any intersection between selections & Specs
      let cursorIsInSomeSpec =
        state.specifications
        ->Array.map(spec => {
          selections
          ->Array.map(selection => {
            let specRange = SrcLoc.Range.toVSCodeRange(spec.range)
            VSCode.Selection.intersection(selection, specRange)->Option.isSome
          })
          ->Array.some(x => x)
        })
        ->Array.some(x => x)

      // we wish that the keybindings we use are only in effect when needed
      // see https://code.visualstudio.com/api/references/when-clause-contexts
      // set the context "guabao-cursor-in-hole" to `true` so that `guabao:refine` can be triggered
      if cursorIsInSomeSpec {
        VSCode.Commands.setContext("guabao-cursor-in-hole", true)->ignore
      } else {
        VSCode.Commands.setContext("guabao-cursor-in-hole", false)->ignore
      }

      // unsubscribe this SetTimeout
      setTimeoutIDRef.contents->Option.forEach(id => Js.Global.clearTimeout(id))
    }, 100)
    setTimeoutIDRef := Some(setTimeoutID)
  })

let activate = (context: VSCode.ExtensionContext.t) => {
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  VSCode.Workspace.getConfiguration(None,None)
  ->VSCode.WorkspaceConfiguration.updateGlobalSettings("guabao.globalStoragePath", globalStoragePath, None)
  ->ignore
  let subscribe = x => x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore

  // on response/notification from the server
  Connection.onNotification(result =>
    switch result {
    | Ok(response) => handleResponse(response)->ignore
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      State.displayError(header, body)->ignore
    }
  )->subscribe

  // on LSP client-server error
  Connection.onError(error => {
    let (header, body) = Connection.Error.toString(error)
    State.displayError(header, body)->ignore
  })->subscribe

  // An editor of a gcl file is opend. (Not necessarily be the first time.)
  // The state needs to be updated for this file.
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
      State.decorateAndUpdateSpecs(state, state.specifications)
      State.decorateAndUpdatePOs(state, state.proofObligations)
      state
    }

    previouslyActivatedState := Some(state)
  })->subscribe

  // on close
  Events.onCloseEditor(document => {
    let filePath = VSCode.TextDocument.fileName(document)
    Registry.destroy(filePath)
  })->subscribe

  // helper function for starting a connection to the language server
  let connect = () => {
    Connection.start(globalStoragePath, State.onDownload)
    ->Promise.flatMap(result =>
      switch result {
      | Ok(sourceAndMethod) =>
        State.updateConnectionStatus(Connection.methodToString(sourceAndMethod))
      | Error(error) =>
        let (header, body) = Connection.Error.toString(error)
        State.displayError(header, body)
      })
  }

  // First gcl file is opend, the view panel and the connection needs to be created.
  Events.onActivateExtension(() => {
    // 1. activate the view
    let extensionPath = VSCode.ExtensionContext.extensionPath(context)
    View.activate(extensionPath)->Promise.flatMap(connect)->ignore
  })->subscribe

  // The last gcl file was closed and the view panel still exists.
  // -> the view panel needs to be closed.
  Events.onDeactivateExtension(_ => {
    View.deactivate()
    previouslyActivatedState := None
    Connection.stop()->ignore
  })->subscribe

  // on change cursor position/selection, for:
  // 1. send the `Inspect` request to the backend when the cursor is placed within some proof obligation
  // 2. see if the cursor is placed within a Spec (hole), and set the keybinding context accordingly
  Events.onChangeCursorPosition(event => {
    let selections = event->VSCode.TextEditorSelectionChangeEvent.selections
    let editor = event->VSCode.TextEditorSelectionChangeEvent.textEditor
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName

    // we don't want to trigger this event when we are digging holes
    // we can see if this event is triggered by the user
    let triggeredByUser = switch event->VSCode.TextEditorSelectionChangeEvent.kind {
    | Some(VSCode.TextEditorSelectionChangeKind.Mouse)
    | Some(VSCode.TextEditorSelectionChangeKind.Keyboard) => true
    | _ => false
    }
    if triggeredByUser {
      // send Inspect
      Registry.get(filePath)->Option.forEach(state =>
        // TODO, there may be multiple selections at once
        selections[0]->Option.forEach(selection => {
          let start = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.start(selection), state.document)
          let end = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.end_(selection), state.document)
          sendLSPRequest(state, Inspect(Range(start, end)))->ignore
        })
      )
    }
    // see if the cursor is placed within some Spec (hole)
    updateCursorInHoleStatus(filePath)
  })->subscribe

  // on change content of TextDocument, for:
  //  1. see if the cursor is placed within a Spec (hole), and set the keybinding context accordingly
  Events.onChangeTextDocument(event => {
    let filePath = event->VSCode.TextDocumentChangeEvent.document->VSCode.TextDocument.fileName
    // see if the cursor is placed within some Spec (hole)
    updateCursorInHoleStatus(filePath)
  })->subscribe

  // on events from the view
  View.on(handleViewResponse)->Promise.get(subscribe)

  // What the restart command does:
  let restartConnection = () =>
    // When server is stopped, getState() would return None.
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      previouslyActivatedState.contents = None
      let editor = state.editor
      let filePath = state.filePath
      // destroy
      //Registry.destroy(filePath)
      Js.Dict.keys(Registry.dict)->Array.forEach(Registry.destroy)
      
      // make
      let state = State.make(globalStoragePath, editor)
      Registry.add(filePath, state)
      previouslyActivatedState := Some(state)
      // reactivate the view
      let extensionPath = VSCode.ExtensionContext.extensionPath(context)
      View.deactivate()

      View.activate(extensionPath)->Promise.flatMap(Connection.stop)->Promise.flatMap(connect)

      //// Since in current version we're not connecting via TCP.
      // Connection.stop()->ignore

      // open NodeJs.ChildProcess
      // spawnSync("sleep",["0.5"],spawnSyncOptions(()))->ignore
      // // reconnect with GCL
      // View.activate(extensionPath)->Promise.flatMap(connect)
    })

  // on refine
  VSCode.Commands.registerCommand("guabao.refine", () =>
  // When server is stopped, getState() would return None.
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      let selection = state.editor->VSCode.TextEditor.selection
      let start = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.start(selection), state.document)
      let end = SrcLoc.Pos.fromVSCodePos(VSCode.Selection.end_(selection), state.document)
      sendLSPRequest(state, Refine(Range(start, end)))
    })->Promise.flatMap(()=>{
      // Invoking "restart" here as a workaround to solve strange behavior issue on windows.
      restartConnection()
    })
  )->subscribe

  // on restart
  VSCode.Commands.registerCommand("guabao.restart", restartConnection)->subscribe

  VSCode.Commands.registerCommand("guabao.stop", () =>
    if Connection.isTurnedOn.contents {
      switch getState() {
        | None => 
          // This case presumes that the view panel is not activated and the registry list should be blank.
          Connection.isTurnedOn := false
        | Some(_) => {
          Connection.isTurnedOn := false
          //Empty the registry list, the related state will be cleared altogether by Registry.destroy
          Js.Dict.keys(Registry.dict)->Array.forEach(Registry.destroy)
          //is it possible that nothing to destroy? when does a registry be made?
          View.deactivate()
          previouslyActivatedState := None
          Connection.stop()->ignore
        }
      }
    }
  )->subscribe

  VSCode.Commands.registerCommand("guabao.start", () =>
    if !Connection.isTurnedOn.contents {
      Connection.isTurnedOn := true
      let isGCL = editor =>
        Js.Re.test_(%re("/\\.gcl$/i"), editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName)
      VSCode.Window.activeTextEditor
      ->Option.flatMap(e => if isGCL(e) {Some(e)} else {None})
      ->Option.forEach(editor=>{
        // the current editor is opening a gcl file
        //register the file
        let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
        let state = State.make(globalStoragePath, editor)
        Registry.add(filePath, state)
        previouslyActivatedState := Some(state)

        //activating the view panel and make the connection (via the connect function)
        let extensionPath = VSCode.ExtensionContext.extensionPath(context)
        View.activate(extensionPath)->Promise.flatMap(connect)->ignore
          
      })
    }
  )->subscribe

  // on debug
  VSCode.Commands.registerCommand("guabao.debug", () =>
    getState()->Option.mapWithDefault(Promise.resolved(), state => {
      sendLSPRequest(state, Debug)
    })
  )->subscribe

  // on force check update
  VSCode.Commands.registerCommand("guabao.force-check-update", () => {
    let releaseDataPath = NodeJs.Path.join2(globalStoragePath, "releases-cache-.json")
    // invalidate the release data cache by deleting it
    if NodeJs.Fs.existsSync(releaseDataPath) {
      NodeJs.Fs.unlinkSync(releaseDataPath)
    }
  })->subscribe
}

let deactivate = () => {
  Connection.stop()
}
