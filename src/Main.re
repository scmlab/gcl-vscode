open Belt;

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

let updateViewOnOpen = (extensionPath, state) => {
  // number of visible GCL file in the workplace
  let visibleCount =
    VSCode.Window.visibleTextEditors
    ->Array.keep(editor =>
        isGCL(
          editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        )
      )
    ->Array.length;
  // should activate the view when there's a visible GCL file
  let shouldAcitvateView = visibleCount > 0 && !View.isActivated();

  if (shouldAcitvateView) {
    View.activate(extensionPath);
  };

  state->Option.forEach(View.wire);
};

let updateViewOnClose = () => {
  // number of GCL States in the Registry
  let openedCount = Registry.size();
  // should deacitvate the view when all GCL States have been destroyed
  let shouldDeacitvateView = openedCount === 0 && View.isActivated();

  if (shouldDeacitvateView) {
    View.deactivate();
  };
};

module Handler = {
  let onSelect = event => {
    let selections = event->VSCode.TextEditorSelectionChangeEvent.selections;
    let editor = event->VSCode.TextEditorSelectionChangeEvent.textEditor;
    let filePath =
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName;

    Registry.get(filePath)
    ->Option.forEach(state => {
        // TODO, there may be multiple selections at once
        selections[0]
        ->Option.forEach(selection => {
            let start =
              VSCode.TextDocument.offsetAt(
                state.document,
                VSCode.Selection.start(selection),
              );
            let end_ =
              VSCode.TextDocument.offsetAt(
                state.document,
                VSCode.Selection.end_(selection),
              );
            state
            ->State.sendRequest(Request.Inspect(state.filePath, start, end_))
            ->Promise.getOk(State.handleResponse(state));
          })
      });
  };

  let onOpen = (context, editor) => {
    let filePath =
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName;
    // filter out ".gcl.git" files
    if (isGCL(filePath)) {
      switch (Registry.get(filePath)) {
      | None =>
        // state initialization
        let state = State.make(editor);
        Registry.add(filePath, state);
      | Some(_) => ()
      };

      // update the view
      updateViewOnOpen(
        context->VSCode.ExtensionContext.extensionPath,
        Registry.get(filePath),
      );
    };
  };

  let onClose = doc => {
    let filePath = VSCode.TextDocument.fileName(doc);
    if (isGCL(filePath)) {
      Js.log("CLOSE " ++ filePath);
      Registry.destroy(filePath);
      // update the view
      updateViewOnClose();
    };
  };
};

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x =>
    x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore;

  // on open
  VSCode.Window.activeTextEditor->Option.forEach(Handler.onOpen(context));
  VSCode.Window.onDidChangeActiveTextEditor(next =>
    next->Option.forEach(Handler.onOpen(context))
  )
  ->subscribe;

  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onClose)->subscribe;

  // on change selection
  VSCode.Window.onDidChangeTextEditorSelection(Handler.onSelect)->subscribe;

  ();
  // registerCommand("toggle", editor => {
  //   // state initialization
  //   let state = State.make(editor);
  //   // view initialization
  //   let extensionPath = context->VSCode.ExtensionContext.extensionPath;
  //   state.view = Some(View.make(extensionPath, editor));
  //   state.view
  //   ->Option.forEach(view => {
  //       View.send(
  //         view,
  //         ViewType.Request.Display(
  //           Plain("Proof Obligations"),
  //           ProofObligations(0, [||], [||]),
  //         ),
  //       )
  //       ->ignore
  //     });
  //   let filePath =
  //     editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName;
  //   Registry.add(filePath, state);
  // })
  // ->subscribe;
};

let deactivate = () => {
  ();
};
