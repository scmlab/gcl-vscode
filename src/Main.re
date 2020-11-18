open Belt;

module Handler = {
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

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
      Js.log("OPEN " ++ filePath);
      Js.log("size " ++ string_of_int(Registry.size()));
      switch (Registry.get(filePath)) {
      | None =>
        // state initialization
        let state = State.make(editor);
        // view initialization
        let extensionPath = context->VSCode.ExtensionContext.extensionPath;
        // state.view = Some(View.make(extensionPath, editor));
        // state.view
        // ->Option.forEach(view => {
        //     View.send(
        //       view,
        //       ViewType.Request.Display(
        //         Plain("Proof Obligations"),
        //         ProofObligations(0, [||], [||]),
        //       ),
        //     )
        //     ->ignore
        //   });
        Registry.add(filePath, state);
      | Some(_) => ()
      };
    };
  };

  let onClose = doc => {
    let filePath = VSCode.TextDocument.fileName(doc);
    if (isGCL(filePath)) {
      Js.log("CLOSE " ++ filePath);
      Js.log("size " ++ string_of_int(Registry.size()));
      Registry.destroy(filePath);
    };
  };
  // let previous = ref(VSCode.Window.activeTextEditor);
  // let onChangeActivation = next => {
  //   let nextFileName =
  //     next
  //     ->Option.map(VSCode.TextEditor.document)
  //     ->Option.map(VSCode.TextDocument.fileName);
  //   let prevFileName =
  //     (previous^)
  //     ->Option.map(VSCode.TextEditor.document)
  //     ->Option.map(VSCode.TextDocument.fileName);
  //   // see if the active text editor changed;
  //   let changed = nextFileName != prevFileName;
  //   Js.log(
  //     prevFileName->Option.getWithDefault("")
  //     ++ " => "
  //     ++ nextFileName->Option.getWithDefault(""),
  //   );
  //   if (changed) {
  //     // prevFileName->Option.flatMap(Registry.get)->Option.forEach(State.hide);
  //     // nextFileName->Option.flatMap(Registry.get)->Option.forEach(State.show);
  //     previous := next;
  //   };
  // };
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
