open Belt;
open Vscode;

module States = {
  let dict: Js.Dict.t(State.t) = Js.Dict.empty();

  let getByFileName = fileName => {
    dict->Js.Dict.get(fileName);
  };

  let get = editor => {
    getByFileName(editor->TextEditor.document->TextDocument.fileName);
  };

  let set = (editor, state) => {
    switch (get(editor)) {
    | Some(_) => ()
    | None =>
      dict->Js.Dict.set(
        editor->TextEditor.document->TextDocument.fileName,
        state,
      )
    };
  };

  let disposeByFileName = fileName => {
    let delete_: (Js.Dict.t('a), string) => unit = [%raw
      "function (dict, key) {delete dict[key]}"
    ];
    getByFileName(fileName)->Option.forEach(State.dispose);
    delete_(dict, fileName);
  };

  let dispose = editor => {
    let delete_: (Js.Dict.t('a), string) => unit = [%raw
      "function (dict, key) {delete dict[key]}"
    ];
    get(editor)->Option.forEach(State.dispose);

    let fileName = editor->TextEditor.document->TextDocument.fileName;
    delete_(dict, fileName);
  };

  let disposeAll = () => {
    dict->Js.Dict.entries->Array.map(((_, state)) => State.dispose(state));
  };
};

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

let getActiveGCLEditor = () =>
  // initializing only GCL files
  Window.activeTextEditor->Option.flatMap(editor =>
    isGCL(editor->TextEditor.document->TextDocument.fileName)
      ? Some(editor) : None
  );

let getOrMakeState = context =>
  getActiveGCLEditor()
  ->Option.map(editor => {
      switch (States.get(editor)) {
      | None =>
        let state = State.make(context, editor);
        States.set(editor, state);
        state;
      | Some(state) => state
      }
    });

let get = () => getActiveGCLEditor()->Option.flatMap(States.get);

let addToSubscriptions = (f, context) =>
  f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;

let activate = (context: ExtensionContext.t) => {
  // when a GCL file becomes a non-GCL file
  Workspace.onDidRenameFiles(event =>
    event
    ->Option.map(Vscode.FileRenameEvent.files)
    ->Option.forEach(files => {
        files
        ->Array.keep(file => file##oldUri->Uri.path->isGCL)
        ->Array.keep(file => file##newUri->Uri.path->isGCL->(!))
        ->Array.forEach(file =>
            States.disposeByFileName(file##oldUri->Uri.path)
          )
      })
  )
  ->addToSubscriptions(context);

  // when a TextEditor gets closed, close the corresponding Panel (if any)
  Workspace.onDidCloseTextDocument(textDoc => {
    textDoc
    ->Option.map(TextDocument.fileName)
    ->Option.flatMap(States.getByFileName)
    ->Option.forEach(State.dispose)
  })
  ->addToSubscriptions(context);

  // when a TextEditor gets activated, reveal the corresponding Panel (if any)
  Window.onDidChangeActiveTextEditor(editor => {
    editor->Option.flatMap(States.get)->Option.forEach(View.activate)
  })
  ->addToSubscriptions(context);

  // on load
  Commands.registerCommand("extension.load", () =>
    getOrMakeState(context)->Option.forEach(Command.load)
  )
  ->addToSubscriptions(context);

  // on save
  Commands.registerCommand("workbench.action.files.save", () =>
    get()
    ->Option.forEach(state =>
        state.editor->TextEditor.document->TextDocument.fileName->Js.log
      )
  )
  ->addToSubscriptions(context);
};

let deactive = () => {
  States.disposeAll();
};