open Belt;
open Vscode;

module States = {
  let dict: Js.Dict.t(State.t) = Js.Dict.empty();

  let get = editor => {
    dict->Js.Dict.get(editor->TextEditor.document->TextDocument.fileName);
  };

  let getByFileName = fileName => {
    dict->Js.Dict.get(fileName);
  };
  let set = (fileName, state) => {
    Js.log2("[ states ][ set ]", fileName);
    // update the dict
    dict->Js.Dict.set(fileName, state);
  };

  // see if an TextEditor has been loaded
  let isLoaded = editor => {
    dict
    ->Js.Dict.get(editor->TextEditor.document->TextDocument.fileName)
    ->Option.isSome;
  };

  let disposeByFileName = fileName => {
    Js.log2("[ states ][ delete ]", fileName);
    getByFileName(fileName)->Option.forEach(State.dispose);
    [%raw "delete dict[fileName] "] |> ignore;
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
  ->Option.map(editor =>
      switch (States.get(editor)) {
      | None =>
        let state = State.make(context, editor, editor);
        States.set(editor->TextEditor.document->TextDocument.fileName, state);
        state;
      | Some(state) => state
      }
    );

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
        ->Array.forEach(file => {
            States.disposeByFileName(file##oldUri->Uri.path)
          })
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