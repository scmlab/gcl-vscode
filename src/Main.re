open Belt;
open Vscode;

module States = {
  let dict: Js.Dict.t(State.t) = Js.Dict.empty();

  let get = editor => {
    dict->Js.Dict.get(editor.TextEditor.document->TextDocument.fileName);
  };

  let getByFileName = fileName => {
    dict->Js.Dict.get(fileName);
  };

  let set = (fileName, state) => {
    // update the dict
    dict->Js.Dict.set(fileName, state);
  };

  // see if an TextEditor has been loaded
  let isLoaded = editor => {
    dict
    ->Js.Dict.get(editor.TextEditor.document->TextDocument.fileName)
    ->Option.isSome;
  };

  let dispose = () => {
    dict->Js.Dict.entries->Array.map(((_, state)) => State.dispose(state));
  };
};

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

let getOrMakeState = context =>
  Window.activeTextEditor
  // initializing only GCL files
  ->Option.flatMap(editor =>
      isGCL(editor.document->TextDocument.fileName) ? Some(editor) : None
    )
  ->Option.map(editor =>
      switch (States.get(editor)) {
      | None =>
        let state = State.make(context, editor);
        States.set(editor.document->TextDocument.fileName, state);
        state;
      | Some(state) => state
      }
    );

let activate = (context: ExtensionContext.t) => {
  // // initialize the current opend TextEditor if it's a GCL file
  // Window.activeTextEditor->Option.forEach(editor => {
  //   let fileName = editor.document.fileName;
  //   if (isGCL(fileName)) {
  //     let state = State.make(context, editor);
  //     States.set(fileName, state);
  //   };
  // });

  // initialize the current opened GCL files
  // Workspace.textDocuments
  // ->Array.keep(isGCL)
  // ->Array.forEach(textDoc => {
  //     let state = State.make(context, editor);
  //     States.set(textDoc.fileName, state);
  //   })
  // ->Js.log;

  // Workspace.textDocuments->Array.map(textDoc => textDoc.fileName)->Js.log;

  // Workspace.onDidOpenTextDocument(textDoc => {
  //   textDoc.fileName->Js.log2("[open]")
  // })
  // ->Js.Array.push(context->ExtensionContext.subscriptions)
  // ->ignore;

  // Workspace.onDidCloseTextDocument(textDoc => {
  //   textDoc.fileName->Js.log2("[close]")
  // })
  // ->Js.Array.push(context->ExtensionContext.subscriptions)
  // ->ignore;

  // when a TextEditor gets activated, reveal the corresponding Panel (if any)
  Window.onDidChangeActiveTextEditor(editor => {
    editor->Option.flatMap(States.get)->Option.forEach(View.activate)
  })
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  // when a TextEditor gets closed, close the corresponding Panel (if any)
  Workspace.onDidCloseTextDocument(textDoc => {
    textDoc
    ->Option.map(TextDocument.fileName)
    ->Option.flatMap(States.getByFileName)
    ->Option.forEach(State.dispose)
  })
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  // on load
  Commands.registerCommand("extension.load", () =>
    getOrMakeState(context)->Option.forEach(Command.load)
  )
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;
  // on save
  // Commands.registerCommand("workbench.action.files.save", () =>
  //   getState(context)->Option.forEach(Js.log)
  // )
  // ->Js.Array.push(context->ExtensionContext.subscriptions)
  // ->ignore;
};

let deactive = () => {
  States.dispose();
};