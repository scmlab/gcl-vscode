open Belt;
open Vscode;

module Impl = (Interface: Editor.Interface) => {
  module States = {
    // a dictionary of (FileName, State) entries
    let dict: Js.Dict.t(State.t) = Js.Dict.empty();

    let get = fileName => {
      dict->Js.Dict.get(fileName);
    };

    let getByEditor = (editor: Interface.editor) => {
      get(editor->Interface.getFileName');
    };

    // do nothing if the state already exists
    let add = (fileName, state) => {
      // let fileName = editor->Interface.getFileName';
      switch (get(fileName)) {
      | Some(_) => ()
      | None => dict->Js.Dict.set(fileName, state)
      };
    };

    let rename = (oldName, newName) => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(oldName)
      ->Option.forEach(state => {
          delete_(dict, oldName);
          add(newName, state);
        });
    };

    let destroy = fileName => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(fileName)->Option.forEach(State.destroy);
      delete_(dict, fileName);
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.map(((_, state)) => State.destroy(state));
    };
  };
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
  // let getActiveGCLEditor = () =>
  //   // initializing only GCL files
  //   Window.activeTextEditor->Option.flatMap(editor =>
  //     isGCL(editor->TextEditor.document->TextDocument.fileName)
  //       ? Some(editor) : None
  //   );
  // let getOrMakeState = context =>
  //   getActiveGCLEditor()
  //   ->Option.map(editor => {
  //       switch (States.getByEditor(editor)) {
  //       | None =>
  //         let state = State.make(context, editor);
  //         States.add(editor, state);
  //         state;
  //       | Some(state) => state
  //       }
  //     });
  // let get = () => getActiveGCLEditor()->Option.flatMap(States.getByEditor);
  let addToSubscriptions = (f, context) =>
    f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;
  let activate = (context: Interface.context) => {
    // when a TextEditor gets closed, destroy the corresponding State
    Interface.onDidCloseEditor(States.destroy)
    ->Interface.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Interface.onDidChangeFileName((oldName, newName) =>
      if (States.contains(oldName)) {
        if (isGCL(newName)) {
          States.rename(oldName, newName);
        } else {
          States.destroy(oldName);
        };
      }
    )
    ->Interface.addToSubscriptions(context);
  };
  let deactive = () => {
    States.destroyAll();
  };
};

// // when a GCL file becomes a non-GCL file
// Workspace.onDidRenameFiles(event =>
//   event
//   ->Option.map(Vscode.FileRenameEvent.files)
//   ->Option.forEach(files => {
//       files
//       ->Array.keep(file => file##oldUri->Uri.path->isGCL)
//       ->Array.keep(file => file##newUri->Uri.path->isGCL->(!))
//       ->Array.forEach(file => States.destroy(file##oldUri->Uri.path))
//     })
// )
// ->addToSubscriptions(context);
// // when a TextEditor gets activated, reveal the corresponding Panel (if any)
// Window.onDidChangeActiveTextEditor(editor => {
//   editor
//   ->Option.flatMap(States.getByEditor)
//   ->Option.forEach(View.activate)
// })
// ->addToSubscriptions(context);
// // on load
// Commands.registerCommand("extension.load", () =>
//   getOrMakeState(context)->Option.forEach(Command.load)
// )
// ->addToSubscriptions(context);
// // on save
// Commands.registerCommand("workbench.action.files.save", () =>
//   get()
//   ->Option.forEach(state =>
//       state.editor->TextEditor.document->TextDocument.fileName->Js.log
//     )
// )
// ->addToSubscriptions(context);
include Impl(VscodeImpl);