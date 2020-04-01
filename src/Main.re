open Belt;
open Vscode;

module StateDict = {
  module Impl = (Editor: Editor.Sig) => {
    module State = State.Impl(Editor);
    // a dictionary of (FileName, State) entries
    let dict: Js.Dict.t(State.t) = Js.Dict.empty();

    let get = fileName => {
      dict->Js.Dict.get(fileName);
    };

    let getByEditor = (editor: Editor.editor) => {
      get(editor->Editor.editorFileName);
    };

    // do nothing if the state already exists
    let add = (fileName, state) => {
      // let fileName = editor->Interface.getFileName';
      switch (get(fileName)) {
      | Some(_) => ()
      | None =>
        Js.log("[ states ][ add ]");
        dict->Js.Dict.set(fileName, state);
      };
    };

    let rename = (oldName, newName) => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(oldName)
      ->Option.forEach(state => {
          Js.log3("[ states ][ rename ]", oldName, newName);
          delete_(dict, oldName);
          add(newName, state);
        });
    };

    let destroy = fileName => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(fileName)
      ->Option.forEach(state => {
          Js.log("[ states ][ destroy ]");
          State.destroy(state);
          delete_(dict, fileName);
        });
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.map(((_, state)) => State.destroy(state))
      ->ignore;
    };
  };
};

module Impl = (Editor: Editor.Sig) => {
  module State = State.Impl(Editor);
  module States = StateDict.Impl(Editor);

  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

  let addToSubscriptions = (f, context) =>
    f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;

  let activate = context => {
    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(States.destroy)
    ->Editor.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (States.contains(oldName)) {
            if (isGCL(newName)) {
              States.rename(oldName, newName);
            } else {
              States.destroy(oldName);
            };
          }
        )
      )
    )
    ->Editor.addToSubscriptions(context);
    // on editor activation, reveal the corresponding Panel (if any)
    Editor.onDidChangeActivation((_previous, next) => {
      next
      ->Option.flatMap(States.get)
      ->Option.forEach(Js.log2("[activate]"))
    })
    ->Editor.addToSubscriptions(context);

    // on load
    Editor.registerCommand("load", editor => {
      let fileName = editor->Editor.editorFileName;
      if (isGCL(fileName)) {
        // see if it's already in the States
        switch (States.get(fileName)) {
        | None =>
          Js.log("[ main ][ first LOAD ]");
          let state = State.make(context, editor);
          States.add(fileName, state);
        | Some(_state) => Js.log("[ main ][ LOAD ]")
        };
      };
    })
    ->Editor.addToSubscriptions(context);
  };

  let deactive = () => {
    States.destroyAll();
  };
};

include Impl(VscodeImpl);