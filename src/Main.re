open Belt;
open Vscode;

module Impl = (Interface: Editor.Interface) => {
  module State = State.Impl(Interface);

  module States = {
    // a dictionary of (FileName, State) entries
    let dict: Js.Dict.t(State.t) = Js.Dict.empty();

    let get = fileName => {
      dict->Js.Dict.get(fileName);
    };

    let getByEditor = (editor: Interface.editor) => {
      get(editor->Interface.editorFileName);
    };

    // do nothing if the state already exists
    let add = (fileName, state) => {
      // let fileName = editor->Interface.getFileName';
      switch (get(fileName)) {
      | Some(_) => ()
      | None =>
        Js.log("[ state ][ add ]");
        dict->Js.Dict.set(fileName, state);
      };
    };

    let rename = (oldName, newName) => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(oldName)
      ->Option.forEach(state => {
          Js.log3("[ state ][ rename ]", oldName, newName);
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
          Js.log("[ state ][ destroy ]");
          State.destroy(state);
          delete_(dict, fileName);
        });
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.map(((_, state)) => State.destroy(state));
    };
  };
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

  let addToSubscriptions = (f, context) =>
    f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;

  let activate = context => {
    // when a TextEditor gets closed, destroy the corresponding State
    Interface.onDidCloseEditor(States.destroy)
    ->Interface.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Interface.onDidChangeFileName((oldName, newName) =>
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
    ->Interface.addToSubscriptions(context);
    // on editor activation, reveal the corresponding Panel (if any)
    Interface.onDidChangeActivation((_previous, next) => {
      next
      ->Option.flatMap(States.get)
      ->Option.forEach(Js.log2("[activate]"))
    })
    ->Interface.addToSubscriptions(context);

    // on load
    Interface.registerCommand("load", editor => {
      let fileName = editor->Interface.editorFileName;
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
    ->Interface.addToSubscriptions(context);
  };

  let deactive = () => {
    States.destroyAll();
  };
};

include Impl(VscodeImpl);