open Belt;
open Vscode;

// a dictionary of FileName-State entries
module StateDict = {
  module Impl = (Editor: Sig.Editor, State: State.Sig) => {
    module State = State(Editor);
    let dict: Js.Dict.t(State.t) = Js.Dict.empty();

    let get = fileName => dict->Js.Dict.get(fileName);

    let getByEditor = (editor: Editor.editor) =>
      editor->Editor.getFileName->Option.flatMap(get);

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
          State.destroy(state) |> ignore;
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

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

module Impl = (Editor: Sig.Editor, State: State.Sig) => {
  module States = StateDict.Impl(Editor, State);
  module TaskCommand = Task__Command.Impl(Editor, State);
  module TaskRunner = TaskRunner.Impl(Editor, State);
  module State = State(Editor);

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
    Editor.onDidChangeActivation((prev, next) => {
      prev->Option.flatMap(States.get)->Option.forEach(State.hide);
      next->Option.flatMap(States.get)->Option.forEach(State.show);
    })
    ->Editor.addToSubscriptions(context);

    // on "toggle activate/deactivate"
    Editor.registerCommand("toggle", editor => {
      editor
      ->Editor.getFileName
      ->Option.forEach(fileName => {
          // see if it's already in the States
          switch (States.get(fileName)) {
          | None =>
            Js.log("[ main ][ toggle activate ]");
            let state = State.make(context, editor);
            States.add(fileName, state);
          | Some(_state) =>
            Js.log("[ main ][ toggle deactivate ]");
            fileName->States.destroy;
          }
        })
    })
    ->Editor.addToSubscriptions(context);

    // on other commands
    Command.names->Array.forEach(((command, name)) => {
      Editor.registerCommand(name, editor => {
        editor
        ->States.getByEditor
        ->Option.forEach(state =>
            TaskCommand.dispatch(command) |> TaskRunner.run(state) |> ignore
          )
      })
      ->Editor.addToSubscriptions(context)
    });
    // // on "reload"
    // Editor.registerCommand("reload", editor => {
    //   editor
    //   ->Editor.getFileName
    //   ->Option.forEach(fileName => {
    //       editor
    //       ->Editor.save
    //       ->Promise.get(saveSucceed =>
    //           if (saveSucceed && fileName != "") {
    //             editor
    //             ->States.getByEditor
    //             ->Option.forEach(state => {
    //                 state
    //                 ->State.sendRequest(Types.Request.Load(fileName))
    //                 ->Promise.get(Js.log);
    //                 ();
    //               });
    //           }
    //         )
    //     });
    //   // editor->TextEditor.document->TextDocument.save->Promise.flatMap(()=> {
    //   // })
    //   // editor -> TextEditor.
    //   Js.log("[ main ][ reload ]");
    // })
    // ->Editor.addToSubscriptions(context);
  };

  let deactivate = () => {
    States.destroyAll();
  };
};