open Belt;
open Vscode;

// a dictionary of FileName-State entries
module StateDict = {
  module Impl = (Editor: Sig.Editor) => {
    module State = State.Impl(Editor);
    module TaskRunner = TaskRunner.Impl(Editor);
    let dict: Js.Dict.t((State.t, TaskRunner.t)) = Js.Dict.empty();

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

    // remove the entry (but without triggering .destroy() )
    let remove = fileName => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      delete_(dict, fileName);
    };
    let destroy = fileName => {
      get(fileName)
      ->Option.mapWithDefault(
          Promise.resolved(),
          ((state, taskRunner)) => {
            Js.log("[ states ][ destroy ]");
            State.destroy(state) |> ignore;
            TaskRunner.destroy(taskRunner);
          },
        )
      ->Promise.get(() => remove(fileName));
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.map(((_, (state, taskRunner))) => {
          Js.log("[ states ][ destroy ]");
          State.destroy(state) |> ignore;
          TaskRunner.destroy(taskRunner);
        })
      ->List.fromArray
      ->Promise.all
      ->Promise.get(_ => ());
    };
  };
};

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

module Impl = (Editor: Sig.Editor) => {
  module States = StateDict.Impl(Editor);
  // module TaskCommand = Task__Command.Impl(Editor);
  module TaskRunner = TaskRunner.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);

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
      prev
      ->Option.flatMap(States.get)
      ->Option.forEach(((state, _)) => State.hide(state));
      next
      ->Option.flatMap(States.get)
      ->Option.forEach(((state, _)) => State.show(state));
    })
    ->Editor.addToSubscriptions(context);

    // on "toggle activate/deactivate"
    Editor.registerCommand("toggle", editor => {
      editor
      ->Editor.getFileName
      ->Option.forEach(fileName => {
          switch (States.get(fileName)) {
          | None =>
            // not in the States dict, instantiate one new
            let state = State.make(context, editor);
            let taskRunner = TaskRunner.make(state);
            // remove it from the States dict if it got destroyed
            state
            ->State.onDestroy(() => {States.remove(fileName)})
            ->Editor.addToSubscriptions(context);
            States.add(fileName, (state, taskRunner));

            state.view
            ->Editor.View.recv(response => {
                TaskRunner.addTask(taskRunner, ViewResponse(response))
              })
            ->Editor.addToSubscriptions(context);

            // dispatch "Load"
            taskRunner->TaskRunner.addTask(DispatchCommand(Load));
          | Some((_state, taskRunner)) =>
            // already in the States dict, dispatch "Quit"
            // and remove and destroy it from the dict
            taskRunner->TaskRunner.addTask(DispatchCommand(Quit));
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
        ->Option.forEach(((_, taskRunner)) =>
            taskRunner->TaskRunner.addTask(DispatchCommand(command))
          )
      })
      ->Editor.addToSubscriptions(context)
    });
  };

  let deactivate = () => {
    States.destroyAll();
  };
};