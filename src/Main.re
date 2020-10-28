module Impl = (Editor: API.Editor) => {
  open Belt;
  module Registry = Registry.Impl(Editor);
  // module TaskCommand = Task__Command.Impl(Editor);
  module TaskRunner = TaskRunner.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);

  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

  let activate = context => {
    let disposables = Editor.getDisposables(context);
    let extensionPath = Editor.getExtensionPath(context);
    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(Registry.destroy)
    ->Js.Array.push(disposables)
    ->ignore;
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (Registry.contains(oldName)) {
            if (isGCL(newName)) {
              Registry.rename(oldName, newName);
            } else {
              Registry.destroy(oldName);
            };
          }
        )
      )
    )
    ->Js.Array.push(disposables)
    ->ignore;

    // on editor activation, reveal the corresponding Panel (if any)
    Editor.onDidChangeActivation((prev, next) => {
      prev
      ->Option.flatMap(Registry.getByEditor)
      ->Option.forEach(((state, _)) => State.hide(state));
      next
      ->Option.flatMap(Registry.getByEditor)
      ->Option.forEach(((state, _)) => State.show(state));
    })
    ->Js.Array.push(disposables)
    ->ignore;

    // on "toggle activate/deactivate"
    Editor.registerCommand("toggle", (editor, fileName) => {
      switch (Registry.get(fileName)) {
      | None =>
        // not in the Registry, instantiate one new
        let state = State.make(extensionPath, editor);
        let taskRunner = TaskRunner.make(state);
        // remove it from the Registry if it got destroyed
        state
        ->State.onDestroy(() => {Registry.remove(fileName)})
        ->Js.Array.push(disposables)
        ->ignore;
        Registry.add(fileName, (state, taskRunner));

        state.view
        ->Editor.View.recv(response => {
            TaskRunner.addTask(taskRunner, ViewResponse(response))
          })
        ->Js.Array.push(disposables)
        ->ignore;

        // dispatch "Load"
        taskRunner->TaskRunner.addTask(DispatchCommand(Load));
      | Some((_state, taskRunner)) =>
        // already in the Registry, dispatch "Quit"
        // and remove and destroy it from the Registry
        taskRunner->TaskRunner.addTask(DispatchCommand(Quit));
        fileName->Registry.destroy;
      }
    })
    ->Js.Array.push(disposables)
    ->ignore;

    // on other commands
    Command.names->Array.forEach(((command, name)) => {
      Editor.registerCommand(name, (_editor, fileName) => {
        fileName
        ->Registry.get
        ->Option.forEach(((_, taskRunner)) =>
            taskRunner->TaskRunner.addTask(DispatchCommand(command))
          )
      })
      ->Js.Array.push(disposables)
      ->ignore
    });
  };

  let deactivate = () => {
    Registry.destroyAll();
  };
};
