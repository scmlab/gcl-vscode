module Impl = (Editor: Sig.Editor) => {
  open AgdaModeVscode.VSCode;
  open Belt;
  module States = Registry.Impl(Editor);
  // module TaskCommand = Task__Command.Impl(Editor);
  module TaskRunner = TaskRunner.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);

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

include Impl(Editor);
