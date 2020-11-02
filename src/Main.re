open Belt;
open VSCode;

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

let activate = context => {
  let disposables = context->ExtensionContext.subscriptions;
  let extensionPath = context->ExtensionContext.extensionPath;
  // when a TextEditor gets closed, destroy the corresponding State
  Workspace.onDidCloseTextDocument(textDoc =>
    textDoc->Option.forEach(textDoc =>
      Registry.destroy(textDoc->TextDocument.fileName)
    )
  )
  ->Js.Array.push(disposables)
  ->ignore;
  // when a file got renamed, destroy the corresponding State if it becomes non-GCL
  Workspace.onDidRenameFiles(event =>
    event
    ->Option.map(FileRenameEvent.files)
    ->Option.forEach(files => {
        files->Array.forEach(file => {
          let oldName = file##oldUri->Uri.path;
          let newName = file##newUri->Uri.path;
          if (Registry.contains(oldName)) {
            if (isGCL(newName)) {
              Registry.rename(oldName, newName);
            } else {
              Registry.destroy(oldName);
            };
          };
        })
      })
  )
  ->Js.Array.push(disposables)
  ->ignore;

  // on editor activation, reveal the corresponding Panel (if any)
  let previous = ref(Window.activeTextEditor);
  Window.onDidChangeActiveTextEditor(next => {
    let nextFileName =
      next
      ->Option.map(TextEditor.document)
      ->Option.map(TextDocument.fileName);
    let prevFileName =
      (previous^)
      ->Option.map(TextEditor.document)
      ->Option.map(TextDocument.fileName);
    // see if the active text editor changed;
    let changed = nextFileName != prevFileName;
    if (changed) {
      prevFileName
      ->Option.flatMap(Registry.get)
      ->Option.forEach(((state, _)) => State.hide(state));

      nextFileName
      ->Option.flatMap(Registry.get)
      ->Option.forEach(((state, _)) => State.show(state));

      previous := next;
    };
  })
  ->Js.Array.push(disposables)
  ->ignore;

  // on "toggle activate/deactivate"
  let registerCommand = (name, callback) =>
    Commands.registerCommand("guacamole." ++ name, () =>
      Window.activeTextEditor->Option.map(editor => {
        let fileName = editor->TextEditor.document->TextDocument.fileName;
        callback(editor, fileName);
      })
    );

  registerCommand("toggle", (editor, fileName) => {
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
      ->View.recv(response => {
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
    registerCommand(name, (_editor, fileName) => {
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
