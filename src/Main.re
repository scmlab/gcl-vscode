open Belt;

module Handler = {
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
  let onOpen = doc => {
    let fileName = VSCode.TextDocument.fileName(doc);
    // filter out ".gcl.git" files
    if (isGCL(fileName)) {
      Js.log("GCL " ++ fileName);

      let state = State.make();
      Registry.add(fileName, state);
    };
  };
  let onClose = doc => {
    let fileName = VSCode.TextDocument.fileName(doc);
    Registry.destroy(fileName);
  };
  let onDelete = deleteEvents => {
    VSCode.FileDeleteEvent.files(deleteEvents)
    ->Array.forEach(uri => {
        let fileName = uri->VSCode.Uri.path;
        Registry.destroy(fileName);
      });
  };
  let onRename = renameEvents => {
    VSCode.FileRenameEvent.files(renameEvents)
    ->Array.forEach(event => {
        let oldName = event##oldUri->VSCode.Uri.path;
        let newName = event##newUri->VSCode.Uri.path;
        if (Registry.contains(oldName)) {
          if (isGCL(newName)) {
            Registry.rename(oldName, newName);
          } else {
            Registry.destroy(oldName);
          };
        };
      });
  };
};

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x =>
    x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore;

  // on open
  VSCode.Workspace.onDidOpenTextDocument(. Handler.onOpen)->subscribe;
  VSCode.Workspace.textDocuments->Array.forEach(Handler.onOpen);
  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onClose)->subscribe;
  VSCode.Workspace.onDidDeleteFiles(. Handler.onDelete)->subscribe;
  // on rename
  VSCode.Workspace.onDidRenameFiles(. Handler.onRename);
};

let deactivate = () => {
  ();
};
