open Belt;

module Handler = {
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
  let onOpen = (context, doc) => {
    let fileName = VSCode.TextDocument.fileName(doc);
    // filter out ".gcl.git" files
    if (isGCL(fileName)) {
      VSCode.Window.activeTextEditor
      ->Option.map(editor => {
          let state = State.make(editor);

          VSCode.Commands.registerCommand("guacamole.toggle", () => {
            // view initialization
            let extensionPath = context->VSCode.ExtensionContext.extensionPath;

            state.view = Some(View.make(extensionPath, editor));

            state.view
            ->Option.forEach(view => {
                View.send(
                  view,
                  ViewType.Request.Display(
                    Plain("Proof Obligations"),
                    ProofObligations(0, state.pos, [||]),
                  ),
                )
                ->ignore
              });

            Registry.add(fileName, state);
          })
          ->ignore;
        })
      ->ignore;
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

  let onSelect = event => {
    event
    ->VSCode.TextEditorSelectionChangeEvent.selections
    ->Array.forEach(_selection => ());
  };
};

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x =>
    x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore;

  // on open
  VSCode.Workspace.onDidOpenTextDocument(. Handler.onOpen(context))
  ->subscribe;
  VSCode.Workspace.textDocuments->Array.forEach(Handler.onOpen(context));
  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onClose)->subscribe;
  VSCode.Workspace.onDidDeleteFiles(. Handler.onDelete)->subscribe;
  // on rename
  VSCode.Workspace.onDidRenameFiles(. Handler.onRename)->subscribe;
  // on change selection
  VSCode.Window.onDidChangeTextEditorSelection(Handler.onSelect)->subscribe;
};

let deactivate = () => {
  ();
};
