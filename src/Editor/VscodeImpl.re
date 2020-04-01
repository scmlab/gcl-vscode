module Impl:
  Editor.Interface with
    type editor = Vscode.TextEditor.t and
    type context = Vscode.ExtensionContext.t and
    type disposable = Vscode.Disposable.t = {
  open Vscode;
  open Belt;

  type editor = Vscode.TextEditor.t;
  type context = Vscode.ExtensionContext.t;
  type disposable = Vscode.Disposable.t;
  type fileName = string;

  type t = {
    editor,
    context,
  };

  let make = (editor, context) => {editor, context};

  let getExtensionPath = (self: t) =>
    self.context->ExtensionContext.extensionPath;

  let editorFileName = editor =>
    editor->TextEditor.document->TextDocument.fileName;

  let addToSubscriptions = (disposable, context) =>
    disposable
    ->Js.Array.push(context->ExtensionContext.subscriptions)
    ->ignore;
  // let onOpenEditor = callback => Workspace.onDidRenameFiles(event => ());
  // when the editor got closed
  let onDidCloseEditor = callback =>
    Workspace.onDidCloseTextDocument(textDoc =>
      textDoc->Option.forEach(textDoc =>
        callback(textDoc->TextDocument.fileName)
      )
    );

  let onDidChangeFileName = callback =>
    Workspace.onDidRenameFiles(event =>
      event
      ->Option.map(Vscode.FileRenameEvent.files)
      ->Option.forEach(files => {
          files->Array.forEach(file =>
            callback(
              Some(file##oldUri->Uri.path),
              Some(file##newUri->Uri.path),
            )
          )
        })
    );
  let onDidChangeActivation = callback => {
    let previous = ref(Window.activeTextEditor->Option.map(editorFileName));

    Window.onDidChangeActiveTextEditor(next => {
      let next = next->Option.map(editorFileName);
      if (next != previous^) {
        callback(previous^, next);
        previous := next;
      };
    });
  };

  let registerCommand = (name, callback) =>
    Commands.registerCommand("extension." ++ name, () => {
      Window.activeTextEditor->Option.forEach(callback)
    });

  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

include Impl;