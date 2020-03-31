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
            callback(file##oldUri->Uri.path, file##newUri->Uri.path)
          )
        })
    );
  let onDidActivateEditor = callback =>
    Window.onDidChangeActiveTextEditor(editor => {
      editor->Option.map(editorFileName)->Option.forEach(callback)
    });
  // NOOP
  let onDidDeactivateEditor = _callback =>
    Window.onDidChangeActiveTextEditor(_ => ());

  let registerCommand = (name, callback) =>
    Commands.registerCommand("extension." ++ name, callback);

  let getActiveEditor = () => Window.activeTextEditor;
  // Window.activeTextEditor
  // ->Option.map(editor =>
  //   editor->TextEditor.document->TextDocument.fileName
  // );

  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

include Impl;