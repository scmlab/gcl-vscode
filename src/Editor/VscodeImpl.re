module Impl: Editor.Interface with type editor = Vscode.TextEditor.t = {
  open Vscode;

  type editor = Vscode.TextEditor.t;

  let getFileName = editor =>
    editor->TextEditor.document->TextDocument.fileName;

  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

include Impl;