module Impl:
  Editor.Interface with
    type editor = Vscode.TextEditor.t and
    type context = Vscode.ExtensionContext.t = {
  open Vscode;

  type editor = Vscode.TextEditor.t;
  type context = Vscode.ExtensionContext.t;

  type t = {
    editor,
    context,
  };

  let make = (editor, context) => {editor, context};

  let getExtensionPath = (self: t) =>
    self.context->ExtensionContext.extensionPath;

  let getFileName = self =>
    self.editor->TextEditor.document->TextDocument.fileName;

  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

include Impl;