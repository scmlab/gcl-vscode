open Vscode;
open Belt;

module Impl = (Interface: Editor.Interface) => {
  type t = {
    context: ExtensionContext.t,
    editor: TextEditor.t,
    editor2: Interface.editor,
    mutable connection: option(AgdaMode.Process.t),
    mutable panel: option(WebviewPanel.t),
  };

  let make = (context, editor, editor2) => {
    context,
    editor,
    editor2,
    connection: None,
    panel: None,
  };
};

include Impl(VscodeImpl);

let dispose = state =>
  state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);