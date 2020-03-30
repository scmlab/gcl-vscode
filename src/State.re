open Vscode;
open Belt;

module Impl = (Interface: Editor.Interface) => {
  type t = {
    context: ExtensionContext.t,
    editor: Interface.editor,
    mutable connection: option(AgdaMode.Process.t),
    mutable panel: option(WebviewPanel.t),
  };

  let make = (context, editor) => {
    context,
    editor,
    connection: None,
    panel: None,
  };

  let dispose = state =>
    state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);
};

include Impl(VscodeImpl);