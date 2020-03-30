open Vscode;
open Belt;

module Impl = (Interface: Editor.Interface) => {
  type t = {
    editor: Interface.editor,
    editor2: Interface.t,
    mutable connection: option(AgdaMode.Process.t),
    mutable panel: option(WebviewPanel.t),
  };

  let make = (context, editor) => {
    editor,
    editor2: Interface.make(editor, context),
    connection: None,
    panel: None,
  };

  let dispose = state =>
    state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);
};

include Impl(VscodeImpl);