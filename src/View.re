type message =
  | Display(string, string);

module Impl: Sig.View =
  (Type: Sig.Type, Editor: Sig.Editor) => {
    // open Belt;
    open Vscode;

    module Editor = Editor(Type);

    let make = Editor.createView;
    let destroy = Editor.destroyView;

    // switch (state.State.panel) {
    // | None =>
    //   // intantiate the panel
    //   createPanel(state);
    //   moveToBottom() |> ignore;
    // | Some(panel) => panel->WebviewPanel.reveal(~preserveFocus=true, ())
    // };

    let _postMessage = (panel: WebviewPanel.t, message: message): unit => {
      panel->WebviewPanel.webview->Webview.postMessage(message) |> ignore;
    };
  };