// signature for the View
module type Sig =
  (Editor: Editor.Sig) =>
   {
    // construction/destruction
    let make: (Editor.context, Editor.editor) => Editor.view;
    let destroy: Editor.view => unit;
  };

type message =
  | Display(string, string);

module Impl: Sig =
  (Editor: Editor.Sig) => {
    // open Belt;
    open Vscode;

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