// switch (state.State.panel) {
// | None =>
//   // intantiate the panel
//   createPanel(state);
//   moveToBottom() |> ignore;
// | Some(panel) => panel->WebviewPanel.reveal(~preserveFocus=true, ())
// };

// let _postMessage = (panel: WebviewPanel.t, message: message): unit => {
//   panel->WebviewPanel.webview->Webview.postMessage(message) |> ignore;
// };
type message =
  | Display(string, string);