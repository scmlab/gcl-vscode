open Vscode;
open Belt;

// module Error = {
//   type t =
//     | Connection(Connection.Error.t)
//     | Decode(string, Js.Json.t);

//   let toString =
//     fun
//     | Connection(e) => Connection.Error.toString(e)
//     | Decode(msg, json) => (
//         {js|JSON Decode Error|js},
//         msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
//       );
// };

type t = {
  context: ExtensionContext.t,
  editor: TextEditor.t,
  mutable connection: option(AgdaMode.Process.t),
  mutable panel: option(WebviewPanel.t),
};

let make = (context, editor) => {
  context,
  editor,
  connection: None,
  panel: None,
};

// let getOrMakeConnection = () =>
//   // looking for GCL
//   AgdaMode.Process.PathSearch.run("gcl")
//   ->Promise.flatMapOk(path => {
// AgdaMode.Process.
//   })

let dispose = state =>
  state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);