open Belt;

module Conn = Connection.Impl(VscodeImpl);

let load = state => {
  Js.log("load!");
  View.activate(state);
  // Conn.getGCLPath
  // Connection.getGCLPath
  // Connection.VSCODE.getConfiguration("gclPath")->Js.log;
  // looking for GCL
  // AgdaMode.Process.PathSearch.run("gcl")
  // ->Promise.get(
  //     fun
  //     | Error(e) => {
  //         let (header, body) = AgdaMode.Process.PathSearch.Error.toString(e);
  //         state.panel
  //         ->Option.forEach(View.postMessage(_, View.Display(header, body)));
  //       }
  //     | Ok(path) => {
  //         state.panel
  //         ->Option.forEach(View.postMessage(_, View.Display("Path", path)));
  //       },
  //   );
};