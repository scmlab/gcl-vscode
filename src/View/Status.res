// open Belt
open React

@react.component
let make = (~status: string) => <div id="gcl-connection-status"> {string(status)} </div>
  // switch method {
  // | Some(method) => <div id="gcl-connection-status"> {string(method)} </div>
  // // | Some(ViaTCP(_)) => <div id="gcl-connection-status"> {string("via TCP from GHCi")} </div>
  // // | Some(ViaStdIO(_, path)) =>  <div id="gcl-connection-status"> {string("Executable at " ++ path)} </div>
  // // | Some(ViaPrebuilt(version, _)) => <div id="gcl-connection-status"> {string("Prebuilt " ++ version)} </div>
  // | None =>
  //   <div id="gcl-connection-status" className="disconnected">
  //     <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
  //   </div>
  // }
