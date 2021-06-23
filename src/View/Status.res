// open Belt
open React

@react.component
let make = (~method: option<Connection.method>) =>
  switch method {
  | Some(ViaTCP(_)) => <div id="gcl-connection-status"> {string("via TCP from GHCi")} </div>
  | Some(ViaStdIO(_, path)) =>  <div id="gcl-connection-status"> {string("Executable at " ++ path)} </div>
  | Some(ViaPrebuilt(version, _)) => <div id="gcl-connection-status"> {string("Prebuilt " ++ version)} </div>
  | None =>
    <div id="gcl-connection-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  }
