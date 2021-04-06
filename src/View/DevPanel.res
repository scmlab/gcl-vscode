// open Belt
open React

@react.component
let make = (~devMode: bool, ~method: option<Connection.method>) =>
  switch method {
  | Some(ViaTCP(_)) => <div id="gcl-connection-status"> {string("TCP")} </div>
  | Some(ViaStdIO(_)) => <div id="gcl-connection-status"/>
  | None =>
    <div id="gcl-connection-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  }
