// open Belt
open React

@react.component
let make = (
  ~devMode: bool,
  ~connectViaTCP: bool,
  ~status: LSP.Client.status,
  ~onConnect: unit => unit,
  ~onDisconnect: unit => unit,
  ~onChangeMethod: unit => unit,
) => {
  let onConnect = _ => onConnect()
  let onDisconnect = _ => onDisconnect()
  let onChangeMethod = _ => onChangeMethod()
  let status = switch status {
  | Disconnected =>
    <div id="gcl-dev-panel-status" className="disconnected" onClick=onConnect>
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  | Connecting =>
    <div id="gcl-dev-panel-status" className="connecting">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connecting")} </span>
    </div>
  | Connected =>
    <div id="gcl-dev-panel-status" className="connected" onClick=onDisconnect>
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connected")} </span>
    </div>
  }
  let method = if connectViaTCP {
    <div id="gcl-dev-panel-method" onClick=onChangeMethod>
      <i className="codicon codicon-plug" /> <span> {string("via TCP")} </span>
    </div>
  } else {
    <div id="gcl-dev-panel-method" onClick=onChangeMethod>
      <i className="codicon codicon-plug" /> <span> {string("via STDIO")} </span>
    </div>
  }
  let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
  <div className> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> method status </div>
}
