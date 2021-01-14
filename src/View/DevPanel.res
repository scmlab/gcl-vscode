// open Belt
open React

@react.component
let make = (~devMode: bool, ~status: LSP.Client.status) => {
  let status = switch status {
  | Disconnected => 
    <div id="gcl-dev-panel-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  | Connecting => 
    <div id="gcl-dev-panel-status" className="connecting">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connecting")} </span>
    </div>
  | Connected => 
    <div id="gcl-dev-panel-status" className="connected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connected")} </span>
    </div>
  }
  let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
  <div className> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> status  </div>
}
