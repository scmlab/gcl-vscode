// open Belt
open React

@react.component
let make = (~devMode: bool, ~method: option<Connection.method>) => {
  let method = switch method {
  | Some(ViaTCP(_)) => <>
      <div id="gcl-dev-panel-status" className="connected">
        <i className="codicon codicon-circle-large-filled" />
        <span> {string("connected (tcp)")} </span>
      </div>
    </>
  | Some(ViaStdIO(_)) => <>
      <div id="gcl-dev-panel-status" className="connected">
        <i className="codicon codicon-circle-large-filled" />
        <span> {string("connected (stdio)")} </span>
      </div>
    </>
  | None =>
    <div id="gcl-dev-panel-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  }
  let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
  <div className> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> method </div>
}
