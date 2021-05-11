// open Belt
open React

@react.component
let make = (~method: option<Connection.method>) => {
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
  | Some(ViaPrebuilt(version, _)) => <>
      <div id="gcl-dev-panel-status" className="connected">
        <i className="codicon codicon-circle-large-filled" />
        <span> {string(version)} </span>
      </div>
    </>
  | None =>
    <div id="gcl-dev-panel-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  }
  <div className="gcl-dev-panel"> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> method </div>
}
