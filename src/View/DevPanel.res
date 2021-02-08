// open Belt
open React

@react.component
let make = (~devMode: bool, ~method: option<Connection.method>) => {
  let status = if Belt.Option.isSome(method) {
    <div id="gcl-dev-panel-status" className="connected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connected")} </span>
    </div>
  } else {
    <div id="gcl-dev-panel-status" className="disconnected">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("disconnected")} </span>
    </div>
  }
  let method = switch method {
  | Some(ViaTCP(_)) =>
    <div id="gcl-dev-panel-method">
      <i className="codicon codicon-plug" /> <span> {string("via TCP")} </span>
    </div>
  | Some(ViaStdIO(_)) =>
    <div id="gcl-dev-panel-method">
      <i className="codicon codicon-plug" /> <span> {string("via StdIO")} </span>
    </div>
  | None => <> </>
  }
  let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
  <div className> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> method status </div>
}
