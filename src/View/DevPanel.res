// open Belt
open React

@react.component
let make = (~devMode: bool) => {
  let status =
    <div id="gcl-dev-panel-status">
      <i className="codicon codicon-circle-large-filled" /> <span> {string("connected")} </span>
    </div>
  let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
  <div className> <div id="gcl-dev-panel-header"> {string("dev mode")} </div> status  </div>
}
