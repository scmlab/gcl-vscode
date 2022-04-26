// open Belt
open React

@react.component
let make = (~status: string) => {
  <div id="gcl-connection-status"> 
    <div id="gcl-connection-status-version">{string(status)}</div>
  </div>}