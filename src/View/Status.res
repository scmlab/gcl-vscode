// open Belt
open React

@react.component
let make = (~status: string, ~onSolve) => {

  let onClick = _ => onSolve();

  <div id="gcl-connection-status"> 
    
    <div id="gcl-connection-status-buttons">
      <button onClick id="gcl-connection-status-buttons-solve">
        <span className="codicon codicon-wand" />
        <span id="gcl-connection-status-buttons-solve-label"> {string("solve")} </span>
      </button>
    </div>
    <div id="gcl-connection-status-version">{string(status)}</div>
  </div>}