// open Base;

let emitter: Chan.t<ViewType.Response.linkEvent> = Chan.make()
let eventContext = React.createContext(emitter)

module Provider = {
  let makeProps = (~value, ~children, ()) =>
    {
      "value": value,
      "children": children,
    }

  let make = React.Context.provider(eventContext)
}

@react.component
let make = (~loc, ~children) => {
  let link = React.useContext(eventContext)
  let onMouseOver = _ => link->Chan.emit(MouseOver(loc))
  let onMouseOut = _ => link->Chan.emit(MouseOut(loc))
  let onClick = _ => link->Chan.emit(MouseClick(loc))
  <div className="expr-link" onMouseOver onMouseOut onClick> children </div>
}
