// open Base;

module Event = {
  type t =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc)

  open Util.Decode
  open Json.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "MouseOver" => Contents(loc => MouseOver(GCL.Loc.decode(loc)))
    | "MouseOut" => Contents(loc => MouseOut(GCL.Loc.decode(loc)))
    | "MouseClick" => Contents(loc => MouseClick(GCL.Loc.decode(loc)))
    | tag => raise(DecodeError("[Link.Event] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | MouseOver(loc) =>
      object_(list{("tag", string("MouseOver")), ("contents", GCL.Loc.encode(loc))})
    | MouseOut(loc) => object_(list{("tag", string("MouseOut")), ("contents", GCL.Loc.encode(loc))})
    | MouseClick(loc) =>
      object_(list{("tag", string("MouseClick")), ("contents", GCL.Loc.encode(loc))})
    }
}

let emitter: Chan.t<Event.t> = Chan.make()
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

module WithRange = {
  @react.component
  let make = (~range, ~children) => {
    // TEMP: loc => range
    let loc = switch range {
    | GCL.Range.Range(x, y) => GCL.Loc.Loc(x, y)
    }
    let link = React.useContext(eventContext)
    let onMouseOver = _ => link->Chan.emit(MouseOver(loc))
    let onMouseOut = _ => link->Chan.emit(MouseOut(loc))
    let onClick = _ => link->Chan.emit(MouseClick(loc))
    <div className="expr-link" onMouseOver onMouseOut onClick> children </div>
  }
}
