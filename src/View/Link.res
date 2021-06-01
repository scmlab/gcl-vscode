// open Base;

module Event = {
  type t =
    | MouseOver(SrcLoc.range)
    | MouseOut(SrcLoc.range)
    | MouseClick(SrcLoc.range)

  open Util.Decode
  open Json.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "MouseOver" => Contents(range => MouseOver(SrcLoc.Range.decode(range)))
    | "MouseOut" => Contents(range => MouseOut(SrcLoc.Range.decode(range)))
    | "MouseClick" => Contents(range => MouseClick(SrcLoc.Range.decode(range)))
    | tag => raise(DecodeError("[Link.Event] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | MouseOver(range) =>
      object_(list{("tag", string("MouseOver")), ("contents", SrcLoc.Range.encode(range))})
    | MouseOut(range) => object_(list{("tag", string("MouseOut")), ("contents", SrcLoc.Range.encode(range))})
    | MouseClick(range) =>
      object_(list{("tag", string("MouseClick")), ("contents", SrcLoc.Range.encode(range))})
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
    let link = React.useContext(eventContext)
    let onMouseOver = _ => link->Chan.emit(MouseOver(range))
    let onMouseOut = _ => link->Chan.emit(MouseOut(range))
    let onClick = _ => link->Chan.emit(MouseClick(range))
    <span className="element-link" onMouseOver onMouseOut onClick> children </span>
  }
}
