open Belt
module Inlines = Element__Inlines__Type

module Event = {
  type t = SubstReq(int) | SubstRes(int, Inlines.t)
}

let emitter: Chan.t<Event.t> = Chan.make()
let context = React.createContext(emitter)

module Provider = {
  let makeProps = (~value, ~children, ()) =>
    {
      "value": value,
      "children": children,
    }

  let make = React.Context.provider(context)
}

type status = Unreduced | Reduced(Inlines.t) | Reducing

@react.component
let make = (~makeInline, ~id, ~expr, ~onSubst: option<Trace.t => unit>) => {
  let (status, setStatus) = React.useState(_ => Unreduced)
  let (hoverSubstitutee, setHoverSubstitutee) = React.useState(_ => false)
  let undo = () => setStatus(_ => Unreduced)
  let channel = React.useContext(context)

  React.useEffect1(() => Some(
    channel->Chan.on(event =>
      switch event {
      | SubstReq(_) => ()
      | SubstRes(responseID, result) =>
        // only subsitute when the request/response ID matches
        if responseID == id {
          setStatus(_ => Reduced(result))
        }
      }
    ),
  ), [])

  let onClick = ev => {
    channel->Chan.emit(Event.SubstReq(id))
    setHoverSubstitutee(_ => false)
    setStatus(_ => Reducing)
    onSubst->Option.forEach(onSubst =>
      onSubst({
        undo: undo,
        expr: expr,
      })
    )
    ReactEvent.Mouse.stopPropagation(ev)
  }

  let onMouseOver = ev => {
    setHoverSubstitutee(_ => true)
    ReactEvent.Mouse.stopPropagation(ev)
  }
  let onMouseOut = ev => {
    setHoverSubstitutee(_ => false)
    ReactEvent.Mouse.stopPropagation(ev)
  }

  switch status {
  | Unreduced
  | Reducing =>
    let expr = makeInline(~value=Inlines.Element(expr), ~onSubst)
    let className = hoverSubstitutee ? "element-sbst element-sbst-hovered" : "element-sbst"
    <span className onClick onMouseOver onMouseOut> expr </span>
  | Reduced(result) =>
    let result = makeInline(~value=result, ~onSubst)
    <span className="element-sbst"> result </span>
  }
}
