// let emitter: Chan.t<int => unit> = Chan.make()
let context: React.Context.t<option<int => unit>> = React.createContext(None)

module Provider = {
  let makeProps = (~value, ~children, ()) =>
    {
      "value": value,
      "children": children,
    }

  let make = React.Context.provider(context)
}

open Belt
open Element__Inlines__Type

@react.component
let make = (~makeInline, ~id, ~mapping, ~before, ~after, ~onSubst: option<Trace.t => unit>) => {
  let (substituted, setSubstitute) = React.useState(_ => false)
  let (hoverSubstitutee, setHoverSubstitutee) = React.useState(_ => false)
  let undo = () => setSubstitute(_ => false)
  let chan = React.useContext(context)
  let onClick = ev => {
    chan->Option.forEach(callback => callback(id))

    setHoverSubstitutee(_ => false)
    onSubst->Option.forEach(onSubst =>
      onSubst({
        id: id,
        undo: undo,
        before: before,
        mapping: mapping,
        after: after,
      })
    )
    setSubstitute(_ => true)
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

  if substituted {
    let after = makeInline(~value=Element(after), ~onSubst)
    <span className="element-sbst"> after </span>
  } else {
    let before = makeInline(~value=Element(before), ~onSubst)
    let className = hoverSubstitutee ? "element-sbst element-sbst-hovered" : "element-sbst"
    <span className onClick onMouseOver onMouseOut> before </span>
  }
}
