open Belt

open Element__Inlines__Type

module Parens = {
  @react.component
  let make = (~children) => {
    let (activated, setActivated) = React.useState(_ => false)
    let (contracted, setContracted) = React.useState(_ => false)
    let className = activated ? "element-parentheses activated" : "element-parentheses"
    let onMouseOver = _ => setActivated(_ => true)
    let onMouseOut = _ => setActivated(_ => false)
    let onClick = _ => setContracted(x => !x)
    <>
      <span className onMouseOver onMouseOut onClick> {React.string("(")} </span>
      {contracted ? {React.string("..")} : children}
      <span className onMouseOver onMouseOut onClick> {React.string(")")} </span>
    </>
  }
}

// TODO: for PrHz, refactor this
module Parens2 = {
  @react.component
  let make = (~payload) => {
    // states
    let (activated, setActivated) = React.useState(_ => false)
    let (contracted, setContracted) = React.useState(_ => false)
    // event handlers
    let onMouseOver = _ => setActivated(_ => true)
    let onMouseOut = _ => setActivated(_ => false)
    let onClick = _ => setContracted(x => !x)
    // the opening parenthesis
    let openParenClassName =
      "element-horz-item element-parentheses" ++ (activated ? " activated" : "")
    let openParen =
      <span className=openParenClassName onMouseOver onMouseOut onClick> {React.string("(")} </span>
    // the closing parenthesis
    let closeParenClassName =
      "element-horz-item element-parentheses compact" ++ (activated ? " activated" : "")
    let closeParen =
      <span className=closeParenClassName onMouseOver onMouseOut onClick>
        {React.string(")")}
      </span>

    // display only "(..)" when its contracted
    if contracted {
      <span className="element-horz">
        <span className=openParenClassName onMouseOver onMouseOut onClick>
          {React.string("(..)")}
        </span>
      </span>
    } else {
      let children = Array.concatMany([[openParen], payload, [closeParen]])
      <span className="element-horz"> {React.array(children)} </span>
    }
  }
}

module Expn = {
  @react.component
  let make = (~makeInline, ~mapping, ~before, ~after, ~onSubst: option<Trace.t => unit>) => {
    let (substituted, setSubstitute) = React.useState(_ => false)
    let (hoverSubstitutee, setHoverSubstitutee) = React.useState(_ => false)
    let undo = () => setSubstitute(_ => false)

    let onClick = ev => {
      setHoverSubstitutee(_ => false)
      onSubst->Option.forEach(onSubst =>
        onSubst({
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

      let mapping =
        Js.Array.length(mapping) > 0
          ? <> {React.string(" ")} {makeInline(~value=Element(mapping), ~onSubst)} </>
          : makeInline(~value=Element(mapping), ~onSubst)

      let className = hoverSubstitutee ? "element-sbst element-sbst-hovered" : "element-sbst"

      <span className onClick onMouseOver onMouseOut> before {mapping} </span>
      //   let mapping = makeInline(~value=Element(mapping), ~onSubst)
      //   <span className="element-sbst">
      //     <span className="element-sbst-button" onClick onMouseOver onMouseOut> before {React.string(" ")}  mapping </span>
      //   </span>
      // } else {
      //   <span className="element-sbst">
      //     <span className="element-sbst-button" onClick onMouseOver onMouseOut> before </span>
      //   </span>

      // if hoverSubstitutee {
      //   <span className="element-sbst">
      //     <span className="element-sbst-hovered"> before </span>
      //     <span className="element-sbst-button" onMouseOver onMouseOut onClick>
      //       {React.string(" ")}
      //       // {React.string(j`⇊`)}
      //     </span>
      //   </span>
      // } else {
      //   <span className="element-sbst">
      //     <span> before </span>
      //     <span className="element-sbst-button" onMouseOver onMouseOut onClick>
      //       {React.string(" ")}
      //     </span>
      //   </span>
      // }
    }
  }
}

let rec make = (~value: t, ~onSubst: option<Trace.t => unit>) => {
  let Element(elements) = value
  <span>
    {elements
    ->Array.mapWithIndex((i, x) => {
      switch x {
      | Text(text, className) =>
        let className = {String.concat(" ", List.fromArray(className))}
        <span className key={string_of_int(i)}> {React.string(text)} </span>
      | Icon(kind, className) =>
        let className = Array.concat(["codicon", "codicon-" ++ kind], className)
        let className = {String.concat(" ", List.fromArray(className))}
        <div className key={string_of_int(i)} />
      | Code(code) =>
        let className = "element-inline-code"
        let code = make(~value=Element(code), ~onSubst)
        <span className key={string_of_int(i)}> {code} </span>
      | Link(range, children, _className) =>
        let child = make(~value=Element(children), ~onSubst)
        <Link range key={string_of_int(i)}> {child} </Link>
      | Expn(before, mapping, after) =>
        <Expn makeInline=make key={string_of_int(i)} before mapping after onSubst />
      | Horz(elements) =>
        let children =
          elements->Array.mapWithIndex((j, element) =>
            <span className="element-horz-item" key={string_of_int(j)}>
              {make(~value=Element(element), ~onSubst)}
            </span>
          )
        <span className="element-horz" key={string_of_int(i)}> {React.array(children)} </span>
      | Vert(elements) =>
        let children =
          elements->Array.mapWithIndex((j, element) =>
            <span className="element-vert-item" key={string_of_int(j)}>
              {make(~value=Element(element), ~onSubst)}
            </span>
          )
        <span className="element-vert" key={string_of_int(i)}> {React.array(children)} </span>
      | Parn(element) =>
        <Parens key={string_of_int(i)}> {make(~value=Element(element), ~onSubst)} </Parens>
      | PrHz(elements) =>
        let children =
          elements->Array.mapWithIndex((index, element) =>
            index == 0
              ? <span className="element-horz-item compact" key={string_of_int(index)}>
                  {make(~value=Element(element), ~onSubst)}
                </span>
              : <span className="element-horz-item" key={string_of_int(index)}>
                  {make(~value=Element(element), ~onSubst)}
                </span>
          )
        <Parens2 key={string_of_int(i)} payload=children />
      }
    })
    ->React.array}
  </span>
}

@react.component
let make = (~value: t, ~onSubst: option<Trace.t => unit>=?) => make(~value, ~onSubst)
