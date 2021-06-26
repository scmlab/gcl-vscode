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

module Sbst = {
  @react.component
  let make = (~makeInline, ~before, ~mapping, ~after, ~onSubst: option<Trace.t => unit>) => {
    let (substituted, setSubstitute) = React.useState(_ => false)
    let undo = () => setSubstitute(_ => false)

    let onClick = _ => {
      onSubst->Option.forEach(onSubst =>
        onSubst({
          undo: undo,
          before: before,
          mapping: mapping,
          after: after,
        })
      )
      setSubstitute(_ => true)
    }
    if substituted {
      let after = makeInline(~value=Element(after), ~onSubst)
      <span className="element-sbst"> after </span>
    } else {
      let before = makeInline(~value=Element(before), ~onSubst)
      if Js.Array.length(mapping) > 0 {
        // "mapping" is not empty
        let env = makeInline(~value=Element(mapping), ~onSubst)
        <span className="element-sbst">
          before {React.string(" ")} <span className="element-sbst-env" onClick> env </span>
          // before {React.string(" ")} <span className="element-sbst-env" onClick> env </span>
        </span>
      } else {
        // "mapping" is empty, make "before" clickable
        <span className="element-sbst-env" onClick> before </span>
      }
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
      | Link(range, children, _className) =>
        let child = make(~value=Element(children), ~onSubst)
        <Link range key={string_of_int(i)}> {child} </Link>
      | Sbst(before, mapping, after, _className) =>
        <Sbst makeInline=make key={string_of_int(i)} before mapping after onSubst />
      | Sbst2(_reason, before, mapping, after, _className) =>
        <Sbst makeInline=make key={string_of_int(i)} before mapping after onSubst />
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
