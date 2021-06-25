open Belt

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

module ClassNames = {
  type t = array<string>

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    array(string)
  }

  let encode: Json.Encode.encoder<t> = {
    open Json.Encode
    array(string)
  }
}

module RewriteReason = {
  type t = Assigment

  let decode: Json.Decode.decoder<t> = {
    // open Json.Decode
    _ => Assigment
  }

  let encode: Json.Encode.encoder<t> = {
    open Json.Encode
    _ => string("RRAssigment")
  }
}

module Inline = {
  type rec t =
    | Icon(string, ClassNames.t)
    | Text(string, ClassNames.t)
    | Link(SrcLoc.Range.t, array<t>, ClassNames.t)
    | Sbst(array<t>, array<t>, array<t>, ClassNames.t)
    | Sbst2(RewriteReason.t, array<t>, array<t>, array<t>, ClassNames.t)
    | Horz(array<array<t>>)
    | Vert(array<array<t>>)
    | Parn(array<t>)
    // refactor PrHz
    | PrHz(array<array<t>>)

  let rec decode: unit => Json.Decode.decoder<t> = () => {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "Icon" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Icon(s, cs)))
      | "Text" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Text(s, cs)))
      | "Link" =>
        Contents(
          tuple3(SrcLoc.Range.decode, array(decode()), ClassNames.decode) |> map(((
            r,
            xs,
            cs,
          )) => Link(r, xs, cs)),
        )
      | "Sbst" =>
        Contents(
          tuple4(array(decode()), array(decode()), array(decode()), ClassNames.decode) |> map(((
            a,
            b,
            c,
            d,
          )) => Sbst(a, b, c, d)),
        )
      | "Sbst2" =>
        Contents(
          tuple5(
            RewriteReason.decode,
            array(decode()),
            array(decode()),
            array(decode()),
            ClassNames.decode,
          ) |> map(((a, b, c, d, e)) => Sbst2(a, b, c, d, e)),
        )
      | "Horz" => Contents(array(array(decode())) |> map(xs => Horz(xs)))
      | "Vert" => Contents(array(array(decode())) |> map(xs => Vert(xs)))
      | "Parn" => Contents(array(decode()) |> map(x => Parn(x)))
      | "PrHz" => Contents(array(array(decode())) |> map(xs => PrHz(xs)))
      | tag => raise(DecodeError("[Element.Inline] Unknown constructor: " ++ tag))
      }
    )
  }
  let decode = decode()

  let rec encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Icon(s, cs) =>
      object_(list{
        ("tag", string("Icon")),
        ("contents", (s, cs) |> pair(string, ClassNames.encode)),
      })

    | Text(s, cs) =>
      object_(list{
        ("tag", string("Text")),
        ("contents", (s, cs) |> pair(string, ClassNames.encode)),
      })
    | Link(r, s, cs) =>
      object_(list{
        ("tag", string("Link")),
        ("contents", (r, s, cs) |> tuple3(SrcLoc.Range.encode, array(encode), ClassNames.encode)),
      })
    | Sbst(a, b, c, d) =>
      object_(list{
        ("tag", string("Sbst")),
        (
          "contents",
          (a, b, c, d) |> tuple4(array(encode), array(encode), array(encode), ClassNames.encode),
        ),
      })
    | Sbst2(a, b, c, d, e) =>
      object_(list{
        ("tag", string("Sbst2")),
        (
          "contents",
          (a, b, c, d, e) |> Util.Encode.tuple5(
            RewriteReason.encode,
            array(encode),
            array(encode),
            array(encode),
            ClassNames.encode,
          ),
        ),
      })
    | Horz(xs) => object_(list{("tag", string("Horz")), ("contents", xs |> array(array(encode)))})
    | Vert(xs) => object_(list{("tag", string("Vert")), ("contents", xs |> array(array(encode)))})
    | Parn(x) => object_(list{("tag", string("Parn")), ("contents", x |> array(encode))})
    | PrHz(xs) => object_(list{("tag", string("PrHz")), ("contents", xs |> array(array(encode)))})
    }
  }
}

type t = Element(array<Inline.t>)

let empty = Element([])
let string = s => Element([Text(s, [])])

let concatMany = xs => Element(
  xs
  ->Array.map(x =>
    switch x {
    | Element(xs) => xs
    }
  )
  ->Array.concatMany,
)

module Sbst = {
  @react.component
  let make = (~makeInline, ~before, ~env, ~after, ~reason=?) => {
    let (substituted, setSubstitute) = React.useState(_ => false)
    let undo = () => setSubstitute(_ => false)

    let history = React.useContext(History.Context.context)

    let onClick = _ => {
      reason->Option.forEach(Js.log2("REASON: "))
      history->History.push(undo)
      setSubstitute(_ => true)
    }
    if substituted {
      let after = makeInline(~value=Element(after))
      <span className="element-sbst"> after </span>
    } else {
      let before = makeInline(~value=Element(before))
      // render ENV only if it is non-empty
      if Js.Array.length(env) > 0 {
        let env = makeInline(~value=Element(env))
        <span className="element-sbst">
          before {React.string(" ")} <span className="element-sbst-env" onClick> env </span>
        </span>
      } else {
        <span className="element-sbst"> before </span>
      }
    }
  }
}

let rec make = (~value: t) => {
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
        let child = make(~value=Element(children))
        <Link range key={string_of_int(i)}> {child} </Link>
      | Sbst(before, env, after, _className) =>
        <Sbst makeInline=make key={string_of_int(i)} before env after />
      | Sbst2(reason, before, env, after, _className) =>
        <Sbst makeInline=make key={string_of_int(i)} reason before env after />
      | Horz(elements) =>
        let children =
          elements->Array.mapWithIndex((j, element) =>
            <span className="element-horz-item" key={string_of_int(j)}>
              {make(~value=Element(element))}
            </span>
          )
        <span className="element-horz" key={string_of_int(i)}> {React.array(children)} </span>
      | Vert(elements) =>
        let children =
          elements->Array.mapWithIndex((j, element) =>
            <span className="element-vert-item" key={string_of_int(j)}>
              {make(~value=Element(element))}
            </span>
          )
        <span className="element-vert" key={string_of_int(i)}> {React.array(children)} </span>
      | Parn(element) =>
        <Parens key={string_of_int(i)}> {make(~value=Element(element))} </Parens>
      | PrHz(elements) =>
        let children =
          elements->Array.mapWithIndex((index, element) =>
            index == 0
              ? <span className="element-horz-item compact" key={string_of_int(index)}>
                  {make(~value=Element(element))}
                </span>
              : <span className="element-horz-item" key={string_of_int(index)}>
                  {make(~value=Element(element))}
                </span>
          )
        <Parens2 key={string_of_int(i)} payload=children />
      }
    })
    ->React.array}
  </span>
}

let decode: Json.Decode.decoder<t> = {
  open Json.Decode
  array(Inline.decode) |> map(elems => Element(elems))
}

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  switch x {
  | Element(elemss) => elemss |> array(Inline.encode)
  }
}

@react.component
let make = (~value: t) => make(~value)
