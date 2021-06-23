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

module type Module = {
  type t
  // constructors
  let text: string => t
  let concatMany: array<t> => t

  // React component
  let make: (~value: t) => React.element

  // JSON encoding/decoding
  let decode: Js.Json.t => t
  let encode: t => Js.Json.t
}

module Inlines = {
  module ClassNames = {
    type t = array<string>

    open! Json.Decode
    let decode: decoder<t> = array(string)

    open! Json.Encode
    let encode: encoder<t> = array(string)
  }

  module Inline = {
    type rec t =
      | Icon(string, ClassNames.t)
      | Text(string, ClassNames.t)
      | Link(SrcLoc.Range.t, array<t>, ClassNames.t)
      | Sbst(array<t>, array<t>, array<t>, ClassNames.t)
      | Horz(array<array<t>>)
      | Vert(array<array<t>>)
      | Parn(array<t>)
      // refactor PrHz
      | PrHz(array<array<t>>)

    open! Json.Decode
    open Util.Decode
    let rec decode: unit => decoder<t> = () =>
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
        | "Horz" => Contents(array(array(decode())) |> map(xs => Horz(xs)))
        | "Vert" => Contents(array(array(decode())) |> map(xs => Vert(xs)))
        | "Parn" => Contents(array(decode()) |> map(x => Parn(x)))
        | "PrHz" => Contents(array(array(decode())) |> map(xs => PrHz(xs)))
        | tag => raise(DecodeError("[Element.Inline] Unknown constructor: " ++ tag))
        }
      )
    let decode = decode()

    open! Json.Encode
    let rec encode: encoder<t> = x =>
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
      | Horz(xs) => object_(list{("tag", string("Horz")), ("contents", xs |> array(array(encode)))})
      | Vert(xs) => object_(list{("tag", string("Vert")), ("contents", xs |> array(array(encode)))})
      | Parn(x) => object_(list{("tag", string("Parn")), ("contents", x |> array(encode))})
      | PrHz(xs) => object_(list{("tag", string("PrHz")), ("contents", xs |> array(array(encode)))})
      }
  }
  type t = Element(array<Inline.t>)

  let empty = Element([])
  let string = s => Element([Text(s, [])])
  // let srcLoc = range => Element([
  //   Link(
  //     range,
  //     [Icon("link ", []), Text(SrcLoc.Range.toString(range), [])],
  //     ["element-link element-hole"],
  //   ),
  // ])

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
    let make = (~before, ~env, ~after, ~history) => {
      let (clicked, setClicked) = React.useState(_ => false)
      let undo = () => setClicked(_ => false)
      let onClick = _ => {
        history->History.push(undo)
        setClicked(_ => true)
      }
      if clicked {
        <span className="element-sbst"> after </span>
      } else {
        <span className="element-sbst">
          before {React.string(" ")} <span className="element-sbst-env" onClick> env </span>
        </span>
      }
    }
  }

  let rec make = (~value: t, ~history: History.t) => {
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
          let child = make(~value=Element(children), ~history)
          <Link range> {child} </Link>
        | Sbst(before, env, after, _className) =>
          let before = make(~value=Element(before), ~history)
          let env = make(~value=Element(env), ~history)
          let after = make(~value=Element(after), ~history)
          <Sbst before env after history />
        | Horz(elements) =>
          let children =
            elements->Array.map(element =>
              <span className="element-horz-item"> {make(~value=Element(element), ~history)} </span>
            )
          <span className="element-horz" key={string_of_int(i)}> {React.array(children)} </span>
        | Vert(elements) =>
          let children =
            elements->Array.map(element =>
              <span className="element-vert-item"> {make(~value=Element(element), ~history)} </span>
            )
          <span className="element-vert" key={string_of_int(i)}> {React.array(children)} </span>
        | Parn(element) => <Parens> {make(~value=Element(element), ~history)} </Parens>
        | PrHz(elements) =>
          let children =
            elements->Array.mapWithIndex((index, element) =>
              index == 0
                ? <span className="element-horz-item compact">
                    {make(~value=Element(element), ~history)}
                  </span>
                : <span className="element-horz-item">
                    {make(~value=Element(element), ~history)}
                  </span>
            )
          <Parens2 payload=children />
        }
      })
      ->React.array}
    </span>
  }

  open! Json.Decode
  let decode: decoder<t> = array(Inline.decode) |> map(elems => Element(elems))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Element(elemss) => elemss |> array(Inline.encode)
    }

  @react.component
  let make = (~value: t, ~history: History.t) => make(~value, ~history)
}

module Block = {
  type t =
    | Header(string, option<SrcLoc.Range.t>)
    | Paragraph(Inlines.t)
    | Code(Inlines.t)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = json =>
    json |> sum(x =>
      switch x {
      | "Header" =>
        Contents(tuple2(string, optional(SrcLoc.Range.decode)) |> map(((a, b)) => Header(a, b)))
      | "Paragraph" => Contents(Inlines.decode |> map(a => Paragraph(a)))
      | "Code" => Contents(Inlines.decode |> map(a => Code(a)))
      | tag => raise(DecodeError("[Element.Block] Unknown constructor: " ++ tag))
      }
    )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Header(a, b) =>
      object_(list{
        ("tag", string("Header")),
        ("contents", (a, b) |> tuple2(string, nullable(SrcLoc.Range.encode))),
      })
    | Paragraph(a) => object_(list{("tag", string("Paragraph")), ("contents", a |> Inlines.encode)})
    | Code(a) => object_(list{("tag", string("Code")), ("contents", a |> Inlines.encode)})
    }

  open! React
  @react.component
  let make = (~value: t) => {
    let history = History.make()
    switch value {
    | Header(header, range) =>
      switch range {
      | None => <header> {string(header)} </header>
      | Some(range) =>
        <Link range>
          <header>
            {string(header)}
            <span className="element-block-header-range">
              {string(SrcLoc.Range.toString(range))}
            </span>
          </header>
        </Link>
      }
    | Paragraph(value) => <p> <Inlines value history /> </p>
    | Code(value) =>
      <pre>
        <div className="element-block-code-buttons">
          <button
            onClick={_ => history->History.pop} className="codicon codicon-debug-step-back"
          />
        </div>
        <Inlines value history />
      </pre>
    }
  }
}

module Section = {
  module Deco = {
    type t = Plain | Red | Yellow | Green | Blue

    let toClassName = x =>
      switch x {
      | Plain => ""
      | Red => "element-deco-red"
      | Yellow => "element-deco-yellow"
      | Green => "element-deco-green"
      | Blue => "element-deco-blue"
      }

    open Json.Decode
    let decode: decoder<t> = string |> map(tag =>
      switch tag {
      | "Red" => Red
      | "Yellow" => Yellow
      | "Green" => Green
      | "Blue" => Blue
      | _ => Plain
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Plain => string("Plain")
      | Red => string("Red")
      | Yellow => string("Yellow")
      | Green => string("Green")
      | Blue => string("Blue")
      }
  }

  type t = {
    deco: Deco.t,
    blocks: array<Block.t>,
  }

  open Json.Decode
  let decode: decoder<t> = json => {
    deco: json |> field("sectionDeco", Deco.decode),
    blocks: json |> field("sectionBlocks", array(Block.decode)),
  }

  open! Json.Encode
  let encode: encoder<t> = x =>
    object_(list{
      ("sectionDeco", x.deco |> Deco.encode),
      ("sectionBlocks", x.blocks |> array(Block.encode)),
    })

  open! React
  @react.component
  let make = (~value: t) => {
    let className = "element-section " ++ Deco.toClassName(value.deco)
    let blocks =
      value.blocks
      ->Array.mapWithIndex((i, value) => {
        <Block value key={string_of_int(i)} />
      })
      ->array
    <li className> {blocks} </li>
  }
}
