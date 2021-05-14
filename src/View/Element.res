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
      | Link(GCL.Range.t, array<t>, ClassNames.t)
      | Sbst(array<t>, array<t>, ClassNames.t)
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
            tuple3(GCL.Range.decode, array(decode()), ClassNames.decode) |> map(((
              r,
              xs,
              cs,
            )) => Link(r, xs, cs)),
          )
        | "Sbst" =>
          Contents(
            tuple3(array(decode()), array(decode()), ClassNames.decode) |> map(((a, b, c)) => Sbst(
              a,
              b,
              c,
            )),
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
          ("contents", (r, s, cs) |> tuple3(GCL.Range.encode, array(encode), ClassNames.encode)),
        })
      | Sbst(a, b, c) =>
        object_(list{
          ("tag", string("Sbst")),
          ("contents", (a, b, c) |> tuple3(array(encode), array(encode), ClassNames.encode)),
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
  //     [Icon("link ", []), Text(GCL.Range.toString(range), [])],
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
          <Link.WithRange range> {child} </Link.WithRange>
        | Sbst(before, after, _className) =>
          let (clicked, setClicked) = React.useState(_ => false)
          let onClick = _ => setClicked(_ => true)
          let before = make(~value=Element(before))
          let after = make(~value=Element(after))
          <span onClick>
            {if clicked {
              after
            } else {
              before
            }}
          </span>
        | Horz(elements) =>
          let children =
            elements->Array.map(element =>
              <span className="element-horz-item"> {make(~value=Element(element))} </span>
            )
          <span className="element-horz" key={string_of_int(i)}> {React.array(children)} </span>
        | Vert(elements) =>
          let children =
            elements->Array.map(element =>
              <span className="element-vert-item"> {make(~value=Element(element))} </span>
            )
          <span className="element-vert" key={string_of_int(i)}> {React.array(children)} </span>
        | Parn(element) => <Parens> {make(~value=Element(element))} </Parens>
        | PrHz(elements) =>
          let children =
            elements->Array.mapWithIndex((index, element) =>
              index == 0
                ? <span className="element-horz-item compact">
                    {make(~value=Element(element))}
                  </span>
                : <span className="element-horz-item"> {make(~value=Element(element))} </span>
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
  let make = (~value: t) => make(~value)
}

module Block = {
  type t =
    | Block(option<string>, option<GCL.Range.t>, Inlines.t)
    | PO(option<string>, option<GCL.Range.t>, Inlines.t, Inlines.t)
    | Header(string)

  let block = (header, range, body) => Block(header, range, body)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = json =>
    json |> sum(x =>
      switch x {
      | "Block" =>
        Contents(
          tuple3(optional(string), optional(GCL.Range.decode), Inlines.decode) |> map(((
            a,
            b,
            c,
          )) => Block(a, b, c)),
        )
      | "PO" =>
        Contents(
          tuple4(
            optional(string),
            optional(GCL.Range.decode),
            Inlines.decode,
            Inlines.decode,
          ) |> map(((a, b, c, d)) => PO(a, b, c, d)),
        )
      | "Header" => Contents(string |> map(s => Header(s)))
      | tag => raise(DecodeError("[Element.Block] Unknown constructor: " ++ tag))
      }
    )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Block(a, b, c) =>
      object_(list{
        ("tag", string("Block")),
        (
          "contents",
          (a, b, c) |> tuple3(nullable(string), nullable(GCL.Range.encode), Inlines.encode),
        ),
      })
    | PO(a, b, c, d) =>
      object_(list{
        ("tag", string("PO")),
        (
          "contents",
          (a, b, c, d) |> tuple4(
            nullable(string),
            nullable(GCL.Range.encode),
            Inlines.encode,
            Inlines.encode,
          ),
        ),
      })
    | Header(xs) => object_(list{("tag", string("Header")), ("contents", xs |> string)})
    }

  open! React
  @react.component
  let make = (~value: t) =>
    switch value {
    | Block(header, None, body) =>
      let header = switch header {
      | None => <> </>
      | Some(header) => <div className="element-block-header"> {string(header)} </div>
      }
      <li className="element-block">
        {header} <div className="element-block-body"> <Inlines value=body /> </div>
      </li>
    | Block(header, Some(range), body) =>
      let header = switch header {
      | None => <> </>
      | Some(header) =>
        <Link.WithRange range>
          <div className="element-block-header">
            {string(header)}
            <span className="element-block-header-range">
              {string(GCL.Range.toString(range))}
            </span>
          </div>
        </Link.WithRange>
      }
      <li className="element-block">
        {header} <div className="element-block-body"> <Inlines value=body /> </div>
      </li>
    | PO(header, None, pre, post) =>
      let header = switch header {
      | None => <> </>
      | Some(header) => <div className="element-block-header"> {string(header)} </div>
      }
      <li className="element-block">
        {header}
        <div className="element-po-body">
          <div className="element-po-pre"> <Inlines value=pre /> </div>
          <span className="element-po-arrow"> {string(j`⇒`)} </span>
          <div className="element-po-post"> <Inlines value=post /> </div>
        </div>
      </li>

    | PO(header, Some(range), pre, post) =>
      let header = switch header {
      | None => <> </>
      | Some(header) =>
        <Link.WithRange range>
          <div className="element-block-header">
            {string(header)}
            <span className="element-block-header-range">
              {string(GCL.Range.toString(range))}
            </span>
          </div>
        </Link.WithRange>
      }
      <li className="element-block">
        {header}
        <div className="element-po-body">
          <div className="element-po-pre"> <Inlines value=pre /> </div>
          <span className="element-po-arrow"> {string(j`⇒`)} </span>
          <div className="element-po-post"> <Inlines value=post /> </div>
        </div>
      </li>
    | Header(header) => <li className="element-header"> <h3> {string(header)} </h3> </li>
    // <h2> {string(header)} </h2>
    }
}
