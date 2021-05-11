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
      // | Link(GCL.Range.t, array<t>, ClassNames.t)
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
        // | "Link" =>
        //   Contents(
        //     tuple3(GCL.Range.decode, array(decode()), ClassNames.decode) |> map(((
        //       r,
        //       xs,
        //       cs,
        //     )) => Link(r, xs, cs)),
        //   )
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

      // | Link(r, s, cs) =>
      //   object_(list{
      //     ("tag", string("Link")),
      //     (
      //       "contents",
      //       (r, s, cs) |> tuple3(GCL.Range.encode, array(encode), ClassNames.encode),
      //     ),
      //   })
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
        // | Link(range, children, className) =>
        //   let child = make(~value=Element(children))
        //   <Component__Link
        //     key={string_of_int(i)} className jump=true hover=false target=Common.Link.SrcLoc(range)>
        //     {child}
        //   </Component__Link>
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
    | Unlabeled(Inlines.t, option<string>, option<GCL.Range.t>)
    | Header(string)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = json =>
    json |> sum(x =>
      switch x {
      | "Unlabeled" =>
        Js.log(json)
        Contents(
          tuple3(Inlines.decode, optional(string), optional(GCL.Range.decode)) |> map(((
            a,
            b,
            c,
          )) => Unlabeled(a, b, c)),
        )
      | "Header" => Contents(string |> map(s => Header(s)))
      | tag => raise(DecodeError("[Element.Block] Unknown constructor: " ++ tag))
      }
    )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Unlabeled(a, b, c) =>
      object_(list{
        ("tag", string("Unlabeled")),
        (
          "contents",
          (a, b, c) |> tuple3(Inlines.encode, nullable(string), nullable(GCL.Range.encode)),
        ),
      })
    | Header(xs) => object_(list{("tag", string("Header")), ("contents", xs |> string)})
    }

  open! React
  @react.component
  let make = (~value: t) =>
    switch value {
    | Unlabeled(body, header, None) =>
      <li className="gcl-list-item native-key-bindings" tabIndex={-1}>
        <span className="gcl-list-item-header">
          {header->Option.mapWithDefault("", x => x)->string}
        </span>
        <span className="gcl-list-item-body"> <Inlines value=body /> </span>
      </li>
    | Unlabeled(body, header, Some(range)) =>
      // highlight the header with range on hover
      <li className="gcl-list-item native-key-bindings" tabIndex={-1}>
        <Link.WithRange range>
          <span className="gcl-list-item-header has-range">
            {header->Option.mapWithDefault("", x => x)->string}
            <span className="gcl-list-item-range"> {string(GCL.Range.toString(range))} </span>
          </span>
        </Link.WithRange>
        <span className="gcl-list-item-body"> <Inlines value=body /> </span>
      </li>
    | Header(header) => <h2> {string(header)} </h2>
    }
}
