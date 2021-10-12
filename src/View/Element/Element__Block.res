open Belt
module Inlines = Element__Inlines

module Code = {
  open React
  @react.component
  let make = (~value: Inlines.t) => {
    let (trace, setTrace) = useState((_): array<Trace.t> => [])
    let (hidden, setHidden) = useState(_ => true)

    let stepBack = _ => {
      setTrace(trace => {
        let popped = Js.Array2.slice(trace, ~start=0, ~end_=Js.Array2.length(trace) - 1)
        Js.Array2.pop(trace)->Option.forEach((step: Trace.t) => {
          step.undo()
        })
        popped
      })
    }
    let toggleView = _ => {
      setHidden(not)
    }

    let onSubst = step => {
      Js.log2("[view] onSubst", step.Trace.id)
      setTrace(trace => {
        Js.Array2.concat(trace, [step])
      })
    }

    <>
      <pre>
        <div className="element-block-code-buttons">
          <button onClick=stepBack className="codicon codicon-debug-step-back" />
          <button onClick=toggleView className="codicon codicon-debug-rerun" />
        </div>
        <Inlines value onSubst />
      </pre>
      <TraceView hidden trace />
    </>
  }
}

type t =
  | Header(string, option<SrcLoc.Range.t>)
  | HeaderWithButtons(string, option<SrcLoc.Range.t>, string, option<SrcLoc.Range.t>)
  | Paragraph(Inlines.t)
  | Code(Inlines.t)

let decode: Json.Decode.decoder<t> = json => {
  open Json.Decode
  open Util.Decode
  json |> sum(x =>
    switch x {
    | "Header" =>
      Contents(tuple2(string, optional(SrcLoc.Range.decode)) |> map(((a, b)) => Header(a, b)))
    | "HeaderWithButtons" =>
      Contents(
        tuple4(
          string,
          optional(SrcLoc.Range.decode),
          string,
          optional(SrcLoc.Range.decode),
        ) |> map(((a, b, c, d)) => HeaderWithButtons(a, b, c, d)),
      )
    | "Paragraph" => Contents(Inlines.decode |> map(a => Paragraph(a)))
    | "Code" => Contents(Inlines.decode |> map(a => Code(a)))
    | tag => raise(DecodeError("[Element.Block] Unknown constructor: " ++ tag))
    }
  )
}

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  switch x {
  | Header(a, b) =>
    object_(list{
      ("tag", string("Header")),
      ("contents", (a, b) |> tuple2(string, nullable(SrcLoc.Range.encode))),
    })
  | HeaderWithButtons(a, b, c, d) =>
    object_(list{
      ("tag", string("HeaderWithButtons")),
      (
        "contents",
        (a, b, c, d) |> tuple4(
          string,
          nullable(SrcLoc.Range.encode),
          string,
          nullable(SrcLoc.Range.encode),
        ),
      ),
    })
  | Paragraph(a) => object_(list{("tag", string("Paragraph")), ("contents", a |> Inlines.encode)})
  | Code(a) => object_(list{("tag", string("Code")), ("contents", a |> Inlines.encode)})
  }
}

open React
@react.component
let make = (~value: t, ~onInsertAnchor: string => unit, ~onDisplayExplanation: bool => unit) => {
  // for `HeaderWithButtons`
  let (headerClicked, setHeaderClicked) = useState(_ => false)

  switch value {
  | Header(header, range) =>
    switch range {
    | None => <header> {string(header)} </header>
    | Some(range) =>
      <Link range>
        <header>
          {string(header)}
          <span className="element-block-header-range">
            {string("at " ++ SrcLoc.Range.toString(range))}
          </span>
        </header>
      </Link>
    }
  | HeaderWithButtons(header, range, hash, anchor) =>
    let header = {
      let range = switch range {
      | None => <> </>
      | Some(range) =>
        <Link range title="location of proof obligation">
          <span className="element-block-header-range">
            {string("at " ++ SrcLoc.Range.toString(range))}
          </span>
        </Link>
      }
      // toggle on clicked
      let onClick = _ => {
        onDisplayExplanation(!headerClicked)
        setHeaderClicked(x => !x)
      }
      let className = headerClicked
        ? "element-block-header-text toggled"
        : "element-block-header-text"

      <> <span className onClick> {string(header)} </span> {range} </>
    }
    let anchor = {
      // crop the hash value and display only the first 7 characters
      let croppedHash = "#" ++ Js.String2.slice(hash, ~from=0, ~to_=7)
      // see if the anchor range is available
      switch anchor {
      // insert anchor when the range is not available
      | None =>
        <span
          className="element-block-anchor-range"
          title="click to create a corresponding proof"
          onClick={_ => onInsertAnchor(hash)}>
          {string(croppedHash)}
        </span>
      | Some(range) =>
        <Link range title="location of the corresponding proof">
          <span className="element-block-anchor-range linked"> {string(croppedHash)} </span>
        </Link>
      }
    }

    <header> {header} {anchor} </header>
  | Paragraph(value) => <p> <Inlines value /> </p>
  | Code(value) => <Code value />
  }
}
