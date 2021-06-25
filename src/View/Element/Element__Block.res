open Belt
module Inlines = Element__Inlines

module Code = {
  open React
  @react.component
  let make = (~value: Inlines.t) => {
    let (trace, setTrace) = useState(_ => ([]: array<Trace.t>))
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
      <Trace.View hidden trace />
    </>
  }
}

type t =
  | Header(string, option<SrcLoc.Range.t>)
  | Paragraph(Inlines.t)
  | Code(Inlines.t)

let decode: Json.Decode.decoder<t> = json => {
  open Json.Decode
  open Util.Decode
  json |> sum(x =>
    switch x {
    | "Header" =>
      Contents(tuple2(string, optional(SrcLoc.Range.decode)) |> map(((a, b)) => Header(a, b)))
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
  | Paragraph(a) => object_(list{("tag", string("Paragraph")), ("contents", a |> Inlines.encode)})
  | Code(a) => object_(list{("tag", string("Code")), ("contents", a |> Inlines.encode)})
  }
}

open React
@react.component
let make = (~value: t) => {
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
  | Paragraph(value) => <p> <Inlines value /> </p>
  | Code(value) => <Code value />
  }
}