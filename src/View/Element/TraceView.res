open Element__Inlines
module Inlines = Element__Inlines

@react.component
let make = (~hidden: bool, ~trace: array<Trace.t>) => {
  let className = "element-block-code-trace" ++ (hidden ? " hidden" : "")

  let traceView =
    trace
    ->Belt.Array.mapWithIndex((i, {expr}) => {
      open Inline
      let line = Belt.Array.concatMany([[Text("---------", [])]])
      let expr = Belt.Array.concatMany([[Text("expr: ", [])], expr])

      let inlines = [Vert([line, expr])]

      <Inlines value=Element(inlines) key={string_of_int(i)} />
    })
    ->React.array

  <div className>
    {React.string(
      "DEBUG: displaying the whole history (" ++ string_of_int(Js.Array.length(trace)) ++ " steps)",
    )}
    {traceView}
  </div>
}
