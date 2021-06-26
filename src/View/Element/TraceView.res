open Element__Inlines
module Inlines = Element__Inlines

@react.component
let make = (~hidden: bool, ~trace: array<Trace.t>) => {
  let className = "element-block-code-trace" ++ (hidden ? " hidden" : "")

  let traceView =
    trace
    ->Js.Array2.map(({before, mapping, after}) => {
      open Inline
      let line = Belt.Array.concatMany([[Text("---------", [])]])
      let before = Belt.Array.concatMany([[Text("before: ", [])], before])
      let mapping = Belt.Array.concatMany([[Text("mapping: ", [])], mapping])
      let after = Belt.Array.concatMany([[Text("after: ", [])], after])

      let inlines = [Vert([line, before, mapping, after ])]

      <Inlines value=Element(inlines) />
    })
    ->React.array

  <div className>
    {React.string(
      "DEBUG: displaying the whole history (" ++ string_of_int(Js.Array.length(trace)) ++ " steps)",
    )}
    {traceView}
  </div>
}
