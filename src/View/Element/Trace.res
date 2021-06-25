module Inlines = Element__Inlines__Type

type t = {
  before: array<Inlines.Inline.t>,
  mapping: array<Inlines.Inline.t>,
  after: array<Inlines.Inline.t>,
  undo: unit => unit 
}

module View = {
  open React
  @react.component
  let make = (~hidden: bool, ~trace: array<t>) => {
    let className = "element-block-code-trace" ++ (hidden ? " hidden" : "")
    <div className> {string("Temp debug info: length of trace: " ++ string_of_int(Js.Array.length(trace)))} </div>
  }
}
