type t = {trace: array<unit => unit>}

let make = () => {
  trace: [],
}

let push = (self, undo) => Js.Array.push(undo, self.trace)->ignore
let pop = self =>
  switch Js.Array.pop(self.trace) {
  | None => ()
  | Some(undo) => undo()
  }
let isEmpty = self => Js.Array.length(self.trace) == 0

module Context = {
  let context = React.createContext(make())

  module Provider = {
    let provider = React.Context.provider(context)

    @react.component
    let make = (~value, ~children) => {
      React.createElement(provider, {"value": value, "children": children})
    }
  }
}

module View = {
  open React
  @react.component
  let make = (~hidden: bool) => {
    let className = "element-block-code-trace" ++ (hidden ? " hidden" : "")
    <div className> {string("asdf")} </div>
  }
}
