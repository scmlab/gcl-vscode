type t = {history: array<unit => unit>}

let make = () => {
  history: [],
}

let push = (self, undo) => Js.Array.push(undo, self.history)->ignore
let pop = self =>
  switch Js.Array.pop(self.history) {
  | None => ()
  | Some(undo) => undo()
  }
let isEmpty = (self) => Js.Array.length(self.history) == 0
