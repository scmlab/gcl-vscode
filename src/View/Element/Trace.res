module Inlines = Element__Inlines__Type

type t = {
  expr: array<Inlines.Inline.t>,
  undo: unit => unit,
}
