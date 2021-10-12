module Inlines = Element__Inlines__Type

type t = {
  id: int,
  before: array<Inlines.Inline.t>,
  mapping: array<Inlines.Inline.t>,
  after: array<Inlines.Inline.t>,
  undo: unit => unit,
}
