open Belt
module Inlines = Element__Inlines
module Block = Element__Block

module Deco = {
  type t = Plain | Red | Yellow | Green | Blue

  let toClassName = x =>
    switch x {
    | Plain => ""
    | Red => "element-deco-red"
    | Yellow => "element-deco-yellow"
    | Green => "element-deco-green"
    | Blue => "element-deco-blue"
    }

  open Json.Decode
  let decode: decoder<t> = string |> map(tag =>
    switch tag {
    | "Red" => Red
    | "Yellow" => Yellow
    | "Green" => Green
    | "Blue" => Blue
    | _ => Plain
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Plain => string("Plain")
    | Red => string("Red")
    | Yellow => string("Yellow")
    | Green => string("Green")
    | Blue => string("Blue")
    }
}

type t = {
  deco: Deco.t,
  blocks: array<Block.t>,
}

let decode: Json.Decode.decoder<t> = {
  open Json.Decode 
  tuple2(Deco.decode, array(Block.decode)) |> map(((deco, blocks)) => { deco, blocks })
}

let encode: Json.Encode.encoder<t> = ({ deco, blocks }) => {
  open Json.Encode 
  (deco, blocks) |> tuple2(Deco.encode, array(Block.encode))
}

open React
@react.component
let make = (~value: t, ~onInsertAnchor: string => unit) => {
  let className = "element-section " ++ Deco.toClassName(value.deco)
  let blocks =
    value.blocks
    ->Array.mapWithIndex((i, value) => {
      <Block value key={string_of_int(i)} onInsertAnchor />
    })
    ->array
  <li className> {blocks} </li>
}
