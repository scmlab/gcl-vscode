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

let decode: Json.Decode.decoder<t> = json => {
  open Json.Decode
  {
    deco: json |> field("sectionDeco", Deco.decode),
    blocks: json |> field("sectionBlocks", array(Block.decode)),
  }
}

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  object_(list{
    ("sectionDeco", x.deco |> Deco.encode),
    ("sectionBlocks", x.blocks |> array(Block.encode)),
  })
}

open! React
@react.component
let make = (~value: t) => {
  let className = "element-section " ++ Deco.toClassName(value.deco)
  let blocks =
    value.blocks
    ->Array.mapWithIndex((i, value) => {
      <Block value key={string_of_int(i)} />
    })
    ->array
  <li className> {blocks} </li>
}
