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
  tuple2(Deco.decode, array(Block.decode)) |> map(((deco, blocks)) => {deco: deco, blocks: blocks})
}

let encode: Json.Encode.encoder<t> = ({deco, blocks}) => {
  open Json.Encode
  (deco, blocks) |> tuple2(Deco.encode, array(Block.encode))
}

open React
@react.component
let make = (~value: t, ~onInsertAnchor: string => unit, ~onClickSolveButton: string => unit) => {
  // HACK:
  //    if the first block is "HeaderWithButtons"
  //    then we assume that this section is a PO
  //    and the last block (the explanation for the PO) should be hidden

  let isPO = value.blocks[0]->Option.flatMap(block =>
    switch block {
    | HeaderWithButtons(_) => Some()
    | _ => None
    }
  )->Option.isSome

  // effects for the button for toggle display th eexplanation
  let (displayExplanation, setDisplayExplanation) = useState(_ => false)
  let onDisplayExplanation = x => setDisplayExplanation(_ => x)

  let className = "element-section " ++ Deco.toClassName(value.deco)

  let blocks =
    value.blocks
    ->Array.keepWithIndex((_, index) => {
      // see if this is the last block, which might be the block for displaying explanations
      // If this is the last block(which entails it's explanation), and displayExplanation==false,
      // then drop this block from value.blocks.
      let isForExplanation = isPO && index == Array.length(value.blocks) - 1
      !isForExplanation || (isForExplanation && displayExplanation)
    })
    ->Array.mapWithIndex((index, block) => {
      <Block value=block key={string_of_int(index)} onInsertAnchor onClickSolveButton onDisplayExplanation />
    })
    ->array

  <li className> {blocks} </li>
}
