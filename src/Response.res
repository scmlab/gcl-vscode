open Belt
open SrcLoc

module Specification = {
  type t = {
    id: int,
    pre: string,
    post: string,
    mutable loc: Loc.t,
    mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  }

  let destroy = self => self.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)

  open Json.Decode
  let decode: decoder<t> = tuple4(int, string, string, Loc.decode) |> map(((
    id,
    pre,
    post,
    loc,
  )) => {
    id: id,
    pre: pre,
    post: post,
    loc: loc,
    decorations: [],
  })
}

module Kind = {
  type t =
    | Display(int, array<Element.Block.t>)
    | UpdateSpecs(array<Specification.t>)
    | ConsoleLog(string)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "ResDisplay" =>
      Contents(
        tuple2(int, array(Element.Block.decode)) |> map(((id, blocks)) => Display(id, blocks)),
      )
    | "ResUpdateSpecs" => Contents(array(Specification.decode) |> map(specs => UpdateSpecs(specs)))
    | "ResConsoleLog" => Contents(string |> map(i => ConsoleLog(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag))
    }
  )
}

type t =
  | Res(string, array<Kind.t>)
  | NotLoaded
  | CannotDecodeResponse(string, Js.Json.t)
  | CannotDecodeRequest(string)
  | CannotSendRequest(string)

open Json.Decode
open Util.Decode
let decode: decoder<t> = sum(x =>
  switch x {
  | "Res" =>
    Contents(pair(string, array(Kind.decode)) |> map(((filePath, kinds)) => Res(filePath, kinds)))
  | "NotLoaded" => TagOnly(_ => NotLoaded)
  | "CannotDecodeRequest" => Contents(string |> map(msg => CannotDecodeRequest(msg)))
  | tag => raise(DecodeError("Unknown constructor: " ++ tag))
  }
)
