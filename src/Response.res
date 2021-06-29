open Belt

module Specification = {
  type t = {
    id: int,
    pre: string,
    post: string,
    mutable range: SrcLoc.Range.t,
    mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  }

  let destroy = self => self.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    tuple4(int, string, string, SrcLoc.Range.decode) |> map(((id, pre, post, range)) => {
      id: id,
      pre: pre,
      post: post,
      range: range,
      decorations: [],
    })
  }
}

module Kind = {
  type t =
    | Display(int, array<Element.Section.t>)
    | UpdateSpecs(array<Specification.t>)
    | ConsoleLog(string)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "ResDisplay" =>
        Contents(
          tuple2(int, array(Element.Section.decode)) |> map(((id, sections)) => Display(
            id,
            sections,
          )),
        )
      | "ResUpdateSpecs" =>
        Contents(array(Specification.decode) |> map(specs => UpdateSpecs(specs)))
      | "ResConsoleLog" => Contents(string |> map(i => ConsoleLog(i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag))
      }
    )
  }
}

type t =
  | Res(string, array<Kind.t>)
  | CannotDecodeResponse(string, Js.Json.t)
  | CannotDecodeRequest(string)
  | CannotSendRequest(string)

let decode: Json.Decode.decoder<t> = {
  open Json.Decode
  open Util.Decode
  sum(x =>
    switch x {
    | "Res" =>
      Contents(pair(string, array(Kind.decode)) |> map(((filePath, kinds)) => Res(filePath, kinds)))
    | "CannotDecodeRequest" => Contents(string |> map(msg => CannotDecodeRequest(msg)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag))
    }
  )
}
