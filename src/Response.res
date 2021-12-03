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

module ProofObligation = {
  type t = {
    range: SrcLoc.Range.t,
    mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  }

  let destroy = self => self.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    SrcLoc.Range.decode |> map(range => {
      range: range,
      decorations: [],
    })
  }
}

module Kind = {
  type t =
    // display stuff at the panel
    | Display(int, array<Element.Section.t>)
    // mark Specs (holes) in the editor
    | UpdateSpecs(array<Specification.t>)
    // mark Proof Obligations in the editor
    | MarkPOs(array<ProofObligation.t>)
    // perform subsititution in expressions in the panel
    | Substitute(int, Element.Inlines.t)
    // emit console.log for debugging
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
      | "ResMarkPOs" => Contents(array(ProofObligation.decode) |> map(pos => MarkPOs(pos)))
      | "ResSubstitute" =>
        Contents(
          tuple2(int, Element.Inlines.decode) |> map(((id, result)) => Substitute(id, result)),
        )
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
