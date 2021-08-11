open Belt

type t =
  // server probing
  | CannotAcquireHandle(array<LanguageServerMule.Source.Error.t>)
  // connection
  | ConnectionError(Js.Exn.t)
  // decoding
  | CannotDecodeResponse(string, Js.Json.t)

let toString = error =>
  switch error {
  | CannotAcquireHandle(es) => (
      "Cannot connect with \"gcl\"",
      "After these attempts: \n" ++
      es->Array.mapWithIndex((index, error) => string_of_int(index) ++ ". " ++ LanguageServerMule.Source.Error.toString(error))->Js.Array2.joinWith("\n"),
    )
  | ConnectionError(exn) =>
    let isECONNREFUSED =
      Js.Exn.message(exn)->Option.mapWithDefault(
        false,
        Js.String.startsWith("connect ECONNREFUSED"),
      )

    isECONNREFUSED
      ? ("LSP Connection Error", "Please enter \":main -d\" in ghci")
      : ("LSP Client Error", Js.Exn.message(exn)->Option.getWithDefault(""))
  | CannotDecodeResponse(msg, json) => (
      "PANIC: Cannot decode response",
      "Please file an issue\n\n" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  }
