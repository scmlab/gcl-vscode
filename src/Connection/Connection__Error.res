open Belt

type t =
  // server probing
  | CannotAcquireHandle(LanguageServerMule.Source.Error.t)
  // connection
  | ConnectionError(Js.Exn.t)
  // decoding
  | CannotDecodeResponse(string, Js.Json.t)

let toString = error =>
  switch error {
  | CannotAcquireHandle(e) => (
      "Cannot acquire \"gcl\"",
      LanguageServerMule.Source.Error.toString(e),
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
