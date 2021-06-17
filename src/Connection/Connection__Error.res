open Belt


type t =
  // probe
  | CannotConnectViaStdIO(LanguageServerMule.Search.Path.Error.t)
  | CannotConnectViaTCP(Js.Exn.t)
  | CannotConnectViaPrebuilt(Connection__Prebuilt.Error.t)
  // connection
  | ConnectionError(Js.Exn.t)
  | CannotSendRequest(Js.Exn.t)
  // decoding
  | CannotDecodeResponse(string, Js.Json.t)

let toString = error =>
  switch error {
  | CannotConnectViaStdIO(e) =>
    let body = LanguageServerMule.Search.Path.Error.toString(e)
    ("Cannot locate \"gcl\"", body ++ "\nPlease make sure that the executable is in the path")
  | CannotConnectViaTCP(_) => (
      "Cannot connect with the server",
      "Please enter \":main -d\" in ghci",
    )
  | CannotConnectViaPrebuilt(e) => (
      "Cannot download prebuilt language server",
      Connection__Prebuilt.Error.toString(e),
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
  | CannotSendRequest(exn) => (
      "PANIC: Cannot send request",
      "Please file an issue\n" ++ Js.Exn.message(exn)->Option.getWithDefault(""),
    )
  | CannotDecodeResponse(msg, json) => (
      "PANIC: Cannot decode response",
      "Please file an issue\n\n" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  }
