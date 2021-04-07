open Belt 

type t =
  // probe
  | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
  | CannotConnectViaTCP(Js.Exn.t)
  // connection
  | ConnectionError(Js.Exn.t)
  | CannotSendRequest(Js.Exn.t)
  // decoding
  | CannotDecodeResponse(string, Js.Json.t)

let toString = error =>
  switch error {
  | CannotConnectViaStdIO(e) =>
    let (_header, body) = AgdaModeVscode.Process.PathSearch.Error.toString(e)
    ("Cannot locate \"gcl\"", body ++ "\nPlease make sure that the executable is in the path")
  | CannotConnectViaTCP(_) => (
      "Cannot connect with the server",
      "Please enter \":main -d\" in ghci",
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
