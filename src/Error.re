type t =
  | Connection(Connection.Error.t)
  | Decode(string, Js.Json.t);
let toString =
  fun
  | Connection(e) => Connection.Error.toString(e)
  | Decode(msg, json) => (
      {js|JSON Decode Error|js},
      msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
    );
