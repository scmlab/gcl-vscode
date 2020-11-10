type t =
  | Decode(string, Js.Json.t);
let toString =
  fun
  | Decode(msg, json) => (
      {js|JSON Decode Error|js},
      msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
    );
