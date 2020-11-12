type t =
  | LSP(string)
  | Decode(string, Js.Json.t);
let toString =
  fun
  | LSP(msg) => ({js|LSP Error|js}, msg)
  | Decode(msg, json) => (
      {js|JSON Decode Error|js},
      msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
    );

let fromJsError = (error: 'a): string => {
  [%raw "function (e) {return e.toString()}"](error);
};
