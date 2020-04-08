module Request = {
  type t =
    | Load(string)
    | Load2(string)
    | Refine(int, string)
    | InsertAssertion(int)
    | Debug;

  module Encode = {
    open Json.Encode;
    let request: encoder(t) =
      fun
      | Load(filepath) =>
        object_([("tag", string("Load")), ("contents", string(filepath))])
      | Load2(filepath) =>
        object_([
          ("tag", string("Load2")),
          ("contents", string(filepath)),
        ])
      | Refine(id, payload) =>
        object_([
          ("tag", string("Refine")),
          ("contents", (id, payload) |> pair(int, string)),
        ])
      | InsertAssertion(n) =>
        object_([
          ("tag", string("InsertAssertion")),
          ("contents", int(n)),
        ])
      | Debug => object_([("tag", string("Debug"))]);
  };

  let encode: t => string = x => x->Encode.request->Json.stringify;
};