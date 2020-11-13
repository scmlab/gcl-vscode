module Decode = {
  open Json.Decode;

  type fieldType('a) =
    | Contents(decoder('a))
    | TagOnly(decoder('a));

  let sum = decoder =>
    field("tag", string)
    |> andThen(tag =>
         switch (decoder(tag)) {
         | Contents(d) => field("contents", d)
         | TagOnly(d) => d
         }
       );

  let maybe: decoder('a) => decoder(option('a)) =
    decoder =>
      sum(
        fun
        | "Just" => Contents(json => Some(decoder(json)))
        | _ => TagOnly(_ => None),
      );

  let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE, json) =>
    if (Js.Array.isArray(json)) {
      let source: array(Js.Json.t) = Obj.magic(json: Js.Json.t);
      let length = Js.Array.length(source);
      if (length == 5) {
        try((
          decodeA(Js.Array.unsafe_get(source, 0)),
          decodeB(Js.Array.unsafe_get(source, 1)),
          decodeC(Js.Array.unsafe_get(source, 2)),
          decodeD(Js.Array.unsafe_get(source, 3)),
          decodeE(Js.Array.unsafe_get(source, 4)),
        )) {
        | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple5"))
        };
      } else {
        raise(
          DecodeError(
            {j|Expected array of length 5, got array of length $length|j},
          ),
        );
      };
    } else {
      raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)));
    };
};

module Encode = {
  open Json.Encode;
  let tuple5 = (encodeA, encodeB, encodeC, encodeD, encodeE, (a, b, c, d, e)) =>
    jsonArray([|
      encodeA(a),
      encodeB(b),
      encodeC(c),
      encodeD(d),
      encodeE(e),
    |]);
} /* }*/;

// module React = {
//   open ReasonReact;

//   let manyIn = (elems, elem) =>
//     ReactDOMRe.createDOMElementVariadic(
//       elem,
//       ~props=ReactDOMRe.domProps(),
//       elems,
//     );

//   let manyIn2 = (elems, elem, props) =>
//     ReactDOMRe.createDOMElementVariadic(elem, ~props, elems);

//   let sepBy' = (item: list(reactElement), sep: reactElement) =>
//     switch (item) {
//     | [] => <> </>
//     | [x] => x
//     | [x, ...xs] =>
//       [x, ...List.map(xs, i => <> sep i </>)]->List.toArray->manyIn("span")
//     };
//   let sepBy = (sep: reactElement, xs) => xs->List.fromArray->sepBy'(sep);

//   let enclosedBy =
//       (front: reactElement, back: reactElement, item: reactElement) =>
//     <> front {string(" ")} item {string(" ")} back </>;

//   let when_ = (p, className) => p ? " " ++ className : "";
//   let showWhen =
//     fun
//     | true => ""
//     | false => " hidden";
// };

// module JsError = {
//   let toString = (_e: Js.Exn.t) => {
//     %raw
//     "_e.toString()";
//   };
