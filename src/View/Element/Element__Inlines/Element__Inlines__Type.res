module ClassNames = {
  type t = array<string>

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    array(string)
  }

  let encode: Json.Encode.encoder<t> = {
    open Json.Encode
    array(string)
  }
}

module Inline = {
  type rec t =
    | Icon(string, ClassNames.t)
    | Text(string, ClassNames.t)
    | Code(array<t>)
    | Link(SrcLoc.Range.t, array<t>, ClassNames.t)
    | Expn(array<t>, array<t>, array<t>)
    | Horz(array<array<t>>)
    | Vert(array<array<t>>)
    | Parn(array<t>)
    // TODO: refactor PrHz
    | PrHz(array<array<t>>)

  let rec decode: unit => Json.Decode.decoder<t> = () => {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "Icon" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Icon(s, cs)))
      | "Text" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Text(s, cs)))
      | "Snpt" => Contents(array(decode()) |> map(xs => Code(xs)))
      | "Link" =>
        Contents(
          tuple3(SrcLoc.Range.decode, array(decode()), ClassNames.decode) |> map(((
            r,
            xs,
            cs,
          )) => Link(r, xs, cs)),
        )
      | "Expn" =>
        Contents(
          tuple3(array(decode()), array(decode()), array(decode())) |> map(((a, b, c)) => Expn(
            a,
            b,
            c,
          )),
        )
      | "Horz" => Contents(array(array(decode())) |> map(xs => Horz(xs)))
      | "Vert" => Contents(array(array(decode())) |> map(xs => Vert(xs)))
      | "Parn" => Contents(array(decode()) |> map(x => Parn(x)))
      | "PrHz" => Contents(array(array(decode())) |> map(xs => PrHz(xs)))
      | tag => raise(DecodeError("[Element.Inline] Unknown constructor: " ++ tag))
      }
    )
  }
  let decode = decode()

  let rec encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Icon(s, cs) =>
      object_(list{
        ("tag", string("Icon")),
        ("contents", (s, cs) |> pair(string, ClassNames.encode)),
      })

    | Text(s, cs) =>
      object_(list{
        ("tag", string("Text")),
        ("contents", (s, cs) |> pair(string, ClassNames.encode)),
      })
    | Code(xs) => object_(list{("tag", string("Snpt")), ("contents", xs |> array(encode))})
    | Link(r, s, cs) =>
      object_(list{
        ("tag", string("Link")),
        ("contents", (r, s, cs) |> tuple3(SrcLoc.Range.encode, array(encode), ClassNames.encode)),
      })
    | Expn(a, b, c) =>
      object_(list{
        ("tag", string("Expn")),
        ("contents", (a, b, c) |> tuple3(array(encode), array(encode), array(encode))),
      })
    | Horz(xs) => object_(list{("tag", string("Horz")), ("contents", xs |> array(array(encode)))})
    | Vert(xs) => object_(list{("tag", string("Vert")), ("contents", xs |> array(array(encode)))})
    | Parn(x) => object_(list{("tag", string("Parn")), ("contents", x |> array(encode))})
    | PrHz(xs) => object_(list{("tag", string("PrHz")), ("contents", xs |> array(array(encode)))})
    }
  }
}

type t = Element(array<Inline.t>)
let string = s => Element([Text(s, [])])

let decode: Json.Decode.decoder<t> = {
  open Json.Decode
  array(Inline.decode) |> map(elems => Element(elems))
}

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  switch x {
  | Element(elemss) => elemss |> array(Inline.encode)
  }
}
