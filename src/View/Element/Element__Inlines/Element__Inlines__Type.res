open Belt

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

module RewriteReason = {
  type t = Assigment

  let decode: Json.Decode.decoder<t> = {
    // open Json.Decode
    _ => Assigment
  }

  let encode: Json.Encode.encoder<t> = {
    open Json.Encode
    _ => string("RRAssigment")
  }
}

module Inline = {
  type rec t =
    | Icon(string, ClassNames.t)
    | Text(string, ClassNames.t)
    | Link(SrcLoc.Range.t, array<t>, ClassNames.t)
    | Sbst(array<t>, array<t>, array<t>, ClassNames.t)
    | Sbst2(RewriteReason.t, array<t>, array<t>, array<t>, ClassNames.t)
    | Horz(array<array<t>>)
    | Vert(array<array<t>>)
    | Parn(array<t>)
    // refactor PrHz
    | PrHz(array<array<t>>)

  let rec decode: unit => Json.Decode.decoder<t> = () => {
    open Json.Decode
    open Util.Decode
    sum(x =>
      switch x {
      | "Icon" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Icon(s, cs)))
      | "Text" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Text(s, cs)))
      | "Link" =>
        Contents(
          tuple3(SrcLoc.Range.decode, array(decode()), ClassNames.decode) |> map(((
            r,
            xs,
            cs,
          )) => Link(r, xs, cs)),
        )
      | "Sbst" =>
        Contents(
          tuple4(array(decode()), array(decode()), array(decode()), ClassNames.decode) |> map(((
            a,
            b,
            c,
            d,
          )) => Sbst(a, b, c, d)),
        )
      | "Sbst2" =>
        Contents(
          tuple5(
            RewriteReason.decode,
            array(decode()),
            array(decode()),
            array(decode()),
            ClassNames.decode,
          ) |> map(((a, b, c, d, e)) => Sbst2(a, b, c, d, e)),
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
    | Link(r, s, cs) =>
      object_(list{
        ("tag", string("Link")),
        ("contents", (r, s, cs) |> tuple3(SrcLoc.Range.encode, array(encode), ClassNames.encode)),
      })
    | Sbst(a, b, c, d) =>
      object_(list{
        ("tag", string("Sbst")),
        (
          "contents",
          (a, b, c, d) |> tuple4(array(encode), array(encode), array(encode), ClassNames.encode),
        ),
      })
    | Sbst2(a, b, c, d, e) =>
      object_(list{
        ("tag", string("Sbst2")),
        (
          "contents",
          (a, b, c, d, e) |> Util.Encode.tuple5(
            RewriteReason.encode,
            array(encode),
            array(encode),
            array(encode),
            ClassNames.encode,
          ),
        ),
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
