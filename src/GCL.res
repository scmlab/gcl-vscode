module Pos = {
  type t = Pos(string, int, int)

  let toPoint = x =>
    switch x {
    | Pos(_, line, column) => VSCode.Position.make(line - 1, column - 1)
    }

  let fromPoint = (point, filepath) => Pos(
    filepath,
    VSCode.Position.line(point) + 1,
    VSCode.Position.character(point) + 1,
  )

  let toString = x =>
    switch x {
    | Pos(_, line, column) => string_of_int(line) ++ ":" ++ string_of_int(column)
    }

  let translate = (by, x) =>
    switch x {
    | Pos(path, line, column) =>
      let Pos(_, y, x) = by
      Pos(path, line + y, column + x)
    }

  let translateBy = (y, x, point) =>
    switch point {
    | Pos(path, line, column) => Pos(path, line + y, column + x)
    }

  open Json.Decode
  let decode: decoder<t> = tuple4(string, int, int, int) |> map(((w, x, y, _)) => Pos(w, x, y))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Pos(path, line, column) => (path, line, column, 0) |> tuple4(string, int, int, int)
    }
}

module Range = {
  type t = Range(Pos.t, Pos.t)

  let toRange = x =>
    switch x {
    | Range(x, Pos(_, line, column)) =>
      VSCode.Range.make(Pos.toPoint(x), VSCode.Position.make(line - 1, column))
    }

  let toString = range =>
    switch range {
    | Range(Pos(_, line1, col1), Pos(_, line2, col2)) =>
      if line1 == line2 {
        string_of_int(line1) ++ ":" ++ string_of_int(col1) ++ "-" ++ string_of_int(col2)
      } else {
        string_of_int(line1) ++
        ":" ++
        string_of_int(col1) ++
        "-" ++
        string_of_int(line2) ++
        ":" ++
        string_of_int(col2)
      }
    }

  let translate = (by, x) =>
    switch x {
    | Range(x, y) =>
      switch by {
      | Range(w, v) => Range(Pos.translate(x, w), Pos.translate(y, v))
      }
    }

  let translateBy = (startY, startX, endY, endX, x) =>
    switch x {
    | Range(x, y) => Range(Pos.translateBy(startY, startX, x), Pos.translateBy(endY, endX, y))
    }

  open Json.Decode
  let decode: decoder<t> = pair(Pos.decode, Pos.decode) |> map(((x, y)) => Range(x, y))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Range(x, y) => (x, y) |> pair(Pos.encode, Pos.encode)
    }
}

module Loc = {
  type t =
    | NoLoc
    | Loc(Pos.t, Pos.t)

  let toRange = x =>
    switch x {
    | NoLoc => VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(0, 0))
    | Loc(x, Pos(_, line, column)) =>
      VSCode.Range.make(Pos.toPoint(x), VSCode.Position.make(line - 1, column))
    }
  let toString = x =>
    switch x {
    | NoLoc => "NoLoc"
    | Loc(x, y) => Pos.toString(x) ++ ("-" ++ Pos.toString(y))
    }

  let translate = (by, x) =>
    switch x {
    | NoLoc => by
    | Loc(x, y) =>
      switch by {
      | NoLoc => Loc(x, y)
      | Loc(w, v) => Loc(Pos.translate(x, w), Pos.translate(y, v))
      }
    }

  let translateBy = (startY, startX, endY, endX, x) =>
    switch x {
    | NoLoc => Loc(Pos("", startY, startX), Pos("", endY, endX))
    | Loc(x, y) => Loc(Pos.translateBy(startY, startX, x), Pos.translateBy(endY, endX, y))
    }

  open Util.Decode
  open Json.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "Loc" => Contents(pair(Pos.decode, Pos.decode) |> map(((x, y)) => Loc(x, y)))
    | "NoLoc" => TagOnly(_ => NoLoc)
    | tag => raise(DecodeError("[Loc] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | NoLoc => object_(list{("tag", string("NoLoc"))})
    | Loc(x, y) =>
      object_(list{("tag", string("Loc")), ("contents", (x, y) |> pair(Pos.encode, Pos.encode))})
    }
}

type pos = Pos.t
type range = Range.t
type loc = Loc.t
