module Pos = {
  type t = {
    path: string,
    line: int,
    column: int,
    offset: int,
  }

  let toVSCodePos = pos => VSCode.Position.make(pos.line - 1, pos.column - 1)

  // let fromVSCodePos = (point, path) => {
  //   path: path,
  //   line: VSCode.Position.line(point) + 1,
  //   column: VSCode.Position.character(point) + 1,
  //   offset: 0,
  // }

  let toString = pos => string_of_int(pos.line) ++ ":" ++ string_of_int(pos.column)

  let translate = (by, pos) => {
    path: pos.path,
    line: pos.line + by.line,
    column: pos.column + by.column,
    offset: pos.offset + by.offset,
  }

  let translateBy = (line, column, offset, pos) => {
    path: pos.path,
    line: pos.line + line,
    column: pos.column + column,
    offset: pos.offset + offset,
  }

  open Json.Decode
  let decode: decoder<t> = tuple4(string, int, int, int) |> map(((path, line, column, offset)) => {path, line, column, offset})

  open! Json.Encode
  let encode: encoder<t> = pos => (pos.path, pos.line, pos.column, pos.offset) |> tuple4(string, int, int, int)
}

module Range = {
  type t = Range(Pos.t, Pos.t)

  let toVSCodeRange = x =>
    switch x {
    | Range(start, end_) =>
      VSCode.Range.make(Pos.toVSCodePos(start), VSCode.Position.make(end_.line - 1, end_.column))
    }

  let toString = range =>
    switch range {
    | Range(start, end_) =>
      if start.line == end_.line {
        string_of_int(start.line) ++ ":" ++ string_of_int(start.column) ++ "-" ++ string_of_int(end_.column)
      } else {
        string_of_int(start.line) ++
        ":" ++
        string_of_int(start.column) ++
        "-" ++
        string_of_int(end_.line) ++
        ":" ++
        string_of_int(end_.column)
      }
    }

  let translate = (by, x) =>
    switch x {
    | Range(x, y) =>
      switch by {
      | Range(w, v) => Range(Pos.translate(x, w), Pos.translate(y, v))
      }
    }

  let translateBy = (startY, startX, startZ, endY, endX, endZ, x) =>
    switch x {
    | Range(x, y) => Range(Pos.translateBy(startY, startX, startZ, x), Pos.translateBy(endY, endX, endZ, y))
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

  let toVSCodeRange = x =>
    switch x {
    | NoLoc => VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(0, 0))
    | Loc(start, end_) =>
      VSCode.Range.make(Pos.toVSCodePos(start), VSCode.Position.make(end_.line - 1, end_.column))
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

  let translateBy = (startY, startX, startZ, endY, endX, endZ, x) =>
    switch x {
    | NoLoc => Loc({path: "", line: startY, column: startX, offset: startZ}, {path: "", line: endY, column: endX, offset: endZ})
    | Loc(start, end) => Loc(Pos.translateBy(startY, startX, startZ, start), Pos.translateBy(endY, endX, endZ, end))
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
