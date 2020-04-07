module Pos = {
  type t =
    | Pos(string, int, int);

  let toString =
    fun
    | Pos(_, line, column) =>
      string_of_int(line) ++ ":" ++ string_of_int(column);

  let translate = by =>
    fun
    | Pos(path, line, column) => {
        let Pos(_, y, x) = by;
        Pos(path, line + y, column + x);
      };

  let translateBy = (y, x) =>
    fun
    | Pos(path, line, column) => Pos(path, line + y, column + x);

  open Json.Decode;
  let decode: decoder(t) =
    json =>
      Pos(
        field("filepath", string, json),
        field("line", int, json),
        field("column", int, json),
      );
};

module Loc = {
  type t =
    | NoLoc
    | Loc(Pos.t, Pos.t);

  let toString =
    fun
    | NoLoc => "NoLoc"

    | Loc(x, y) => Pos.toString(x) ++ "-" ++ Pos.toString(y);

  let translate = by =>
    fun
    | NoLoc => by

    | Loc(x, y) =>
      switch (by) {
      | NoLoc => Loc(x, y)
      | Loc(w, v) => Loc(Pos.translate(x, w), Pos.translate(y, v))
      };

  let translateBy = (startY, startX, endY, endX) =>
    fun
    | NoLoc => Loc(Pos("", startY, startX), Pos("", endY, endX))
    | Loc(x, y) =>
      Loc(
        Pos.translateBy(startY, startX, x),
        Pos.translateBy(endY, endX, y),
      );

  open Util.Decode;
  open Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Loc" =>
        Contents(
          json =>
            Loc(
              field("start", Pos.decode, json),
              field("end", Pos.decode, json),
            ),
        )
      | "NoLoc" => TagOnly(_ => NoLoc)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

type pos = Pos.t;
type loc = Loc.t;

type message =
  | Display(string, string);

module Response = {
  type mode =
    | WP1
    | WP2;

  type linkEvent =
    | MouseOver(loc)
    | MouseOut(loc)
    | MouseClick(loc);

  type t =
    | SetMode(mode)
    | Link(linkEvent);
};