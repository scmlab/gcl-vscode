module Impl = (Editor: API.Editor) => {
  module Pos = {
    open GCL.Pos;
    let toPoint =
      fun
      | Pos(_, line, column) => Editor.Point.make(line - 1, column - 1);

    let fromPoint = (point, filepath) =>
      Pos(
        filepath,
        Editor.Point.line(point) + 1,
        Editor.Point.column(point) + 1,
      );
  };

  module Loc = {
    open GCL.Loc;
    let toRange =
      fun
      | NoLoc =>
        Editor.Range.make(
          Editor.Point.make(0, 0),
          Editor.Point.make(0, 0),
        )
      | Loc(x, Pos(_, line, column)) =>
        Editor.Range.make(
          Pos.toPoint(x),
          Editor.Point.make(line - 1, column),
        );
  };
};