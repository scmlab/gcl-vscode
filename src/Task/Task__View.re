open Belt;
open VSCode;

open! Response;

// a dictionary of decorations for <Link>
let decorationDict: Js.Dict.t(array(TextEditorDecorationType.t)) =
  Js.Dict.empty();
let delete_: string => unit = [%raw
  "function (id) {delete decorationDict[id]}"
];

let clearDict = () => {
  Js.Dict.entries(decorationDict)
  ->Array.forEach(((key, decos)) => {
      delete_(key);
      decos->Array.forEach(AgdaModeVscode.Editor.Decoration.destroy);
    });
};

// from View response to Tasks
let handle = (editor, response): list(Task.t) =>
  switch (response) {
  | ViewType.Response.SetMode(mode) => [
      Task.WithState(
        state => {
          state.mode = mode;
          Promise.resolved([]);
        },
      ),
    ]
  | Link(MouseOver(loc)) =>
    let key = GCL.Loc.toString(loc);
    let range = GCL.Loc.toRange(loc);
    let decoration =
      AgdaModeVscode.Editor.Decoration.highlightBackground(
        editor,
        "editor.symbolHighlightBackground",
        [|range|],
      );
    Js.Dict.set(decorationDict, key, [|decoration|]);
    [];
  | Link(MouseOut(loc)) =>
    let key = GCL.Loc.toString(loc);
    Js.Dict.get(decorationDict, key)
    ->Option.forEach(decos =>
        decos->Array.forEach(AgdaModeVscode.Editor.Decoration.destroy)
      );
    delete_(key);
    [];
  | Link(MouseClick(loc)) =>
    let range = GCL.Loc.toRange(loc);
    AgdaModeVscode.Editor.Selection.set(editor, range);
    [];
  | Substitute(i, expr, subst) =>
    clearDict();
    [SendRequest(Substitute(i, expr, subst))];
  | Initialized => []
  | Destroyed => [
      WithState(
        state => {
          State.destroy(state)->ignore;
          Promise.resolved([]);
        },
      ),
    ]
  };
