open Belt;

open! Response;

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module Task__Error = Task__Error.Impl(Editor);

  // a dictionary of decorations for <Link>
  let decorationDict: Js.Dict.t(array(Editor.Decoration.t)) =
    Js.Dict.empty();
  let delete_: string => unit = [%raw
    "function (id) {delete decorationDict[id]}"
  ];

  let clearDict = () => {
    Js.Dict.entries(decorationDict)
    ->Array.forEach(((key, decos)) => {
        delete_(key);
        decos->Array.forEach(Editor.Decoration.destroy);
      });
  };

  // from View response to Tasks
  let handle = (editor, response): list(Task.t) =>
    switch (response) {
    | View.Response.SetMode(mode) => [
        Task.WithState(
          state => {
            state.mode = mode;
            Promise.resolved([]);
          },
        ),
      ]
    | Link(MouseOver(loc)) =>
      let key = GCL.Loc.toString(loc);
      let range = Editor.Range.fromLoc(loc);
      let decoration =
        Editor.Decoration.highlightBackground(editor, Highlight, range);
      Js.Dict.set(decorationDict, key, decoration);
      [];
    | Link(MouseOut(loc)) =>
      let key = GCL.Loc.toString(loc);
      Js.Dict.get(decorationDict, key)
      ->Option.forEach(decos =>
          decos->Array.forEach(Editor.Decoration.destroy)
        );
      delete_(key);
      [];
    | Link(MouseClick(loc)) =>
      let range = Editor.Range.fromLoc(loc);
      editor->Editor.selectText(range);
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
};