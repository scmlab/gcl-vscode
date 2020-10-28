open Belt;

open Command;

module Impl = (Editor: API.Editor) => {
  module Task__Types = Task.Impl(Editor);
  module GCLImpl = GCLImpl.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task__Types;
  // from Editor Command to Tasks
  let dispatch =
    fun
    | Load => [Connect]
    | Quit => []
    | Reload => [
        WithState(
          state => {
            let doc = Editor.getDocument(State.getEditor(state));
            doc
            ->Editor.getFileName
            ->Option.mapWithDefault(Promise.resolved([]), fileName => {
                doc
                ->Editor.save
                ->Promise.map(saveSucceed =>
                    if (saveSucceed && fileName != "") {
                      switch (state.mode) {
                      | WP1 => [
                          RemoveDecorations,
                          SendRequest(Request.Load(fileName, GCL.WP1)),
                        ]
                      | WP2 => [
                          RemoveDecorations,
                          SendRequest(Request.Load(fileName, GCL.WP2)),
                        ]
                      };
                    } else {
                      [
                        Display(
                          Error("Cannot read filepath"),
                          Plain("Please save the file first"),
                        ),
                      ];
                    }
                  )
              });
          },
        ),
      ]
    | Refine => [
        DispatchCommand(Reload),
        WithState(
          state => {
            state
            ->State.Spec.fromCursorPosition
            ->Option.mapWithDefault(
                Promise.resolved([]),
                spec => {
                  let doc = Editor.getDocument(state.editor);
                  let payload = State.Spec.getPayload(doc, spec);
                  Promise.resolved([SendRequest(Refine(spec.id, payload))]);
                },
              )
          },
        ),
      ]
    | InsertAssertion => [
        DispatchCommand(Reload),
        WithState(
          state => {
            let cursor = Editor.getCursorPosition(state.editor);
            open GCL.Pos;
            let Pos(_, line, _) = GCLImpl.Pos.fromPoint(cursor, "whatever");
            Promise.resolved([SendRequest(InsertAssertion(line))]);
          },
        ),
      ]
    | Debug => {
        [SendRequest(Debug)];
      };
};