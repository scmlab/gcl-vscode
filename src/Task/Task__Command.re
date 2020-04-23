open Belt;

open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task__Types = Task__Types.Impl(Editor);
  module State = Impl__State.Impl(Editor);
  open! Task__Types;
  // from Editor Command to Tasks
  let dispatch =
    fun
    | Reload => [
        WithState(
          state => {
            let editor = state->State.getEditor;
            editor
            ->Editor.getFileName
            ->Option.mapWithDefault(Promise.resolved([]), fileName => {
                editor
                ->Editor.save
                ->Promise.map(saveSucceed =>
                    if (saveSucceed && fileName != "") {
                      switch (state.mode) {
                      | WP1 => [
                          RemoveDecorations,
                          SendRequest(Request.Load(fileName)),
                        ]
                      | WP2 => [
                          RemoveDecorations,
                          SendRequest(Request.Load2(fileName)),
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
                  let payload = State.Spec.getPayload(state.editor, spec);
                  Promise.resolved([SendRequest(Refine(spec.id, payload))]);
                },
              )
          },
        ),
      ]
    | _ => [];
};