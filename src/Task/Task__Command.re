open Belt;

open Command;

module Impl = (Editor: Sig.Editor, State: Sig.State) => {
  module Task__Types = Task__Types.Impl(Editor, State);
  module State = State(Editor);
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
                      [SendRequest(Request.Load(fileName))];
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
    | _ => [];
};