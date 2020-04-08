open Belt;

open Command;

module Impl = (Editor: Sig.Editor, State: State.Sig) => {
  module Task__Types = Task__Types.Impl(Editor, State);
  module State = State(Editor);
  // from Editor Command to Tasks
  let dispatch =
    fun
    | Reload => [
        Task__Types.WithState(
          state => {
            let editor = state->State.getEditor;
            editor
            ->Editor.getFileName
            ->Option.mapWithDefault(Promise.resolved([]), fileName => {
                editor
                ->Editor.save
                ->Promise.map(saveSucceed =>
                    if (saveSucceed && fileName != "") {
                      state
                      ->State.sendRequest(Types.Request.Load(fileName))
                      ->Promise.get(Js.log);
                      [];
                    } else {
                      [];
                    }
                  )
              });
          },
        ),
      ]
    | _ => [];
};