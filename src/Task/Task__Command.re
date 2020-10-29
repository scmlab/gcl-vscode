open Belt;
open Command;

open! Task;
// from Editor Command to Tasks
let dispatch =
  fun
  | Load => [Connect]
  | Quit => []
  | Reload => [
      WithState(
        state => {
          open VSCode;
          let doc = TextEditor.document(state.editor);
          let fileName = doc->TextDocument.fileName;
          doc
          ->TextDocument.save
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
            );
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
                open VSCode;
                let doc = TextEditor.document(state.editor);
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
          open VSCode;
          let cursor = state.editor->TextEditor.selection->Selection.end_;
          open GCL.Pos;
          let Pos(_, line, _) = GCL.Pos.fromPoint(cursor, "whatever");
          Promise.resolved([SendRequest(InsertAssertion(line))]);
        },
      ),
    ]
  | Debug => {
      [SendRequest(Debug)];
    };
