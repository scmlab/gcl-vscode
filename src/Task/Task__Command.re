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
                [RemoveDecorations, SendRequest(Request.Load(fileName))];
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
                let fileName = TextDocument.fileName(doc);
                Promise.resolved([
                  SendRequest(Refine(fileName, spec.id, payload)),
                ]);
              },
            )
        },
      ),
    ]
  | Debug => {
      [SendRequest(Debug)];
    };
