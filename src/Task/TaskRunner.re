module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task.Command.Impl(Editor);
  module TaskResponse = Task__Response.Impl(Editor);
  module Task = Task__Types.Impl(Editor);
  module State = Impl__State.Impl(Editor);
  open Belt;
  // run the Tasks
  let rec run = (state: State.t, tasks: list(Task.t)): Promise.t(unit) => {
    let runTask = task =>
      switch (task) {
      | Task.WithState(callback) =>
        callback(state)->Promise.flatMap(run(state))
      | MarkError(site) =>
        let range =
          Response.Error.Site.toRange(
            site,
            state.specifications,
            Editor.Range.fromLoc,
          );
        let decorations =
          state.editor
          ->Editor.Decoration.highlightBackground(
              Editor.Decoration.Error,
              range,
            );
        state.decorations = Js.Array.concat(decorations, state.decorations);
        Promise.resolved();
      | MarkSpec(spec) =>
        open! Editor;

        let range = Range.fromLoc(spec.loc);

        let startPoint = Range.start(range);
        let endPoint = Range.end_(range);

        // range of {!
        let startRange =
          Range.make(startPoint, Point.translate(startPoint, 0, 2));
        // range of !}
        let endRange =
          Range.make(Point.translate(endPoint, 0, -2), endPoint);

        let trim = s =>
          if (String.length(s) > 77) {
            String.sub(s, 0, 73) ++ " ...";
          } else {
            s;
          };

        let preCondText = " " ++ trim(GCL.Syntax.Pred.toString(spec.pre));
        let postCondText = " " ++ trim(GCL.Syntax.Pred.toString(spec.post));

        // see if the Spec's precondition and the post-condition look the same (i.e. the Q_Q case)
        let isQQ = preCondText == postCondText;
        let decorations =
          Array.concatMany([|
            Decoration.overlayText(
              state.editor,
              Spec,
              isQQ ? "" : preCondText,
              startRange,
            ),
            Decoration.overlayText(
              state.editor,
              Spec,
              isQQ ? "" : postCondText,
              endRange,
            ),
            Decoration.highlightBackground(state.editor, Spec, startRange),
            Decoration.highlightBackground(state.editor, Spec, endRange),
          |]);

        state.decorations = Js.Array.concat(decorations, state.decorations);
        Promise.resolved();
      | DigHole(site) =>
        let range =
          Response.Error.Site.toRange(
            site,
            state.specifications,
            Editor.Range.fromLoc,
          );
        state.editor->Editor.Decoration.digHole(range);
        Promise.resolved();
      | RemoveDecorations =>
        state.decorations->Array.forEach(Editor.Decoration.destroy);
        Promise.resolved();

      | DispatchCommand(command) =>
        Js.log2("[ dispatch command ]", command);
        TaskCommand.dispatch(command) |> run(state);
      | SendRequest(request) =>
        Js.log("[ send request ]");
        state
        ->State.sendRequest(request)
        ->Promise.flatMap(
            fun
            | Error(error) => {
                let (header, body) = Sig.Error.toString(error);
                [Task.Display(Error(header), Plain(body))] |> run(state);
              }
            | Ok(response) => TaskResponse.handle(response) |> run(state),
          );
      | Display(header, body) =>
        state->State.display(header, body)->Promise.map(_ => ())
      };

    let rec runEach =
      fun
      | [] => Promise.resolved()
      | [x, ...xs] => {
          let%P () = runTask(x);
          let%P () = runEach(xs);
          Promise.resolved();
        };
    runEach(tasks);
  };
};