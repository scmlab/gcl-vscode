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
      | SetSpecifications(specifications) =>
        state->State.setSpecifications(specifications);
        Promise.resolved();
      | AddDecorations(_decorations) => Promise.resolved()
      | MarkError(site) =>
        let range =
          GCL.Response.Error.Site.toRange(
            site,
            state.specifications,
            Editor.toRange,
          );
        state.editor
        ->Editor.Decoration.markBackground(range)
        ->Js.Array.push(state.decorations)
        ->ignore;
        Promise.resolved();
      | DigHole(site) =>
        let range =
          GCL.Response.Error.Site.toRange(
            site,
            state.specifications,
            Editor.toRange,
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