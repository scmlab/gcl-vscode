module Impl = (Editor: Sig.Editor, State: State.Sig) => {
  module TaskCommand = Task.Command.Impl(Editor, State);
  module TaskResponse = Task__Response.Impl(Editor, State);
  module Task = Task__Types.Impl(Editor, State);
  module State = State(Editor);
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
            | Ok(x) => TaskResponse.handle(x) |> run(state),
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