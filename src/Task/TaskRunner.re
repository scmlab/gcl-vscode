module Impl = (Editor: Sig.Editor, State: State.Sig) => {
  module TaskCommand = Task.Command.Impl(Editor, State);
  module Task = Task__Types.Impl(Editor, State);
  module State = State(Editor);
  // run the Tasks
  let rec run = (state: State.t, tasks: list(Task.t)): Promise.t(unit) => {
    let runTask = task =>
      switch (task) {
      | Task.WithState(callback) =>
        callback(state)->Promise.flatMap(run(state))
      | DispatchCommand(command) =>
        Js.log2("[ dispatch command ]", command);
        TaskCommand.dispatch(command) |> run(state);
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