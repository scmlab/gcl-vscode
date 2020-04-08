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
      | SendRequest(request) =>
        Js.log("[ send request ]");
        state
        ->State.sendRequest(request)
        ->Promise.flatMap(
            fun
            | Error(error) => {
                let (_header, _body) = Sig.Error.toString(error);
                Js.log3("[ send request error ]", _header, _body);
                // state |> State.displayError(header, body);
                Promise.resolved();
              }
            | Ok(x) => {
                Js.log(x);
                Promise.resolved();
              },
          );
      | Display(header, body) =>
        Js.log2(header, body);
        Promise.resolved();
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