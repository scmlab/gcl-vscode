module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task__Command.Impl(Editor);
  module TaskResponse = Task__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  type t = Event.t(Task.t);

  let runTask = (task: Task.t, state: State.t): Promise.t(list(Task.t)) =>
    switch (task) {
    | Task.WithState(callback) => callback(state)
    // ->Promise.flatMap(tasks => run(tasks, state))
    | Connect =>
      state
      ->State.connect
      ->Promise.map(
          fun
          | Error(e) => {
              let (header, body) = Sig.Error.toString(e);
              [Task.Display(Error(header), Plain(body))];
            }
          | Ok(_c) => {
              TaskCommand.dispatch(Command.Reload);
            },
        )
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
      Promise.resolved([]);
    | MarkSpec(spec) =>
      open! Editor;

      let range = Range.fromLoc(spec.loc);

      let startPoint = Range.start(range);
      let endPoint = Range.end_(range);

      // range of {!
      let startRange =
        Range.make(startPoint, Point.translate(startPoint, 0, 2));
      // range of !}
      let endRange = Range.make(Point.translate(endPoint, 0, -2), endPoint);

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
      Promise.resolved([]);
    | DigHole(site) =>
      let range =
        Response.Error.Site.toRange(
          site,
          state.specifications,
          Editor.Range.fromLoc,
        );
      state.editor->Editor.Decoration.digHole(range);
      Promise.resolved([]);
    | RemoveDecorations =>
      state.decorations->Array.forEach(Editor.Decoration.destroy);
      Promise.resolved([]);

    | DispatchCommand(command) =>
      Js.log2("[ dispatch command ]", command);
      TaskCommand.dispatch(command)->Promise.resolved;
    | SendRequest(request) =>
      Js.log("[ send request ]");
      state
      ->State.sendRequest(request)
      ->Promise.map(
          fun
          | Error(error) => {
              let (header, body) = Sig.Error.toString(error);
              [Task.Display(Error(header), Plain(body))];
            }
          | Ok(response) => TaskResponse.handle(response),
        );
    | Display(header, body) =>
      state->State.display(header, body)->Promise.map(_ => [])
    };

  let addTask = (emitter: Event.t(Task.t), task) => emitter.emit(task);

  let make = state => {
    // task queue, FIFO
    let queue: array(Task.t) = [||];
    let emitter = Event.make();
    // semaphore for indicating that if `runTasksInQueue` is running
    let busy = ref(false);

    // keep executing tasks in the queue until depleting it
    let rec runTasksInQueue = () => {
      let nextTask = Js.Array.shift(queue);
      switch (nextTask) {
      | None => ()
      | Some(task) =>
        runTask(task, state)
        ->Promise.get(newTasks => {
            newTasks->List.forEach(addTask(emitter));
            runTasksInQueue();
          })
      };
    };

    // we are ignoring the destructor
    // because it's going to be destroyed along with the emitter anyway
    let _ =
      emitter.on(task => {
        Js.Array.push(task, queue)->ignore;
        if (! busy^) {
          runTasksInQueue();
        };
      });

    emitter;
  };

  let destroy = (emitter: Event.t(Task.t)) => emitter.destroy();
};