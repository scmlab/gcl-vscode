module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task__Command.Impl(Editor);
  module TaskResponse = Task__Response.Impl(Editor);
  module TaskView = Task__View.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  type status =
    | Busy
    | Idle;

  type t = {
    taskEmitter: AgdaModeVscode.Event.t(Task.t),
    mutable status,
    statusEmitter: AgdaModeVscode.Event.t(status),
  };

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
              let (header, body) = Error.toString(e);
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
            postCondText,
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
              let (header, body) = Error.toString(error);
              [Task.Display(Error(header), Plain(body))];
            }
          | Ok(response) => TaskResponse.handle(response),
        );
    | Display(header, body) =>
      state->State.display(header, body)->Promise.map(_ => [])
    | ViewRequest(request) =>
      state->State.sendRequestToView(request)->Promise.map(_ => [])
    | ViewResponse(response) =>
      TaskView.handle(state.editor, response)->Promise.resolved
    };

  let addTask = (self: t, task) => self.taskEmitter.emit(task);

  let make = state => {
    // task queue, FIFO
    let queue: array(Task.t) = [||];
    let taskEmitter = AgdaModeVscode.Event.make();
    let statusEmitter = AgdaModeVscode.Event.make();
    let self = {taskEmitter, status: Idle, statusEmitter};

    // keep executing tasks in the queue until depleting it
    let rec runTasksInQueue = () => {
      let nextTask = Js.Array.shift(queue);
      switch (nextTask) {
      | None =>
        self.status = Idle;
        self.statusEmitter.emit(Idle);
      | Some(task) =>
        runTask(task, state)
        ->Promise.get(newTasks => {
            newTasks->List.forEach(addTask(self));
            runTasksInQueue();
          })
      };
    };

    let _ =
      taskEmitter.on(task => {
        Js.Array.push(task, queue)->ignore;
        // kick start `runTasksInQueue` if it's not already running
        if (self.status == Idle) {
          self.status = Busy;
          self.statusEmitter.emit(Busy);
          runTasksInQueue();
        };
      });

    self;
  };
  // destroy only after all tasks have been executed
  let destroy = (self: t): Promise.t(unit) => {
    let (promise, resolve) = Promise.pending();
    let destroy' = () => {
      self.statusEmitter.destroy();
      self.taskEmitter.destroy();
      resolve();
    };

    switch (self.status) {
    | Idle => destroy'()
    | Busy =>
      let _ =
        self.statusEmitter.on(
          fun
          | Idle => destroy'()
          | Busy => (),
        );
      ();
    };
    promise;
  };
};
