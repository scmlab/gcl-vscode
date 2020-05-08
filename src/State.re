module Impl = (Editor: Sig.Editor) => {
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable mode: GCL.mode,
    mutable decorations: array(Editor.Decoration.t),
    mutable specifications: array(Response.Specification.t),
    mutable connection: option(Connection.t),
    onDestroyEventEmitter: Event.t(unit),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;
  let setSpecifications = (state, specifications) =>
    state.specifications = specifications;

  //
  // events
  //
  let onDestroy = (state, callback) => {
    state.onDestroyEventEmitter.on(callback)->Editor.Disposable.make;
  };

  //
  // GCL connection/disconnection
  //

  // connect if not connected yet
  let connect = state =>
    switch (state.connection) {
    | None =>
      Connection.make(Editor.Config.getGCLPath, Editor.Config.setGCLPath)
      ->Promise.mapError(e => Sig.Error.Connection(e))
      ->Promise.tapOk(conn => state.connection = Some(conn))
    | Some(connection) => Promise.resolved(Ok(connection))
    };
  let disconnect = state =>
    switch (state.connection) {
    | None => Promise.resolved()
    | Some(connection) => Connection.disconnect(connection)
    };
  let sendRequest = (state, request) => {
    let value = Request.encode(request);
    Js.log2("<<<", value);

    let%Ok conn = state->connect;
    let%Ok result =
      Connection.send(value, conn)
      ->Promise.mapError(e => Sig.Error.Connection(e));

    Js.log2(
      ">>>",
      Js.String.substring(~from=0, ~to_=200, Js.Json.stringify(result)),
    );

    // catching exceptions occured when decoding JSON values
    switch (Response.decode(result)) {
    | value => Promise.resolved(Ok(value))
    | exception (Json.Decode.DecodeError(msg)) =>
      Promise.resolved(Error(Sig.Error.Decode(msg, result)))
    };
  };

  //
  // construction/destruction
  //

  let destroy = state => {
    state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.destroy();
    state.onDestroyEventEmitter.emit();
    state.decorations->Array.forEach(Editor.Decoration.destroy);
    state->disconnect;
  };

  let make = (context, editor) => {
    // view initialization
    let view = Editor.View.make(context, editor);

    let state = {
      editor,
      context,
      view,
      mode: GCL.WP1,
      decorations: [||],
      specifications: [||],
      connection: None,
      onDestroyEventEmitter: Event.make(),
    };

    // a dictionary of decorations for <Link>
    let decorationDict: Js.Dict.t(array(Editor.Decoration.t)) =
      Js.Dict.empty();
    let delete_: string => unit = [%raw
      "function (id) {delete decorationDict[id]}"
    ];

    let onRecvMessageFromView = x =>
      switch (x) {
      | View.Response.SetMode(mode) => state.mode = mode
      | Link(MouseOver(loc)) =>
        let key = GCL.Loc.toString(loc);
        let range = Editor.Range.fromLoc(loc);
        let decoration =
          Editor.Decoration.highlightBackground(editor, Highlight, range);
        Js.Dict.set(decorationDict, key, decoration);
      | Link(MouseOut(loc)) =>
        let key = GCL.Loc.toString(loc);
        Js.Dict.get(decorationDict, key)
        ->Option.forEach(decos =>
            decos->Array.forEach(Editor.Decoration.destroy)
          );
        delete_(key);
      | Link(MouseClick(loc)) =>
        let range = Editor.Range.fromLoc(loc);
        editor->Editor.selectText(range);
      | Reduce(expr, subst) => Js.log("REDUCE!!")
      // let range = Editor.Range.fromLoc(loc);
      // editor->Editor.selectText(range);
      | Initialized => ()
      | Destroyed => destroy(state)->ignore
      };

    // update the state on receiving messages from the view
    view
    ->Editor.View.recv(onRecvMessageFromView)
    ->Editor.addToSubscriptions(context);

    state;
  };

  //
  // View-related
  //

  let show = state => state.view->Editor.View.show;
  let hide = state => state.view->Editor.View.hide;
  let display = (state, header, body) => {
    state.view->Editor.View.send(View.Request.Display(header, body));
  };
  //
  // Spec-related
  //

  module Spec = {
    // find the hole containing the cursor
    let fromCursorPosition = state => {
      let cursor = Editor.getCursorPosition(state.editor);
      // find the smallest hole containing the cursor, as there might be many of them
      let smallestHole = ref(None);
      state.specifications
      ->Array.keep(spec => {
          let range = Editor.Range.fromLoc(spec.loc);
          Editor.Range.contains(range, cursor);
        })
      ->Array.forEach(spec =>
          switch (smallestHole^) {
          | None => smallestHole := Some(spec)
          | Some(spec') =>
            if (Editor.Range.containsRange(
                  Editor.Range.fromLoc(spec.loc),
                  Editor.Range.fromLoc(spec'.loc),
                )) {
              smallestHole := Some(spec);
            }
          }
        );
      smallestHole^;
    };

    let getPayloadRange = (editor, spec: Response.Specification.t) => {
      open! Editor;
      let range = Editor.Range.fromLoc(spec.loc);
      let startingLine = Point.line(Range.start(range)) + 1;
      let endingLine = Point.line(Range.end_(range)) - 1;

      let start = editor->Editor.rangeForLine(startingLine)->Range.start;
      let end_ = editor->Editor.rangeForLine(endingLine)->Range.end_;
      Range.make(start, end_);
    };
    let getPayload = (editor, spec) => {
      // return the text in the targeted hole
      let innerRange = getPayloadRange(editor, spec);
      editor->Editor.getText(innerRange);
    };

    let resolve = (state, i) => {
      let specs = state.specifications->Array.keep(spec => spec.id == i);
      specs[0]
      ->Option.forEach(spec => {
          let payload = getPayload(state.editor, spec);
          let range = Editor.Range.fromLoc(spec.loc);
          let start = Editor.Range.start(range);
          state.editor
          ->Editor.deleteText(range)
          ->Promise.flatMap(
              fun
              | false => Promise.resolved(false)
              | true =>
                state.editor
                ->Editor.insertText(start, Js.String.trim(payload)),
            )
          ->Promise.get(_ => ());
        });
      Promise.resolved();
    };

    let insert = (state, lineNo, expr) => {
      let assertion = "{ " ++ GCL.Syntax.Expr.toString(expr) ++ " }\n";
      let point = Editor.Point.make(lineNo - 1, 0);
      // insert the assertion
      Editor.insertText(state.editor, point, assertion);
    };
  };
};