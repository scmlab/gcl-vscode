module Impl = (Editor: API.Editor) => {
  open Belt;
  type editor = Editor.editor;
  type t = {
    editor,
    view: Editor.view,
    mutable mode: GCL.mode,
    mutable decorations: array(Editor.Decoration.t),
    mutable specifications: array(Response.Specification.t),
    mutable connection: option(Connection.t),
    onDestroyEventEmitter: AgdaModeVscode.Event.t(unit),
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
      ->Promise.mapError(e => Error.Connection(e))
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

    state
    ->connect
    ->Promise.flatMapOk(conn => {
        Connection.send(value, conn)
        ->Promise.mapError(e => Error.Connection(e))
      })
    ->Promise.flatMapOk(result => {
        Js.log2(
          ">>>",
          Js.String.substring(~from=0, ~to_=200, Js.Json.stringify(result)),
        );

        // catching exceptions occured when decoding JSON values
        switch (result |> Response.decode) {
        | value => Promise.resolved(Ok(value))
        | exception (Json.Decode.DecodeError(msg)) =>
          Promise.resolved(Error(Error.Decode(msg, result)))
        };
      });
  };

  //
  // construction/destruction
  //

  let destroy = state => {
    state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.emit();
    state.onDestroyEventEmitter.destroy();
    state.decorations->Array.forEach(Editor.Decoration.destroy);
    state->disconnect;
  };

  let make = (extentionPath, editor) => {
    // view initialization
    let view = Editor.View.make(extentionPath, editor);

    let state = {
      editor,
      view,
      mode: GCL.WP1,
      decorations: [||],
      specifications: [||],
      connection: None,
      onDestroyEventEmitter: AgdaModeVscode.Event.make(),
    };

    state;
  };

  //
  // View-related
  //

  let show = state => state.view->Editor.View.show;
  let hide = state => state.view->Editor.View.hide;
  let sendRequestToView = (state, request) => {
    Editor.View.send(state.view, request);
  };
  let display = (state, header, body) => {
    sendRequestToView(state, View.Request.Display(header, body));
  };
  //
  // Spec-related
  //

  module Spec = {
    module GCLImpl = GCLImpl.Impl(Editor);
    // find the hole containing the cursor
    let fromCursorPosition = state => {
      let cursor = Editor.getCursorPosition(state.editor);
      // find the smallest hole containing the cursor, as there might be many of them
      let smallestHole = ref(None);
      state.specifications
      ->Array.keep(spec => {
          let range = GCLImpl.Loc.toRange(spec.loc);
          Editor.Range.contains(range, cursor);
        })
      ->Array.forEach(spec =>
          switch (smallestHole^) {
          | None => smallestHole := Some(spec)
          | Some(spec') =>
            if (Editor.Range.containsRange(
                  GCLImpl.Loc.toRange(spec.loc),
                  GCLImpl.Loc.toRange(spec'.loc),
                )) {
              smallestHole := Some(spec);
            }
          }
        );
      smallestHole^;
    };

    let getPayloadRange = (doc, spec: Response.Specification.t) => {
      open! Editor;
      let range = GCLImpl.Loc.toRange(spec.loc);
      let startingLine = Point.line(Range.start(range)) + 1;
      let endingLine = Point.line(Range.end_(range)) - 1;

      let start = Editor.rangeForLine(doc, startingLine)->Range.start;
      let end_ = Editor.rangeForLine(doc, endingLine)->Range.end_;
      Range.make(start, end_);
    };
    let getPayload = (doc, spec) => {
      // return the text in the targeted hole
      let innerRange = getPayloadRange(doc, spec);
      Editor.getTextInRange(doc, innerRange);
    };

    let resolve = (state, i) => {
      let specs = state.specifications->Array.keep(spec => spec.id == i);
      specs[0]
      ->Option.forEach(spec => {
          let doc = Editor.getDocument(state.editor);
          let payload = getPayload(doc, spec);
          let range = GCLImpl.Loc.toRange(spec.loc);
          let start = Editor.Range.start(range);
          doc
          ->Editor.deleteText(range)
          ->Promise.flatMap(
              fun
              | false => Promise.resolved(false)
              | true =>
                doc->Editor.insertText(start, Js.String.trim(payload)),
            )
          ->Promise.get(_ => ());
        });
      Promise.resolved();
    };

    let insert = (state, lineNo, expr) => {
      let assertion = "{ " ++ GCL.Syntax.Expr.toString(expr) ++ " }\n";
      let point = Editor.Point.make(lineNo - 1, 0);
      // insert the assertion
      let doc = Editor.getDocument(state.editor);
      Editor.insertText(doc, point, assertion);
    };
  };
};
