module Impl = (Editor: Sig.Editor) => {
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable mode: View.Response.mode,
    mutable decorations: array(Editor.Decoration.t),
    mutable specifications: array(GCL.Response.Specification.t),
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
      Guacamole.Connection.send(value, conn)
      ->Promise.mapError(e => Sig.Error.Connection(e));

    Js.log2(
      ">>>",
      Js.String.substring(~from=0, ~to_=200, Js.Json.stringify(result)),
    );

    // catching exceptions occured when decoding JSON values
    switch (Guacamole.GCL.Response.decode(result)) {
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
    state.decorations->Belt.Array.forEach(Editor.Decoration.destroy);
    state->disconnect;
  };

  let make = (context, editor) => {
    // view initialization
    let view = Editor.View.make(context, editor);

    let state = {
      editor,
      context,
      view,
      mode: View.Response.WP1,
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
        let key = Guacamole.GCL.Loc.toString(loc);
        Js.Dict.get(decorationDict, key)
        ->Belt.Option.forEach(decos =>
            decos->Belt.Array.forEach(Editor.Decoration.destroy)
          );
        delete_(key);
      | Link(MouseClick(loc)) =>
        let range = Editor.Range.fromLoc(loc);
        editor->Editor.select(range);
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
};