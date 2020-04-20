module Impl = (Editor: Sig.Editor) => {
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable mode: View.Response.mode,
    mutable decorations: array(unit),
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
    state.onDestroyEventEmitter.emit();
    state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.destroy();
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

    // update the state on receiving message from the view
    view
    ->Editor.View.recv(
        fun
        | View.Response.SetMode(mode) => state.mode = mode
        | Link(_) => ()
        | Initialized => ()
        | Destroyed => {
            Js.log("destroyed!!");
            destroy(state)->ignore;
          },
      )
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