// signature for the States module to construct/destruct State.t
module type Sig =
  (Editor: Sig.Editor) =>
   {
    // types
    type editor = Editor.editor;
    type context = Editor.context;
    type t;

    // getters/setters
    let getEditor: t => editor;
    let setSpecifications: (t, array(GCL.Response.Specification.t)) => unit;

    // construction/destruction
    let make: (context, editor) => t;
    let destroy: t => Promise.t(unit);

    // connection/disconnection to GCL
    let connect: t => Promise.t(result(Connection.t, Sig.Error.t));
    let disconnect: t => Promise.t(unit);
    let sendRequest:
      (t, Types.Request.t) => Promise.t(result(GCL.Response.t, Sig.Error.t));

    // view related
    let show: t => unit;
    let hide: t => unit;
    let display:
      (t, View.Request.header, View.Request.body) => Promise.t(bool);
  };

module Impl: Sig =
  (Editor: Sig.Editor) => {
    type editor = Editor.editor;
    type context = Editor.context;
    type t = {
      editor,
      context,
      view: Editor.view,
      mutable decorations: array(unit),
      mutable specifications: array(GCL.Response.Specification.t),
      mutable connection: option(Connection.t),
    };

    //
    // getters
    //
    let getEditor = (state: t) => state.editor;
    let setSpecifications = (state, specifications) =>
      state.specifications = specifications;

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
      let value = Types.Request.encode(request);
      Js.log2("<<<", value);

      let%Ok conn = state->connect;
      let%Ok result =
        Guacamole.Connection.send(value, conn)
        ->Promise.mapError(e => Sig.Error.Connection(e));

      Js.log2(">>>", result);

      // catching exceptions occured when decoding JSON values
      switch (Guacamole.GCL.Response.decode(result)) {
      | value => Promise.resolved(Ok(value))
      | exception (Json.Decode.DecodeError(msg)) =>
        Promise.resolved(Error(Sig.Error.Decode(msg, result)))
      };
    };

    //
    // View show/hide
    //

    //
    // construction/destruction
    //

    let make = (context, editor) => {
      // view initialization
      let view = Editor.View.make(context, editor);
      let state = {
        editor,
        context,
        view,
        decorations: [||],
        specifications: [||],
        connection: None,
      };

      // connection initialization
      state
      ->connect
      ->Promise.get(
          fun
          | Error(e) =>
            Js.log2("[ connection error ]", Sig.Error.toString(e))
          | Ok(c) => Js.log2("[ connection success ]", c),
        );

      state;
    };

    let destroy = state => {
      state.view->Editor.View.destroy;
      state->disconnect;
    };

    //
    // View-related
    //

    let show = state => state.view->Editor.View.show;
    let hide = state => state.view->Editor.View.hide;
    let display = (state, header, body) =>
      state.view->Editor.View.send(View.Request.Display(header, body));
  };