module Error = {
  type t =
    | Connection(Connection.Error.t)
    | Decode(string, Js.Json.t);
  let toString =
    fun
    | Connection(e) => Connection.Error.toString(e)
    | Decode(msg, json) => (
        {js|JSON Decode Error|js},
        msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
      );
};

// signature for the States module to construct/destruct State.t
module type Sig =
  ( Editor: Sig.Editor) =>
   {
    // types
    type editor = Editor.editor;
    type context = Editor.context;
    type t;

    // getters
    let getEditor: t => editor;

    // construction/destruction
    let make: (context, editor) => t;
    let destroy: t => Promise.t(unit);

    // connection/disconnection to GCL
    let connect: t => Promise.t(result(Connection.t, Error.t));
    let disconnect: t => Promise.t(unit);
  };

module Impl: Sig =
  (Editor: Sig.Editor) => {
    type editor = Editor.editor;
    type context = Editor.context;
    type t = {
      editor,
      context,
      mutable connection: option(Connection.t),
      mutable view: Editor.view,
    };

    //
    // getters
    //
    let getEditor = (state: t) => state.editor;

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

    //
    // View show/hide
    //

    //
    // construction/destruction
    //

    let make = (context, editor) => {
      // view initialization
      let view = Editor.View.make(context, editor);
      let state = {editor, context, connection: None, view};

      // connection initialization
      state
      ->connect
      ->Promise.get(
          fun
          | Error(e) => Js.log2("[ connection error ]", Error.toString(e))
          | Ok(c) => Js.log2("[ connection success ]", c),
        );

      state;
    };

    let destroy = state => {
      state.view->Editor.View.destroy;
      state->disconnect;
    };
  };