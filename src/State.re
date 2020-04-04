open Vscode;
open Belt;

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
  (Editor: Editor.Sig) =>
   {
    // types
    type editor = Editor.editor;
    type context = Editor.context;
    type t;

    // construction/destruction
    let make: (context, editor) => t;
    let destroy: t => Promise.t(unit);

    // connection/disconnection to GCL
    let connect: t => Promise.t(result(Connection.t, Error.t));
    let disconnect: t => Promise.t(unit);
  };

module Impl: Sig =
  (Editor: Editor.Sig) => {
    type editor = Editor.editor;
    type context = Editor.context;
    type t = {
      editor: Editor.t,
      mutable connection: option(Connection.t),
      mutable panel: option(WebviewPanel.t),
    };

    //
    // connection/disconnection to GCL
    //

    // connect if not connected yet
    let connect = state =>
      switch (state.connection) {
      | None =>
        Connection.make(Editor.getGCLPath, Editor.setGCLPath)
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
    // construction/destruction
    //

    let make = (context, editor) => {
      editor: Editor.make(editor, context),
      connection: None,
      panel: None,
    };

    let destroy = state => {
      state.panel
      ->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);
      Promise.resolved();
    };
  };