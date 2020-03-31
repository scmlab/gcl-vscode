open Vscode;
open Belt;

module Impl = (Interface: Editor.Interface) => {
  module Connection = Connection.Impl(Interface);

  type t = {
    editor: Interface.editor,
    editor2: Interface.t,
    mutable connection: option(Connection.t),
    mutable panel: option(WebviewPanel.t),
  };

  let make = (context, editor) => {
    editor,
    editor2: Interface.make(editor, context),
    connection: None,
    panel: None,
  };

  let destroy = state =>
    state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);

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

  // connect if not connected yet
  let establishConnection =
      (state): Promise.t(result(Connection.t, Error.t)) => {
    switch (state.connection) {
    | None =>
      Connection.make()
      ->Promise.mapError(e => Error.Connection(e))
      ->Promise.tapOk(conn => state.connection = Some(conn))
    | Some(connection) => Promise.resolved(Ok(connection))
    };
  };
};

include Impl(VscodeImpl);