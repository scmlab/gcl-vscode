type editorType =
  | Atom
  | VsCode;

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

module type Editor = {
  type editor;
  type context;
  module Disposable: {
    type t;
    let make: (unit => unit) => t;
    let dispose: t => unit;
  };
  type view;
  type point;
  type range;
  type fileName = string;

  let editorType: editorType;

  // Helpers
  let getExtensionPath: context => fileName;
  let getFileName: editor => option(fileName);
  let save: editor => Promise.t(bool);

  let toPoint: GCL.pos => point;
  let fromPoint: (fileName, point) => GCL.pos;

  let toRange: GCL.loc => range;
  let fromRange: (fileName, range) => GCL.loc;

  // Events
  let onDidChangeFileName:
    ((option(fileName), option(fileName)) => unit) => Disposable.t;
  let onDidChangeActivation:
    ((option(fileName), option(fileName)) => unit) => Disposable.t;
  let onDidCloseEditor: (fileName => unit) => Disposable.t;
  let registerCommand: (string, editor => unit) => Disposable.t;

  // Subscriptions
  let addToSubscriptions: (Disposable.t, context) => unit;

  module Config: {
    // Configurations
    let getGCLPath: unit => option(fileName);
    let setGCLPath: fileName => Promise.t(unit);
  };

  module View: {
    // construction/destruction
    let make: (context, editor) => view;
    let destroy: view => unit;
    // show/hide
    let show: view => unit;
    let hide: view => unit;
    // messaging
    let send: (view, View.Request.t) => Promise.t(bool);
    let recv: (view, View.Response.t => unit) => Disposable.t;
  };

  let digHole: (editor, range) => Promise.t(unit);
  let decorateBackground: editor => unit;
  /* let decorateWithTex*/
};