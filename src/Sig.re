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
module type Disposable = {
  type t;
  let make: (unit => unit) => t;
  let dispose: t => unit;
};

module type Editor = {
  type editor;
  type context;
  module Disposable: Disposable;
  type view;
  type fileName = string;

  module Point: {
    type t;
    let line: t => int;
    let column: t => int;
    let make: (int, int) => t;
    let translate: (t, int, int) => t;

    let fromPos: GCL.pos => t;
    let toPos: (t, fileName) => GCL.pos;
  };

  module Range: {
    type t;
    let make: (Point.t, Point.t) => t;
    let start: t => Point.t;
    let end_: t => Point.t;

    let fromLoc: GCL.loc => t;
    let toLoc: (t, fileName) => GCL.loc;

    let contains: (t, Point.t) => bool;
    let containsRange: (t, t) => bool;
  };

  let editorType: editorType;

  // Helpers
  let getExtensionPath: context => fileName;
  let getFileName: editor => option(fileName);
  let save: editor => Promise.t(bool);

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

  module Decoration: {
    type t;
    type kind =
      | Error
      | Highlight
      | Spec;

    let digHole: (editor, Range.t) => unit;
    let highlightBackground: (editor, kind, Range.t) => array(t);
    let overlayText: (editor, kind, string, Range.t) => array(t);
    let destroy: t => unit;
  };

  let getCursorPosition: editor => Point.t;
  // let setCursor: (editor, Point.t) => unit;

  let rangeForLine: (editor, int) => Range.t;

  let getText: (editor, Range.t) => string;
  let selectText: (editor, Range.t) => unit;
  let insertText: (editor, Point.t, string) => Promise.t(bool);
  let deleteText: (editor, Range.t) => Promise.t(bool);
};