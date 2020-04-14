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
  type disposable;
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
    ((option(fileName), option(fileName)) => unit) => disposable;
  let onDidChangeActivation:
    ((option(fileName), option(fileName)) => unit) => disposable;
  let onDidCloseEditor: (fileName => unit) => disposable;
  let registerCommand: (string, editor => unit) => disposable;

  // Subscriptions
  let addToSubscriptions: (disposable, context) => unit;

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
    let recv: (view, View.Response.t => unit) => disposable;
  };
};

module type State =
  (Editor: Editor) =>
   {
    // signature for the States module to construct/destruct State.t
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
    let connect: t => Promise.t(result(Connection.t, Error.t));
    let disconnect: t => Promise.t(unit);
    let sendRequest:
      (t, Types.Request.t) => Promise.t(result(GCL.Response.t, Error.t));

    // view related
    let show: t => unit;
    let hide: t => unit;
    let display:
      (t, View.Request.Header.t, View.Request.Body.t) => Promise.t(bool);
  };