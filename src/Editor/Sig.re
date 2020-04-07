module type Editor = {
  type editor;
  type context;
  type disposable;
  type view;
  type point;
  type range;
  type fileName = string;

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
    let send: (view, View.Request.t) => unit;
    let recv: (view, View.Response.t => unit) => unit;
  };
};