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
  let getFileName: editor => fileName;

  let toPoint: View.Pos.t => point;
  let fromPoint: (fileName, point) => View.Pos.t;

  let toRange: View.Loc.t => range;
  let fromRange: (fileName, range) => View.Loc.t;

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
    // messagin
    let recv: (view, View.Response.t => unit) => unit;
  };
};