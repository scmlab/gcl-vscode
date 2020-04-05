module type Editor = {
  type editor;
  type context;
  type disposable;
  type view;
  type fileName = string;

  // Helpers
  let getExtensionPath: context => fileName;
  let getFileName: editor => fileName;

  // Events
  let onDidChangeFileName:
    ((option(fileName), option(fileName)) => unit) => disposable;
  let onDidChangeActivation:
    ((option(fileName), option(fileName)) => unit) => disposable;
  let onDidCloseEditor: (fileName => unit) => disposable;
  let registerCommand: (string, editor => unit) => disposable;

  // Configurations
  let getGCLPath: unit => option(fileName);
  let setGCLPath: fileName => Promise.t(unit);

  // Subscriptions
  let addToSubscriptions: (disposable, context) => unit;

  // View
  let createView: (context, editor) => view;
  let destroyView: view => unit;
};

module type View =
  (Editor: Editor) =>
   {
    // construction/destruction
    let make: (Editor.context, Editor.editor) => Editor.view;
    let destroy: Editor.view => unit;
  };