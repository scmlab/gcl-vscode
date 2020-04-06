module type Type = {};

// module type View = {
//   type context;
//   type editor;
//   type view;
//   // construction/destruction
//   let make: (context, editor) => view;
//   let destroy: view => unit;
// };

module type Editor = (Type: Type) => {
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

  module View : {
    // type context;
    // type editor;
    // type view;
    // construction/destruction
    let make: (context, editor) => view;
    let destroy: view => unit;
  };

};

// module type View =
//   (Type: Type, Editor: Editor) =>
//    {
//     // construction/destruction
//     let make: (Editor(Type).context, Editor(Type).editor) => Editor(Type).view;
//     let destroy: Editor(Type).view => unit;
//   };