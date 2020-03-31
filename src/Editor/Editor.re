module type Interface = {
  type editor;
  type context;
  type disposable;

  type t = {
    editor,
    context,
  };

  let make: (editor, context) => t;

  let getExtensionPath: t => string;

  let getFileName': editor => string;
  // let getActiveEditor: unit => option(editor);

  let onDidChangeFileName: ((string, string) => unit) => disposable;
  let onDidCloseEditor: (string => unit) => disposable;

  let addToSubscriptions: (disposable, context) => unit;

  //   let getConfiguration: string => option('a);
  //   let setConfiguration: (string, 'a) => Promise.t(unit);
  let getGCLPath: unit => option(string);
  let setGCLPath: string => Promise.t(unit);
};