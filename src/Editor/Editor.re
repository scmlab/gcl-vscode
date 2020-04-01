module type Interface = {
  type editor;
  type context;
  type disposable;
  type fileName = string;

  type t = {
    editor,
    context,
  };

  let make: (editor, context) => t;

  let getExtensionPath: t => fileName;

  let editorFileName: editor => fileName;

  // if the file name changed, invoke the callback with the previous and next name (could be None)
  let onDidChangeFileName:
    ((option(fileName), option(fileName)) => unit) => disposable;
  let onDidChangeActivation:
    ((option(fileName), option(fileName)) => unit) => disposable;
    
  let onDidCloseEditor: (fileName => unit) => disposable;

  let addToSubscriptions: (disposable, context) => unit;

  let registerCommand: (string, editor => unit) => disposable;

  //   let getConfiguration: string => option('a);
  //   let setConfiguration: (string, 'a) => Promise.t(unit);
  let getGCLPath: unit => option(fileName);
  let setGCLPath: fileName => Promise.t(unit);
};