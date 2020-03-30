module type Interface = {
  type editor;
  type context;

  type t = {
    editor,
    context,
  };

  let make: (editor, context) => t;

  let getExtensionPath: t => string;
  // let getActiveEditor: unit => option(editor);
  let getFileName: t => string;

  //   let getConfiguration: string => option('a);
  //   let setConfiguration: (string, 'a) => Promise.t(unit);
  let getGCLPath: unit => option(string);
  let setGCLPath: string => Promise.t(unit);
};