module type Interface = {
  type editor;

  // let getActiveEditor: unit => option(editor);
  let getFileName: editor => string;

  //   let getConfiguration: string => option('a);
  //   let setConfiguration: (string, 'a) => Promise.t(unit);
  let getGCLPath: unit => option(string);
  let setGCLPath: string => Promise.t(unit);
};