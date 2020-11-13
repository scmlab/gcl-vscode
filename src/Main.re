open Belt;

// let clientHandle = ref(None);

module Handler = {
  let onOpen = doc => {
    let fileName = VSCode.TextDocument.fileName(doc);
    let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
    // filter out ".gcl.git" files
    if (isGCL(fileName)) {
      Js.log("GCL " ++ fileName);

      let state = State.make();
      Registry.add(fileName, state);
    };
  };
};

let activate = (context: VSCode.ExtensionContext.t) => {
  VSCode.Workspace.onDidOpenTextDocument(. Handler.onOpen)
  ->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))
  ->ignore;
  VSCode.Workspace.textDocuments->Array.forEach(Handler.onOpen);
  // (clientHandle^)->Belt.Option.forEach(LSP.LanguageClient.start);
};

let deactivate = () => {
  ();
    // (clientHandle^)->Belt.Option.forEach(LSP.LanguageClient.stop);
};
