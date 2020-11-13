type t = {client: LSP.LanguageClient.t};

let make = () => {
  let setupLSPClient = () => {
    open LSP;
    open VSCode;
    let serverOptions = ServerOptions.makeCommand("lsp-demo-reactor-server");
    // let serverOptions = ServerOptions.makeCommand("gcl");

    let clientOptions = {
      // let makePattern = [%raw "function(filename) { return fileName }"];
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [|
        DocumentFilterOrString.documentFilter(
          DocumentFilter.{
            scheme: Some("file"),
            pattern: None,
            // Some(makePattern(fileName)),
            language: Some("guacamole"),
          },
        ),
      |];

      // Notify the server about file changes to '.clientrc files contained in the workspace
      let synchronize: FileSystemWatcher.t =
        Workspace.createFileSystemWatcher(
          [%raw "'**/.clientrc'"],
          ~ignoreCreateEvents=false,
          ~ignoreChangeEvents=false,
          ~ignoreDeleteEvents=false,
        );
      LanguageClientOptions.make(documentSelector, synchronize);
    };
    // Create the language client
    LanguageClient.make(
      "guacamoleLanguageServer",
      "Guacamole Language Server",
      serverOptions,
      clientOptions,
    );
  };
  let client = setupLSPClient();
  client->LSP.LanguageClient.start;
  {client: client};
};

let destroy = state => {
  state.client->LSP.LanguageClient.stop;
};
