type t = {client: LSP.LanguageClient.t};

let make = () => {
  let setupLSPClient = () => {
    open LSP;
    open VSCode;
    let serverOptions = ServerOptions.makeCommand("gcl");

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

  client
  ->LSP.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.getOk(() => {
      client
      ->LSP.LanguageClient.onNotification("guacamole/pos", result => {
          Js.log2("pos >>>", result);

          // catching exceptions occured when decoding JSON values
          switch (Response.ProofObligation.decode(result)) {
          | value => Js.log(value)
          | exception (Json.Decode.DecodeError(msg)) => Js.log(msg)
          // Promise.resolved(Error(Error.Decode(msg, result)))
          };
          ();
        })
      ->ignore
    });

  {client: client};
};

let destroy = state => {
  state.client->LSP.LanguageClient.stop;
};
