open Belt;

type t = {
  client: LSP.LanguageClient.t,
  editor: VSCode.TextEditor.t,
  document: VSCode.TextDocument.t,
  filePath: string,
  mutable view: option(View.t),
};

let make = editor => {
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
  // start the LSP client
  client->LSP.LanguageClient.start;
  let document = VSCode.TextEditor.document(editor);
  let filePath = VSCode.TextDocument.fileName(document);
  let state = {client, editor, document, filePath, view: None};

  client
  ->LSP.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.getOk(() => {
      client
      ->LSP.LanguageClient.onNotification("guacamole/pos", result => {
          Js.log2("pos >>>", result);
          // catch exceptions occured from decoding JSON values
          switch (result |> Response.decode) {
          | OK(_, pos, _, _) =>
            state.view
            ->Option.forEach(view => {
                View.send(
                  view,
                  ViewType.Request.Display(
                    Plain("Proof Obligations"),
                    ProofObligations(0, pos, [||]),
                  ),
                )
                ->ignore
              })
          | others => Js.log(others)
          | exception (Json.Decode.DecodeError(msg)) => Js.log(msg)
          // Promise.resolved(Error(Error.Decode(msg, result)))
          };
        })
      ->ignore
    });

  state;
};

let sendRequest = (state, request) => {
  let value = Request.encode(request);
  Js.log2("<<<", value);

  state.client
  ->LSP.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.flatMapOk(() => {
      state.client
      ->LSP.LanguageClient.sendRequest("guacamole", value)
      ->Promise.Js.toResult
    })
  ->Promise.flatMapOk(result => {
      Js.log2(">>>", result);
      // catching exceptions occured when decoding JSON values
      switch (result |> Response.decode) {
      | value => Promise.resolved(Ok(value))
      | exception (Json.Decode.DecodeError(msg)) =>
        Js.log(msg);
        Promise.resolved(Error());
      // Promise.resolved(Error(Error.Decode(msg, result)))
      };
    });
  // ->Promise.mapError(error => {Error.LSP(Error.fromJsError(error))});
};

let destroy = state => {
  state.client->LSP.LanguageClient.stop;
};
