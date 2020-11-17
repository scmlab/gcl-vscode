open Belt;

type t = {
  client: LSP.LanguageClient.t,
  editor: VSCode.TextEditor.t,
  document: VSCode.TextDocument.t,
  filePath: string,
  mutable view: option(View.t),
};

let handleResponse = (state: t, response) =>
  switch (response) {
  | Response.Error(error) => Js.log(error)
  | OK(i, pos, _, props) =>
    state.view
    ->Option.forEach(view => {
        View.send(
          view,
          ViewType.Request.Display(
            Plain("Proof Obligations"),
            ProofObligations(i, pos, props),
          ),
        )
        ->ignore
      })
  | Decorate(locs) => Js.log(locs)
  | _ => ()
  };

let decodeResponse = (json: Js.Json.t): Response.t => {
  // catching exceptions occured when decoding JSON values
  switch (Response.decode(json)) {
  | response => response
  | exception (Json.Decode.DecodeError(msg)) =>
    Error([|Error(Global(NoLoc), CannotDecodeRequest(msg))|])
  };
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
      ->LSP.LanguageClient.onNotification("guacamole", json => {
          Js.log2(">>>", json);
          let response = decodeResponse(json);
          handleResponse(state, response);
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
  ->Promise.flatMapOk(json => {
      Js.log2(">>>", json);

      let response = decodeResponse(json);
      Promise.resolved(Ok(response));
    });
  // ->Promise.mapError(error => {Error.LSP(Error.fromJsError(error))});
};

let destroy = state => {
  state.client->LSP.LanguageClient.stop;
};
