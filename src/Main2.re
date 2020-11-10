open VSCode;

// Options to control the language client
module LanguageClientOptions = {
  type t;
  let make: (DocumentSelector.t, FileSystemWatcher.t) => t = [%raw
    "function (documentSelector, synchronize) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize
      }
    }"
  ];
};

// Options to control the language client
module ServerOptions = {
  type t;
  let makeCommand: string => t = [%raw
    "function (command) {
      return { command: command }
    }"
  ];
};
module LanguageClient = {
  type t;
  // constructor
  [@bs.module "vscode-languageclient"] [@bs.new]
  external make:
    (string, string, ServerOptions.t, LanguageClientOptions.t) => t =
    "LanguageClient";
  // methods
  [@bs.send] external start: t => unit = "start";
  [@bs.send] external stop: t => unit = "stop";
  [@bs.send] external onReady: t => Promise.t(unit) = "onReady";
  [@bs.send]
  external onNotification: (t, string, 'a => unit) => Disposable.t =
    "onNotification";
  [@bs.send]
  external sendNotification: (t, string, 'a) => unit = "sendNotification";
  [@bs.send]
  external sendRequest: (t, string, 'a) => Promise.t('r) = "sendRequest";
};

let clientHandle = ref(None);

let activate = (context: ExtensionContext.t) => {
  let serverOptions = ServerOptions.makeCommand("gcl");

  // Register the server for plain text documents
  let documentSelector: DocumentSelector.t = [|
    DocumentFilterOrString.documentFilter(
      DocumentFilter.{
        scheme: Some("file"),
        pattern: None,
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

  let clientOptions =
    LanguageClientOptions.make(documentSelector, synchronize);

  // Create the language client and start the client.
  let client =
    LanguageClient.make(
      "languageServerExample",
      "Language Server Example",
      serverOptions,
      clientOptions,
    );
  clientHandle := Some(client);

  // custom method
  LanguageClient.onReady(client)
  ->Promise.get(() => {
      client
      ->LanguageClient.sendRequest("guacamole/load", 3)
      ->Promise.get(result => {Js.log(result)})
    });

  // Start the client. This will also launch the server
  LanguageClient.start(client);
};

let deactivate = {
  (clientHandle^)->Belt.Option.forEach(LanguageClient.stop);
};
