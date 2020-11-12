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
  [@bs.send] external onReady: t => Promise.Js.t(unit, _) = "onReady";
  [@bs.send]
  external onNotification: (t, string, 'a => unit) => Disposable.t =
    "onNotification";
  [@bs.send]
  external sendNotification: (t, string, 'a) => unit = "sendNotification";
  [@bs.send]
  external sendRequest: (t, string, Js.Json.t) => Promise.Js.t('result, _) =
    "sendRequest";
};

let make = fileName => {
  let serverOptions = ServerOptions.makeCommand("gcl");

  let clientOptions = {
    let makePattern = [%raw "function(filename) { return fileName }"];
    // Register the server for plain text documents
    let documentSelector: DocumentSelector.t = [|
      DocumentFilterOrString.documentFilter(
        DocumentFilter.{
          scheme: Some("file"),
          pattern: Some(makePattern(fileName)),
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
  // custom method
  // let registerCommand = (name, callback) =>
  //   Commands.registerCommand("guacamole." ++ name, () =>
  //     Window.activeTextEditor->Option.map(editor => {
  //       let fileName = editor->TextEditor.document->TextDocument.fileName;
  //       callback(editor, fileName);
  //     })
  //   );
  // registerCommand("toggle", (editor, fileName) => {
  //   client
  //   ->onReady
  //   ->Promise.get(() => {
  //       client
  //       ->sendRequest("guacamole/load", fileName)
  //       ->Promise.get(result => {Js.log(result)})
  //     })
  //   // switch (Registry.get(fileName)) {
  //   // | None =>
  //   //   // not in the Registry, instantiate one new
  //   //   let state = State.make(extensionPath, editor);
  //   //   let taskRunner = TaskRunner.make(state);
  //   //   // remove it from the Registry if it got destroyed
  //   //   state
  //   //   ->State.onDestroy(() => {Registry.remove(fileName)})
  //   //   ->Js.Array.push(disposables)
  //   //   ->ignore;
  //   //   Registry.add(fileName, (state, taskRunner));
  //   //   state.view
  //   //   ->View.recv(response => {
  //   //       TaskRunner.addTask(taskRunner, ViewResponse(response))
  //   //     })
  //   //   ->Js.Array.push(disposables)
  //   //   ->ignore;
  //   //   // dispatch "Load"
  //   //   taskRunner->TaskRunner.addTask(DispatchCommand(Load));
  //   // | Some((_state, taskRunner)) =>
  //   //   // already in the Registry, dispatch "Quit"
  //   //   // and remove and destroy it from the Registry
  //   //   taskRunner->TaskRunner.addTask(DispatchCommand(Quit));
  //   //   fileName->Registry.destroy;
  //   // }
  // })
  // ->Js.Array.push(disposables)
  // ->ignore;
  // Start the client. This will also launch the server
  // client->start;
};

// let deactivate = {
//   (clientHandle^)->Belt.Option.forEach(LanguageClient.stop);
// };
