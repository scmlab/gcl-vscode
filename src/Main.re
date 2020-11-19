open Belt;

let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

module Client: {
  type t;
  let start: unit => unit;
  let stop: unit => unit;
  let onNotification: (string, Response.t => unit) => unit;
  let sendRequest: (string, Request.t) => Promise.t(Response.t);
} = {
  type t = LSP.LanguageClient.t;
  let make = () => {
    open LSP;
    open VSCode;
    let serverOptions = ServerOptions.makeCommand("gcl");

    let clientOptions = {
      // let makePattern = [%raw "function(filename) { return fileName }"];
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [|
        StringOr.others(
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
  let client = ref(make());

  // start the LSP client
  let start = () => (client^)->LSP.LanguageClient.start;
  // stop the LSP client
  let stop = () => (client^)->LSP.LanguageClient.stop;

  let decodeResponse = (filePath, json: Js.Json.t): Response.t => {
    // catching exceptions occured when decoding JSON values
    switch (Response.decode(json)) {
    | response => response
    | exception (Json.Decode.DecodeError(msg)) =>
      Res(
        filePath,
        Error([|Error(Others, CannotDecodeResponse(msg, json))|]),
      )
    };
  };

  let onNotification = (filePath, handler) => {
    (client^)
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.getOk(() => {
        (client^)
        ->LSP.LanguageClient.onNotification("guacamole", json => {
            handler(decodeResponse(filePath, json))
          })
      });
  };

  let sendRequest = (filePath, request) => {
    (client^)
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
        let value = Request.encode(request);
        (client^)
        ->LSP.LanguageClient.sendRequest("guacamole", value)
        ->Promise.Js.toResult;
      })
    ->Promise.map(
        fun
        | Ok(json) => decodeResponse(filePath, json)
        | Error(error) =>
          Response.Res(
            filePath,
            Response.Kind.Error([|
              Error(
                Others,
                CannotSendRequest(Response.Error.fromJsError(error)),
              ),
            |]),
          ),
      );
  };
};

module Handler = {
  let onSelect = event => {
    let selections = event->VSCode.TextEditorSelectionChangeEvent.selections;
    let editor = event->VSCode.TextEditorSelectionChangeEvent.textEditor;
    let filePath =
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName;

    Registry.get(filePath)
    ->Option.forEach(state => {
        // TODO, there may be multiple selections at once
        selections[0]
        ->Option.forEach(selection => {
            let start =
              VSCode.TextDocument.offsetAt(
                state.document,
                VSCode.Selection.start(selection),
              );
            let end_ =
              VSCode.TextDocument.offsetAt(
                state.document,
                VSCode.Selection.end_(selection),
              );
            ();

            Client.sendRequest(
              state.filePath,
              Req(state.filePath, Inspect(start, end_)),
            )
            ->Promise.get(State.handleResponse(state));
          })
      });
  };

  let onActivateExtension = callback => {
    // number of visible GCL file in the workplace
    let visibleCount =
      VSCode.Window.visibleTextEditors
      ->Array.keep(editor =>
          isGCL(
            editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
          )
        )
      ->Array.length;
    // should activate the view when there's a visible GCL file
    let shouldAcitvateView = visibleCount > 0 && !View.isActivated();

    if (shouldAcitvateView) {
      callback();
    };
  };

  let onDeactivateExtension = callback => {
    // number of GCL States in the Registry
    let openedCount = Registry.size();
    // should deacitvate the view when all GCL States have been destroyed
    let shouldDeacitvateView = openedCount === 0 && View.isActivated();

    if (shouldDeacitvateView) {
      callback();
    };
  };

  let onOpenEditor = (context, editor) => {
    let filePath =
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName;
    // filter out ".gcl.git" files
    if (isGCL(filePath)) {
      switch (Registry.get(filePath)) {
      | None =>
        // state initialization
        let state = State.make(editor);
        View.wire(state);
        Registry.add(filePath, state);
      | Some(state) => View.wire(state)
      };

      onActivateExtension(() => {
        View.activate(context->VSCode.ExtensionContext.extensionPath);
        Registry.get(filePath)->Option.forEach(View.wire);
        Client.start();
      });
    };
  };

  let onCloseEditor = doc => {
    let filePath = VSCode.TextDocument.fileName(doc);
    if (isGCL(filePath)) {
      Registry.destroy(filePath);
      onDeactivateExtension(() => {
        View.deactivate();
        Client.stop();
      });
    };
  };
};

let activate = (context: VSCode.ExtensionContext.t) => {
  let subscribe = x =>
    x->Js.Array.push(VSCode.ExtensionContext.subscriptions(context))->ignore;

  // on open
  VSCode.Window.activeTextEditor->Option.forEach(
    Handler.onOpenEditor(context),
  );
  VSCode.Window.onDidChangeActiveTextEditor(next =>
    next->Option.forEach(Handler.onOpenEditor(context))
  )
  ->subscribe;

  // on close
  VSCode.Workspace.onDidCloseTextDocument(. Handler.onCloseEditor)->subscribe;

  // on change selection
  VSCode.Window.onDidChangeTextEditorSelection(Handler.onSelect)->subscribe;
};

let deactivate = () => {
  ();
};
