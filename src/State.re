open Belt;

type t = {
  editor: VSCode.TextEditor.t,
  document: VSCode.TextDocument.t,
  filePath: string,
  client: LSP.LanguageClient.t,
  // mutable view: option(View.t),
  mutable decorations: array(VSCode.TextEditorDecorationType.t),
  mutable subscriptions: array(VSCode.Disposable.t),
};

let subscribe = (disposable, state) =>
  disposable->Js.Array.push(state.subscriptions)->ignore;

let handleResponse = (state: t, response) =>
  switch (response) {
  | Response.Error(error) => Js.log(error)
  | OK(i, pos, _, props) => ()
  // state.view
  // ->Option.forEach(view => {
  //     View.send(
  //       view,
  //       ViewType.Request.Display(
  //         Plain("Proof Obligations"),
  //         ProofObligations(i, pos, props),
  //       ),
  //     )
  //     ->ignore
  //   })
  | Decorate(locs) =>
    // destroy old decorations
    state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose);
    state.decorations = [||];

    let ranges = locs->Array.map(GCL.Loc.toRange);
    let rangeBehavior =
      VSCode.DecorationRangeBehavior.toEnum(
        VSCode.DecorationRangeBehavior.ClosedClosed,
      );
    let rulerColor =
      VSCode.StringOr.others(
        VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
      );
    let overviewRulerLane: VSCode.OverviewRulerLane.raw =
      VSCode.OverviewRulerLane.Left->VSCode.OverviewRulerLane.toEnum;

    let backgroundColor =
      VSCode.StringOr.others(
        VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
      );
    let after =
      VSCode.ThemableDecorationAttachmentRenderOptions.t(
        ~contentText="*",
        // ~border="dotted 3px",
        // ~borderColor=backgroundColor,
        ~color=backgroundColor,
        (),
      );
    let options =
      VSCode.DecorationRenderOptions.t(
        ~overviewRulerColor=rulerColor,
        ~overviewRulerLane,
        ~after,
        // ~backgroundColor,
        // ~isWholeLine=true,
        ~rangeBehavior,
        (),
      );
    let decoration = VSCode.Window.createTextEditorDecorationType(options);
    decoration->Js.Array.push(state.decorations)->ignore;
    state.editor->VSCode.TextEditor.setDecorations(decoration, ranges);
  | _ => ()
  };

let decodeResponse = (json: Js.Json.t): Response.t => {
  // catching exceptions occured when decoding JSON values
  switch (Response.decode(json)) {
  | response => response
  | exception (Json.Decode.DecodeError(msg)) =>
    Error([|Error(Others, CannotDecodeResponse(msg, json))|])
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
  let client = setupLSPClient();
  // start the LSP client
  client->LSP.LanguageClient.start;
  let document = VSCode.TextEditor.document(editor);
  let filePath = VSCode.TextDocument.fileName(document);
  let state = {
    client,
    editor,
    document,
    filePath,
    // view: None,
    decorations: [||],
    subscriptions: [||],
  };

  client
  ->LSP.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.getOk(() => {
      client->LSP.LanguageClient.onNotification("guacamole", json => {
        // Js.log2(">>>", json);
        let response = decodeResponse(json);
        handleResponse(state, response);
      })
    });

  state;
};

let sendRequest = (state, request) => {
  let value = Request.encode(request);
  // Js.log2("<<<", value);

  state.client
  ->LSP.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.flatMapOk(() => {
      state.client
      ->LSP.LanguageClient.sendRequest("guacamole", value)
      ->Promise.Js.toResult
    })
  ->Promise.flatMapOk(json => {
      // Js.log2(">>>", json);

      let response = decodeResponse(json);
      Promise.resolved(Ok(response));
    });
  // ->Promise.mapError(error => {Error.LSP(Error.fromJsError(error))});
};

let destroy = state => {
  state.client->LSP.LanguageClient.stop;
  // state.view->Option.forEach(View.destroy);
  state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose);
  state.decorations = [||];
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose);
  state.subscriptions = [||];
};

// view related
// let show = state => state.view->Option.forEach(View.show);
// let hide = state => state.view->Option.forEach(View.hide);
