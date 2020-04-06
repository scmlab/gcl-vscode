//
// View
//
module View =
       (Editor: Sig.Editor with type context = Vscode.ExtensionContext.t) => {
  open Vscode;
  let make = (context, editor) => {
    let html = (distPath, styleUri, scriptUri) => {
      let nonce = {
        let text = ref("");
        let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        let cardinality = Js.String.length(charaterSet);
        for (_ in 0 to 32) {
          text :=
            text^
            ++ Js.String.charAt(
                 Js.Math.floor(
                   Js.Math.random() *. float_of_int(cardinality),
                 ),
                 charaterSet,
               );
        };
        text^;
      };

      let styleUri =
        Uri.file(Node.Path.join2(distPath, styleUri))
        ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));

      let scriptUri =
        Uri.file(Node.Path.join2(distPath, scriptUri))
        ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));

      let metaContent =
        "default-src 'none'; img-src vscode-resource: https:; script-src 'nonce-"
        ++ nonce
        ++ "';style-src vscode-resource: 'unsafe-inline' http: https: data:;";

      {j|
  <!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="utf-8">
          <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
          <meta name="theme-color" content="#000000">
          <title>React App</title>
          <link rel="stylesheet" type="text/css" href="$styleUri">
          <meta http-equiv="Content-Security-Policy" content="$metaContent">
        </head>
        <body>
          <noscript>You need to enable JavaScript to run this app.</noscript>
          <div id="root"></div>
          <script nonce="$nonce" src="$scriptUri"></script>
        </body>
        </html>
  |j};
    };

    let createPanel = (context, editor) => {
      let distPath =
        Node.Path.join2(context->Editor.getExtensionPath, "dist");
      let fileName =
        Node.Path.basename_ext(
          editor->TextEditor.document->TextDocument.fileName,
          ".gcl",
        );

      let panel =
        Window.createWebviewPanel(
          "panel",
          "GCL [" ++ fileName ++ "]",
          {preserveFocus: true, viewColumn: 3},
          // None,
          Some(
            WebviewAndWebviewPanelOptions.make(
              ~enableScripts=true,
              // And restric the webview to only loading content from our extension's `media` directory.
              ~localResourceRoots=[|Uri.file(distPath)|],
              (),
            ),
          ),
        );

      // panel->WebviewPanel.webview->Webview.postMessage(C(3)) |> ignore;
      // ->Js.Array.push(state.context.subscriptions)
      // ->ignore;

      // on message
      panel
      ->WebviewPanel.webview
      ->Webview.onDidReceiveMessage(message => {Js.log(message)})
      ->Js.Array.push(context->ExtensionContext.subscriptions)
      ->ignore;

      // on destroy
      panel->WebviewPanel.onDidDispose(() => Js.log("[ view ][ destroyed ]"))
      |> ignore;

      panel
      ->WebviewPanel.webview
      ->Webview.setHtml(html(distPath, "style.css", "bundled-view.js"));

      panel;
    };

    let moveToBottom = () => {
      Commands.(
        executeCommand(
          `setEditorLayout({
            orientation: 1,
            groups:
              Layout.(
                [|
                  sized({groups: [|simple|], size: 0.8}),
                  sized({groups: [|simple|], size: 0.2}),
                |]
              ),
          }),
        )
      );
    };

    // intantiate the panel
    let panel = createPanel(context, editor);
    moveToBottom() |> ignore;

    panel;
  };
  let destroy = view => {
    view->WebviewPanel.dispose;
  };
  // show/hide
  let show = view => view->WebviewPanel.reveal(~preserveFocus=true, ());
  let hide = _view => ();
};

module rec Impl: Sig.Editor with type context = Vscode.ExtensionContext.t = {
  open Vscode;
  open Belt;

  type editor = Vscode.TextEditor.t;
  type context = Vscode.ExtensionContext.t;
  type disposable = Vscode.Disposable.t;
  type view = Vscode.WebviewPanel.t;
  type fileName = string;

  // let make = (editor, context) => {editor, context};

  let getExtensionPath = context => context->ExtensionContext.extensionPath;

  let getFileName = editor =>
    editor->TextEditor.document->TextDocument.fileName;

  let addToSubscriptions = (disposable, context) =>
    disposable
    ->Js.Array.push(context->ExtensionContext.subscriptions)
    ->ignore;
  // let onOpenEditor = callback => Workspace.onDidRenameFiles(event => ());
  // when the editor got closed
  let onDidCloseEditor = callback =>
    Workspace.onDidCloseTextDocument(textDoc =>
      textDoc->Option.forEach(textDoc =>
        callback(textDoc->TextDocument.fileName)
      )
    );

  let onDidChangeFileName = callback =>
    Workspace.onDidRenameFiles(event =>
      event
      ->Option.map(Vscode.FileRenameEvent.files)
      ->Option.forEach(files => {
          files->Array.forEach(file =>
            callback(
              Some(file##oldUri->Uri.path),
              Some(file##newUri->Uri.path),
            )
          )
        })
    );
  let onDidChangeActivation = callback => {
    let previous = ref(Window.activeTextEditor->Option.map(getFileName));

    Window.onDidChangeActiveTextEditor(next => {
      let next = next->Option.map(getFileName);
      if (next != previous^) {
        callback(previous^, next);
        previous := next;
      };
    });
  };

  let registerCommand = (name, callback) =>
    Commands.registerCommand("extension." ++ name, () => {
      Window.activeTextEditor->Option.forEach(callback)
    });

  //
  // Configuration
  //
  module Config = {
    let setGCLPath = path =>
      Workspace.getConfiguration(Some("guacamole"), None)
      ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
    let getGCLPath = () =>
      Workspace.getConfiguration(Some("guacamole"), None)
      ->WorkspaceConfiguration.get("gclPath");
  };
  //
  // View
  //
  module View = View(Impl);
};