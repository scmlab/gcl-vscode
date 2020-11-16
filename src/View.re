// status of the view
type status =
  | Initialized
  // Request.t are queued before the view is initialized
  | Uninitialized(array(ViewType.Request.t));

type t = {
  panel: VSCode.WebviewPanel.t,
  onResponse: Event.t(ViewType.Response.t),
  disposables: array(VSCode.Disposable.t),
  mutable status,
};

// messaging
let send = (view, req) =>
  switch (view.status) {
  | Uninitialized(queued) =>
    Js.Array.push(req, queued)->ignore;
    Promise.resolved(false);
  | Initialized =>
    let stringified = Js.Json.stringify(ViewType.Request.encode(req));
    view.panel
    ->VSCode.WebviewPanel.webview
    ->VSCode.Webview.postMessage(stringified);
  };

let recv = (view, callback) => {
  // Handle messages from the webview
  view.onResponse.on(callback)
  ->VSCode.Disposable.make;
};

let make = (extentionPath, editor) => {
  let html = (distPath, styleUri, scriptUri) => {
    let nonce = {
      let text = ref("");
      let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
      let cardinality = Js.String.length(charaterSet);
      for (_ in 0 to 32) {
        text :=
          text^
          ++ Js.String.charAt(
               Js.Math.floor(Js.Math.random() *. float_of_int(cardinality)),
               charaterSet,
             );
      };
      text^;
    };

    let styleUri =
      VSCode.Uri.file(Node.Path.join2(distPath, styleUri))
      ->VSCode.Uri.with_(
          VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
        );

    let scriptUri =
      VSCode.Uri.file(Node.Path.join2(distPath, scriptUri))
      ->VSCode.Uri.with_(
          VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
        );

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

  let createPanel = editor => {
    let distPath = Node.Path.join2(extentionPath, "dist");
    let fileName =
      Node.Path.basename_ext(
        editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        ".gcl",
      );

    let panel =
      VSCode.Window.createWebviewPanel(
        "panel",
        "GCL [" ++ fileName ++ "]",
        {preserveFocus: true, viewColumn: 3},
        // None,
        Some(
          VSCode.WebviewAndWebviewPanelOptions.make(
            ~enableScripts=true,
            // So that the view don't get wiped out when it's not in the foreground
            ~retainContextWhenHidden=true,
            // And restrict the webview to only loading content from our extension's `media` directory.
            ~localResourceRoots=[|VSCode.Uri.file(distPath)|],
            (),
          ),
        ),
      );

    panel
    ->VSCode.WebviewPanel.webview
    ->VSCode.Webview.setHtml(html(distPath, "style.css", "bundled-view.js"));

    panel;
  };

  let moveToBottom = () => {
    VSCode.Commands.(
      executeCommand(
        `setEditorLayout({
          orientation: 1,
          groups:
            Layout.(
              [|
                sized({groups: [|simple|], size: 0.5}),
                sized({groups: [|simple|], size: 0.5}),
              |]
            ),
        }),
      )
    );
  };

  // intantiate the panel
  let panel = createPanel(editor);
  moveToBottom() |> ignore;

  // array of Disposable.t
  let disposables = [||];

  // on message
  let onResponse = Event.make();
  panel
  ->VSCode.WebviewPanel.webview
  ->VSCode.Webview.onDidReceiveMessage(json => {
      switch (ViewType.Response.decode(json)) {
      | result => onResponse.emit(result)
      | exception e =>
        Js.log2(
          "[ panic ][ Webview.onDidReceiveMessage JSON decode error ]",
          e,
        )
      }
    })
  ->Js.Array.push(disposables)
  ->ignore;

  // on destroy
  panel
  ->VSCode.WebviewPanel.onDidDispose(() =>
      onResponse.emit(ViewType.Response.Destroyed)
    )
  ->Js.Array.push(disposables)
  ->ignore;

  let view = {panel, disposables, onResponse, status: Uninitialized([||])};

  // on initizlied
  view.onResponse.on(
    fun
    | Initialized => {
        switch (view.status) {
        | Uninitialized(queued) =>
          view.status = Initialized;
          queued->Belt.Array.forEach(req => send(view, req)->ignore);
        | Initialized => ()
        };
      }
    | _ => (),
  )
  ->VSCode.Disposable.make
  ->Js.Array.push(disposables)
  ->ignore;

  view;
};

let destroy = view => {
  view.panel->VSCode.WebviewPanel.dispose;
  view.onResponse.destroy();
};

// show/hide
let show = view =>
  view.panel->VSCode.WebviewPanel.reveal(~preserveFocus=true, ());
let focus = view => view.panel->VSCode.WebviewPanel.reveal();
let hide = _view => ();
