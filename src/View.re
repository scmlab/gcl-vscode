
module Request = {
  module Header = {
    type t =
      | Loading
      | Plain(string)
      | Error(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Loading" => Contents(_ => Loading)
        | "Plain" => Contents(json => Plain(string(json)))
        | "Error" => Contents(json => Error(string(json)))
        | tag =>
          raise(
            DecodeError("[Request.Header] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Loading => object_([("tag", string("Loading"))])
      | Plain(s) =>
        object_([("tag", string("Plain")), ("contents", string(s))])
      | Error(s) =>
        object_([("tag", string("Error")), ("contents", string(s))]);
  };
  module Body = {
    type id = int;
    type t =
      | Nothing
      | ProofObligations(id, array(Response.ProofObligation.t), array(Response.GlobalProp.t))
      | Plain(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Nothing" => Contents(_ => Nothing)
        | "ProofObligations" =>
          Contents(
            tuple3(int, array(Response.ProofObligation.decode), array(Response.GlobalProp.decode))
            |> map(((id, xs, ys)) => ProofObligations(id, xs, ys)),
          )
        | "Plain" => Contents(json => Plain(string(json)))
        | tag =>
          raise(
            DecodeError("[Request.Header] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Nothing => object_([("tag", string("Nothing"))])
      | ProofObligations(id, xs, ys) =>
        object_([
          ("tag", string("ProofObligations")),
          (
            "contents",
            (id, xs, ys) |> tuple3(int, array(Response.ProofObligation.encode), array(Response.GlobalProp.encode)),
          ),
        ])
      | Plain(x) =>
        object_([("tag", string("Plain")), ("contents", string(x))]);
  };

  type t =
    | Show
    | Hide
    | Substitute(int, GCL.Syntax.Expr.t)
    | Display(Header.t, Body.t);

  open Json.Decode;
  open Util.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Show" => Contents(_ => Show)
      | "Hide" => Contents(_ => Hide)
      | "Substitute" =>
        Contents(
          pair(int, GCL.Syntax.Expr.decode)
          |> map(((x, y)) => Substitute(x, y)),
        )
      | "Display" =>
        Contents(
          pair(Header.decode, Body.decode)
          |> map(((x, y)) => Display(x, y)),
        )
      | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Show => object_([("tag", string("Show"))])
    | Hide => object_([("tag", string("Hide"))])
    | Substitute(i, expr) =>
      object_([
        ("tag", string("Substitute")),
        ("contents", (i, expr) |> pair(int, GCL.Syntax.Expr.encode)),
      ])
    | Display(header, body) =>
      object_([
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      ]);
};

module Response = {
  type linkEvent =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc);

  type t =
    | SetMode(GCL.mode)
    | Link(linkEvent)
    | Substitute(int, GCL.Syntax.Expr.t, GCL.Syntax.Expr.subst)
    | Initialized
    | Destroyed;

  open Json.Decode;
  open Util.Decode;

  let decodeMode: decoder(GCL.mode) =
    sum(
      fun
      | "WP1" => TagOnly(_ => GCL.WP1)
      | "WP2" => TagOnly(_ => GCL.WP2)
      | tag =>
        raise(
          DecodeError("[Response.mode] Unknown constructor: " ++ tag),
        ),
    );

  let decodeLinkEvent: decoder(linkEvent) =
    sum(
      fun
      | "MouseOver" => Contents(loc => MouseOver(GCL.Loc.decode(loc)))
      | "MouseOut" => Contents(loc => MouseOut(GCL.Loc.decode(loc)))
      | "MouseClick" => Contents(loc => MouseClick(GCL.Loc.decode(loc)))
      | tag =>
        raise(
          DecodeError(
            "[Response.linkEvent] Unknown constructor: " ++ tag,
          ),
        ),
    );

  let decode: decoder(t) =
    sum(
      fun
      | "Initialized" => TagOnly(_ => Initialized)
      | "Destroyed" => TagOnly(_ => Destroyed)
      | "SetMode" => Contents(json => SetMode(decodeMode(json)))
      | "Link" => Contents(json => Link(decodeLinkEvent(json)))
      | "Substitute" =>
        Contents(
          tuple3(int, GCL.Syntax.Expr.decode, GCL.Syntax.Expr.decodeSubst)
          |> map(((i, expr, subst)) => Substitute(i, expr, subst)),
        )
      | tag =>
        raise(DecodeError("[Response.t] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;

  let encodeMode: encoder(GCL.mode) =
    fun
    | WP1 => object_([("tag", string("WP1"))])
    | WP2 => object_([("tag", string("WP2"))]);

  let encodeLinkEvent: encoder(linkEvent) =
    fun
    | MouseOver(loc) =>
      object_([
        ("tag", string("MouseOver")),
        ("contents", GCL.Loc.encode(loc)),
      ])
    | MouseOut(loc) =>
      object_([
        ("tag", string("MouseOut")),
        ("contents", GCL.Loc.encode(loc)),
      ])
    | MouseClick(loc) =>
      object_([
        ("tag", string("MouseClick")),
        ("contents", GCL.Loc.encode(loc)),
      ]);

  let encode: encoder(t) =
    fun
    | Initialized => object_([("tag", string("Initialized"))])
    | Destroyed => object_([("tag", string("Destroyed"))])
    | Link(e) =>
      object_([("tag", string("Link")), ("contents", encodeLinkEvent(e))])
    | Substitute(i, expr, subst) =>
      object_([
        ("tag", string("Substitute")),
        (
          "contents",
          (i, expr, subst)
          |> tuple3(int, GCL.Syntax.Expr.encode, GCL.Syntax.Expr.encodeSubst),
        ),
      ])
    | SetMode(mode) =>
      object_([
        ("tag", string("SetMode")),
        ("contents", encodeMode(mode)),
      ]);
};

// status of the view
type status =
  | Initialized
  // Request.t are queued before the view is initialized
  | Uninitialized(array(Request.t));

open VSCode;

type t = {
  panel: WebviewPanel.t,
  onResponse: AgdaModeVscode.Event.t(Response.t),
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
    let stringified = Js.Json.stringify(req |> Request.encode);
    view.panel->WebviewPanel.webview->Webview.postMessage(stringified);
  };

let recv = (view, callback) => {
  // Handle messages from the webview
  view.onResponse.on(callback)
  ->Disposable.make;
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

  let createPanel = (editor) => {
    let distPath = Node.Path.join2(extentionPath, "dist");
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
            // So that the view don't get wiped out when it's not in the foreground
            ~retainContextWhenHidden=true,
            // And restrict the webview to only loading content from our extension's `media` directory.
            ~localResourceRoots=[|Uri.file(distPath)|],
            (),
          ),
        ),
      );

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
  let onResponse = AgdaModeVscode.Event.make();
  panel
  ->WebviewPanel.webview
  ->Webview.onDidReceiveMessage(json => {
      switch (Response.decode(json)) {
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
  ->WebviewPanel.onDidDispose(() => onResponse.emit(Response.Destroyed))
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
  ->Disposable.make
  ->Js.Array.push(disposables)
  ->ignore;

  view;
};

let destroy = view => {
  view.panel->WebviewPanel.dispose;
  view.onResponse.destroy();
};

// show/hide
let show = view => view.panel->WebviewPanel.reveal(~preserveFocus=true, ());
let focus = view => view.panel->WebviewPanel.reveal();
let hide = _view => ();
