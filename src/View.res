open Belt

module Panel = {
  type t = VSCode.WebviewPanel.t

  let makeHTML = (distPath, styleUri, scriptUri) => {
    let nonce = {
      let text = ref("")
      let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      let cardinality = Js.String.length(charaterSet)
      for _ in 0 to 32 {
        text :=
          text.contents ++
          Js.String.charAt(
            Js.Math.floor(Js.Math.random() *. float_of_int(cardinality)),
            charaterSet,
          )
      }
      text.contents
    }

    let styleUri =
      VSCode.Uri.file(Node.Path.join2(distPath, styleUri))->VSCode.Uri.with_(
        VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
      )

    let scriptUri =
      VSCode.Uri.file(Node.Path.join2(distPath, scriptUri))->VSCode.Uri.with_(
        VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
      )

    let metaContent =
      "default-src 'none'; img-src vscode-resource: https:; script-src 'nonce-" ++
      (nonce ++
      "';style-src vscode-resource: 'unsafe-inline' http: https: data:;")

    j`
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
        `
  }

  let make = extentionPath => {
    let distPath = Node.Path.join2(extentionPath, "dist")
    let panel = VSCode.Window.createWebviewPanel(
      "panel",
      "Guacamole",
      {preserveFocus: true, viewColumn: 3},
      // None,
      Some(
        VSCode.WebviewAndWebviewPanelOptions.make(
          ~enableScripts=true,
          // So that the view don't get wiped out when it's not in the foreground
          ~retainContextWhenHidden=true,
          // And restrict the webview to only loading content from our extension's `media` directory.
          ~localResourceRoots=[VSCode.Uri.file(distPath)],
          (),
        ),
      ),
    )

    let html = makeHTML(distPath, "style.css", "bundled-view.js")
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.setHtml(html)

    panel
  }

  let moveToRight = () => {
    open VSCode.Commands
    executeCommand(
      #setEditorLayout({
        orientation: 0,
        groups: {
          open Layout
          [sized({groups: [simple], size: 0.5}), sized({groups: [simple], size: 0.5})]
        },
      }),
    )->ignore
  }
}

module View = {
  // Request.t are queued before the view is initialized
  type status =
    | Initialized
    | Uninitialized(array<ViewType.Request.t>)

  type t = {
    panel: Panel.t,
    onResponse: Chan.t<ViewType.Response.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable status: status,
  }

  // messaging
  let send = (view, req) =>
    switch view.status {
    | Uninitialized(queued) =>
      Js.Array.push(req, queued)->ignore
      Promise.resolved(false)
    | Initialized =>
      let stringified = Js.Json.stringify(ViewType.Request.encode(req))
      view.panel->VSCode.WebviewPanel.webview->VSCode.Webview.postMessage(stringified)
    }

  let make = extentionPath => {
    let view = {
      panel: Panel.make(extentionPath),
      subscriptions: [],
      onResponse: Chan.make(),
      status: Uninitialized([]),
    }

    Panel.moveToRight()

    // on message
    view.panel->VSCode.WebviewPanel.webview->VSCode.Webview.onDidReceiveMessage(json =>
      switch ViewType.Response.decode(json) {
      | result => view.onResponse->Chan.emit(result)
      | exception e => Js.log2("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e)
      }
    )->Js.Array.push(view.subscriptions)->ignore

    // on destroy
    view.panel
    ->VSCode.WebviewPanel.onDidDispose(() =>
      view.onResponse->Chan.emit(ViewType.Response.Destroyed)
    )
    ->Js.Array.push(view.subscriptions)
    ->ignore
    // on initizlied
    view.onResponse->Chan.on(x =>
      switch x {
      | Initialized =>
        switch view.status {
        | Uninitialized(queued) =>
          view.status = Initialized
          queued->Belt.Array.forEach(req => send(view, req)->ignore)
        | Initialized => ()
        }
      | _ => ()
      }
    )->VSCode.Disposable.make->Js.Array.push(view.subscriptions)->ignore

    view
  }

  let destroy = view => {
    view.panel->VSCode.WebviewPanel.dispose
    view.onResponse->Chan.destroy
  }

  // resolves the returned promise once the view has been destroyed
  let onceDestroyed = (view: t): Promise.t<unit> => {
    let (promise, resolve) = Promise.pending()

    let disposable = view.onResponse->Chan.on(response =>
      switch response {
      | ViewType.Response.Destroyed => resolve()
      | _ => ()
      }
    )

    promise->Promise.tap(disposable)
  }
}

module type Controller = {
  type t
  // methods
  let activate: string => unit
  let deactivate: unit => unit
  let isActivated: unit => bool
  let wire: State.t => unit
  let focus: unit => unit
}
module Controller: Controller = {
  type t = {
    mutable view: option<View.t>,
    mutable reqSubscription: option<unit => unit>,
    mutable resSubscription: option<unit => unit>,
  }
  let handle = {view: None, reqSubscription: None, resSubscription: None}

  let unwire = () => {
    handle.reqSubscription->Option.forEach(disposable => disposable())
    handle.resSubscription->Option.forEach(disposable => disposable())
  }
  let activate = extensionPath => {
    let view = View.make(extensionPath)
    handle.view = Some(view)
    // free the handle when the view has been forcibly destructed
    View.onceDestroyed(view)->Promise.get(() => {
      handle.view = None
      unwire()
    })
  }

  let deactivate = () => {
    handle.view->Option.forEach(View.destroy)
    handle.view = None
    unwire()
  }

  let isActivated = () => handle.view->Option.isSome

  let wire = (state: State.t) => {
    // sever old connections
    unwire()
    // make new connections
    handle.view->Option.forEach(view => {
      handle.reqSubscription = Some(state.viewReq->Req.handle(View.send(view)))
      handle.resSubscription = Some(view.onResponse->Chan.on(Chan.emit(state.viewResChan)))
    })
  }

  let focus = () => handle.view->Option.forEach(view => {
      VSCode.WebviewPanel.reveal(view.panel, ())
    })
}

include Controller
