open Belt

module Panel = {
  type t = VSCode.WebviewPanel.t

  let makeHTML = (webview, extensionPath) => {
    let extensionUri = VSCode.Uri.file(extensionPath)
    // generates gibberish
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

    let scriptUri =
      VSCode.Webview.asWebviewUri(
        webview,
        VSCode.Uri.joinPath(extensionUri, ["dist", "bundled-view.js"]),
      )->VSCode.Uri.toString

    let cspSourceUri = VSCode.Webview.cspSource(webview)

    let styleUri =
      VSCode.Webview.asWebviewUri(
        webview,
        VSCode.Uri.joinPath(extensionUri, ["dist", "style.css"]),
      )->VSCode.Uri.toString

    let codiconsUri = VSCode.Webview.asWebviewUri(
      webview,
      VSCode.Uri.joinPath(
        extensionUri,
        ["dist", "codicon/codicon.css"],
        // ["node_modules", "vscode-codicons", "dist", "codicon.css"],
      ),
    )->VSCode.Uri.toString

    let codiconsFontUri = VSCode.Webview.asWebviewUri(
      webview,
      VSCode.Uri.joinPath(
        extensionUri,
        ["dist", "codicon/codicon.ttf"],
        // ["node_modules", "vscode-codicons", "dist", "codicon.ttf"],
      ),
    )->VSCode.Uri.toString

    // Content-Security-Policy
    let defaultSrc = "default-src 'none'; "
    let scriptSrc = "script-src 'nonce-" ++ nonce ++ "'; "
    let styleSrc = "style-src " ++ cspSourceUri ++ " " ++ styleUri ++ " " ++ codiconsUri ++ "; "
    let fontSrc = "font-src " ++ codiconsFontUri ++ "; "
    let scp = defaultSrc ++ fontSrc ++ scriptSrc ++ styleSrc

    j`
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
        <meta name="theme-color" content="#000000">

        <!--
					Use a content security policy to only allow loading images from https or from our extension directory,
					and only allow scripts that have a specific nonce.
				-->
        <meta http-equiv="Content-Security-Policy" content="$scp">

        <title>React App</title>
        <link href="$styleUri"    rel="stylesheet" type="text/css" >
        <link href="$codiconsUri" rel="stylesheet" />
      </head>
      <body>
        <noscript>You need to enable JavaScript to run this app.</noscript>
        <div id="root"></div>
        <script nonce="$nonce" src="$scriptUri"></script>
      </body>
      </html>
    `
  }

  let make = extensionPath => {
    let distPath = Node.Path.join2(extensionPath, "dist")
    let panel = VSCode.Window.createWebviewPanel(
      "panel",
      "Guacamole",
      {"preserveFocus": true, "viewColumn": 3},
      // None,
      Some(
        VSCode.WebviewAndWebviewPanelOptions.make(
          ~enableScripts=true,
          // So that the view don't get wiped out when it's not in the foreground
          ~retainContextWhenHidden=true,
          // And restrict the webview to only loading content from our extension's `dist` directory.
          ~localResourceRoots=[VSCode.Uri.file(distPath)],
          (),
        ),
      ),
    )

    let html = makeHTML(VSCode.WebviewPanel.webview(panel), extensionPath)
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.setHtml(html)

    panel
  }
}

module type View = {
  type t
  // lifecycle
  let make: string => Promise.t<t>
  let destroy: t => unit
  // messaging
  let send: (t, ViewType.Request.t) => Promise.t<bool>
  let on: (t, ViewType.Response.t => unit) => VSCode.Disposable.t
  let reveal: t => unit
  // event
  let onceDestroyed: t => Promise.t<unit>
}

module View: View = {
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

  let on = (view, callback) => view.onResponse->Chan.on(callback)->VSCode.Disposable.make

  let make = extensionPath => {
    let view = {
      panel: Panel.make(extensionPath),
      subscriptions: [],
      onResponse: Chan.make(),
      status: Uninitialized([]),
    }

    // Panel.moveToRight()

    // on message
    view.panel
    ->VSCode.WebviewPanel.webview
    ->VSCode.Webview.onDidReceiveMessage(json =>
      switch ViewType.Response.decode(json) {
      | result => view.onResponse->Chan.emit(result)
      | exception e => Js.log2("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e)
      }
    )
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // on destroy
    view.panel
    ->VSCode.WebviewPanel.onDidDispose(() =>
      view.onResponse->Chan.emit(ViewType.Response.Destroyed)
    )
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // only gets resolved once initizlied
    let (promise, resolve) = Promise.pending()

    // on initizlied
    view.onResponse
    ->Chan.on(x =>
      switch x {
      | Initialized =>
        switch view.status {
        | Uninitialized(queued) =>
          view.status = Initialized
          resolve(view)
          queued->Belt.Array.forEach(req => send(view, req)->ignore)
        | Initialized => ()
        }
      | _ => ()
      }
    )
    ->VSCode.Disposable.make
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // return a promise that resolves the View once it's initialized
    promise
  }

  let destroy = view => {
    view.panel->VSCode.WebviewPanel.dispose
    view.onResponse->Chan.destroy
  }

  // resolves the returned promise once the view has been destroyed
  let onceDestroyed = view => {
    let (promise, resolve) = Promise.pending()

    let disposable = view.onResponse->Chan.on(response =>
      switch response {
      | ViewType.Response.Destroyed => resolve()
      | _ => ()
      }
    )

    promise->Promise.tap(disposable)
  }

  let reveal = view => VSCode.WebviewPanel.reveal(view.panel, ())
}

module type Controller = {
  // life cycle
  let activate: string => Promise.t<unit>
  let deactivate: unit => unit
  // properties
  let isActivated: unit => bool
  // messaging
  let send: ViewType.Request.t => Promise.t<bool>
  let on: (ViewType.Response.t => unit) => VSCode.Disposable.t
  let focus: unit => unit
}

module Controller: Controller = {
  // internal singleton
  let singleton: ref<option<View.t>> = ref(None)

  let send = req =>
    switch singleton.contents {
    | None => Promise.resolved(false)
    | Some(view) => View.send(view, req)
    }
  let on = callback =>
    switch singleton.contents {
    | None => VSCode.Disposable.make(() => ())
    | Some(view) => View.on(view, callback)
    }

  let activate = extensionPath =>
    switch singleton.contents {
    | None =>
      View.make(extensionPath)->Promise.map(view => {
        singleton.contents = Some(view)
        // free the handle when the view has been forcibly destructed
        View.onceDestroyed(view)->Promise.get(() => {
          singleton.contents = None
        })
        Js.log("[ view ] activated")
      })
    | Some(_) => Promise.resolved()
    }

  let deactivate = () =>
    switch singleton.contents {
    | Some(view) =>
      View.destroy(view)
      singleton.contents = None
      Js.log("[ view ] deactivated")
    | None => ()
    }

  let isActivated = () => singleton.contents->Option.isSome

  let focus = () =>
    switch singleton.contents {
    | Some(view) => View.reveal(view)
    | None => ()
    }
}

include Controller
