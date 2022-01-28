open Belt

// abstraction of a panel
module Panel: {
  type t
  // constructor / destructor
  let make: (string, string) => t
  let destroy: t => unit
  // messaging
  let send: (t, string) => Promise.t<bool>
  let recv: (t, Js.Json.t => unit) => VSCode.Disposable.t
  // events
  let onDestroyed: (t, unit => unit) => VSCode.Disposable.t
  // methods
  let reveal: t => unit
  let focus: t => unit
} = {
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

    let codiconsUri =
      VSCode.Webview.asWebviewUri(
        webview,
        VSCode.Uri.joinPath(extensionUri, ["dist", "codicon/codicon.css"]),
      )->VSCode.Uri.toString

    // let codiconsFontUri =
    //   VSCode.Webview.asWebviewUri(
    //     webview,
    //     VSCode.Uri.joinPath(extensionUri, ["dist", "codicon/codicon.ttf"]),
    //   )->VSCode.Uri.toString

    // Content-Security-Policy
    let defaultSrc = "default-src 'none'; "
    let scriptSrc = "script-src 'nonce-" ++ nonce ++ "'; "
    let styleSrc = "style-src " ++ cspSourceUri ++ "; "
    // let styleSrc = "style-src " ++ cspSourceUri ++ " " ++ styleUri ++ " " ++ codiconsUri ++ "; "
    let fontSrc = "font-src " ++ cspSourceUri ++ "; "
    // let fontSrc = "font-src " ++ codiconsFontUri ++ "; "
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

  let make = (title, extensionPath) => {
    let distPath = Node.Path.join2(extensionPath, "dist")
    let panel = VSCode.Window.createWebviewPanel(
      "panel",
      title,
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

  let destroy = VSCode.WebviewPanel.dispose

  let send = (panel, message) =>
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.postMessage(message)

  let recv = (panel, callback) =>
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.onDidReceiveMessage(callback)

  let onDestroyed = (panel, callback) => panel->VSCode.WebviewPanel.onDidDispose(callback)

  let reveal = panel => panel->VSCode.WebviewPanel.reveal(~preserveFocus=true, ())
  let focus = panel => panel->VSCode.WebviewPanel.reveal()
}

module type Instance = {
  type t
  // lifecycle
  let make: string => Promise.t<t>
  let destroy: t => unit
  // messaging
  let send: (t, ViewType.Request.t) => Promise.t<bool>
  let on: (t, ViewType.Response.t => unit) => VSCode.Disposable.t
  // event
  let onceDestroyed: t => Promise.t<unit>
  // methods
  let reveal: t => unit
  let focus: t => unit
}

module Instance: Instance = {
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
      Panel.send(view.panel, stringified)
    }

  let on = (view, callback) => view.onResponse->Chan.on(callback)->VSCode.Disposable.make

  let make = extensionPath => {
    let view = {
      panel: Panel.make("Guabao", extensionPath),
      subscriptions: [],
      onResponse: Chan.make(),
      status: Uninitialized([]),
    }

    // on message
    view.panel
    ->Panel.recv(json =>
      switch ViewType.Response.decode(json) {
      | result => view.onResponse->Chan.emit(result)
      | exception e => Js.log2("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e)
      }
    )
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // on destroy

    // on destroy
    view.panel
    ->Panel.onDestroyed(() => view.onResponse->Chan.emit(ViewType.Response.Destroyed))
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
    // if we invoke `view.panel->Panel.destroy` first,
    // this would trigger `ViewType.Response.Destroyed`
    // and in turns would trigger this function AGAIN

    // destroy the chan first, to prevent the aforementioned from happening
    view.onResponse->Chan.destroy
    view.panel->Panel.destroy
    view.subscriptions->Belt.Array.forEach(VSCode.Disposable.dispose)
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

  // show/focus
  let reveal = view => view.panel->Panel.reveal
  let focus = view => view.panel->Panel.focus
}

module type Singleton = {
  // life cycle
  let activate: string => Promise.t<unit>
  let deactivate: unit => unit
  // properties
  let isActivated: unit => bool
  // messaging
  let send: ViewType.Request.t => Promise.t<bool>
  let on: (ViewType.Response.t => unit) => Promise.t<VSCode.Disposable.t>
  let focus: unit => unit
}

module Singleton: Singleton = {
  // internal singleton
  let singleton: ref<option<Instance.t>> = ref(None)

  // save listners registered before view activation here
  let listenersBeforeAcvitation: array<(
    ViewType.Response.t => unit,
    VSCode.Disposable.t => unit,
  )> = []

  let send = req =>
    switch singleton.contents {
    | None => Promise.resolved(false)
    | Some(view) => Instance.send(view, req)
    }

  let on = callback =>
    switch singleton.contents {
    | None =>
      let (promise, resolve) = Promise.pending()
      Js.Array.push((callback, resolve), listenersBeforeAcvitation)->ignore
      promise
    | Some(view) => Instance.on(view, callback)->Promise.resolved
    }

  let activate = extensionPath =>
    switch singleton.contents {
    | None =>
      Instance.make(extensionPath)->Promise.map(view => {
        singleton.contents = Some(view)
        // register stored listeners
        listenersBeforeAcvitation->Js.Array2.forEach(((callback, resolve)) => {
          let dispose = Instance.on(view, callback)
          resolve(dispose)
        })
        // free the handle when the view has been forcibly destructed
        Instance.onceDestroyed(view)->Promise.get(() => {
          singleton.contents = None
        })
        Js.log("[ view ] activated")
      })
    | Some(_) => Promise.resolved()
    }

  let deactivate = () =>
    switch singleton.contents {
    | Some(view) =>
      Instance.destroy(view)
      singleton.contents = None
      Js.log("[ view ] deactivated")
    | None => ()
    }

  let isActivated = () => singleton.contents->Option.isSome

  let focus = () =>
    switch singleton.contents {
    | Some(view) => Instance.reveal(view)
    | None => ()
    }
}

include Singleton
