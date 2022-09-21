open! Util.Version
open LanguageServerMule
open Source.GitHub
open Belt

let chooseFromReleases = (releases: array<Release.t>): option<Target.t> => {
  // CURRENT RANGE: [v0.3, v0.4)
  let chooseRelease = (releases: array<Release.t>) => {
    let lowerBound = "v0.3.0"
    let upperBound = "v0.4.0"
    let withinBound = x => {
      let lower = compare(x, lowerBound)
      let upper = compare(x, upperBound)
      (lower == EQ || lower == GT) && upper == LT
    }
    let matched = releases->Array.keep(release => withinBound(release.tagName))
    let compare = (x: Release.t, y: Release.t) =>
      switch compare(x.tagName, y.tagName) {
      | GT => -1
      | EQ => 0
      | LT => 1
      }
    let sorted = Js.Array.sortInPlaceWith(compare, matched)
    sorted[0]
  }

  let toFileName = (release: Release.t, asset: Asset.t) => {
    // take the "macos" part from names like "gcl-macos.zip"
    let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
    // the file name of the language server
    release.tagName ++ "-" ++ osName
  }

  let chooseAsset = (release: Release.t) => {
    // expected asset name
    let os = Node_process.process["platform"]
    let expectedName = switch os {
    | "darwin" => Some("gcl-macos.zip")
    | "linux" => Some("gcl-ubuntu.zip")
    | "win32" => Some("gcl-windows.zip")
    | _others => None
    }

    // find the corresponding asset
    expectedName
    ->Option.flatMap(name => {
      let matched = release.assets->Array.keep(asset => asset.name == name)
      matched[0]
    })
    ->Option.map(asset => {
      Target.srcUrl: asset.url,
      fileName: toFileName(release, asset),
      release: release,
      asset: asset,
    })
  }

  chooseRelease(releases)->Option.flatMap(chooseAsset)
}

let afterDownload = (fromCached, (path, target)) => {
  let executablePath = NodeJs.Path.join2(path, "gcl")
  if fromCached {
    // already chmod after download
    Promise.resolved(Ok((executablePath, [], None, target)))
  } else {
    // no need of chmod on Windows
    switch Node_process.process["platform"] {
    | "win32" => Promise.resolved(Ok((executablePath, [], None, target)))
    | _others => 
      // wait for 100ms after chmod to prevent some mysterious "spawn Unknown system error -88"
    chmodExecutable(executablePath)->Promise.flatMapOk(() => {
      let (promise, resolve) = Promise.pending()
      Js.Global.setTimeout(() => resolve(Ok((executablePath, [], None, target))), 100)->ignore
      promise
    })
    }
  }
}

// see if the server is available
// priorities: TCP => Prebuilt => StdIO
let probe = (globalStoragePath, onDownload) => {
  let port = 3000
  let name = VSCode.Workspace.getConfiguration(None,None)
              ->VSCode.WorkspaceConfiguration.get("guabao.gclPath")
              ->Option.mapWithDefault("",x=>x)
  // Yet the connection established by FromCommand is still based on stdio, which is not functioning at current version.

  Source.searchUntilSuccess([
    Source.FromTCP(port, "localhost"),
    Source.FromGitHub({
      username: "scmlab",
      repository: "gcl",
      userAgent: "gcl-vscode",
      globalStoragePath: globalStoragePath,
      chooseFromReleases: chooseFromReleases,
      afterDownload: afterDownload,
      log: Js.log,
      onDownload: onDownload,
      cacheInvalidateExpirationSecs: 86400,
    }),
    Source.FromCommand(name),
  ])->Promise.map(Source.consumeResult)
}
