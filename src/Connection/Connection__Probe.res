open LanguageServerMule
open Source.GitHub
open Belt

let chooseFromReleases = (releases: array<Release.t>): option<Target.t> => {
  let chooseRelease = (releases: array<Release.t>) => {
    let matched = releases->Array.keep(release => release.tagName == Config.version)
    matched[0]
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

  // // see if the chosen Asset has already been downloaded
  // let checkExistingDownload = (target: Target.t) => {
  //   let matchedFiles = NodeJs.Fs.readdirSync(globalStoragePath)->Array.keep(path => {
  //     Js.log3(path, target.fileName, target.fileName == path)
  //     path == target.fileName
  //   })

  //   if Array.length(matchedFiles) > 0 {
  //     Js.log("has existsing download")
  //     None
  //   } else {
  //     Js.log("no existsing download")
  //     Some(target)
  //   }
  // }

  chooseRelease(releases)->Option.flatMap(chooseAsset)
}

// see if the server is available
// priorities: TCP => Prebuilt => StdIO
let probe = globalStoragePath => {
  let port = 3000
  let name = "gcl"

  Source.Module.searchUntilSuccess([
    Source.FromTCP(port, "localhost"),
    Source.FromPath(name),
    Source.FromGitHub({
      username: "scmlab",
      repository: "gcl",
      userAgent: "gcl-vscode",
      globalStoragePath: globalStoragePath,
      chooseFromReleases: chooseFromReleases,
    }),
  ])->Promise.mapError(e => {
    Js.log("Source.searchUntilSuccess " ++ Source.Error.toString(e))
    Connection__Error.CannotAcquireHandle(e)
  })
}
