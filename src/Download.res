module Nd = {
  module Fs = {
    @bs.module("fs")
    external mkdirSync: string => unit = "mkdirSync"

    @bs.module("fs")
    external readFile_raw: (string, (option<Js.Exn.t>, NodeJs.Buffer.t) => unit) => unit =
      "readFile"
    let readFile = path => {
      let (promise, resolve) = Promise.pending()
      readFile_raw(path, (error, buffer) => {
        switch error {
        | None => resolve(Ok(buffer))
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @bs.module("fs")
    external createWriteStream: string => NodeJs.Fs.WriteStream.t = "createWriteStream"
  }

  module Https = {
    @bs.module("https")
    external get: (
      {"host": string, "path": string, "headers": {"User-Agent": string}},
      NodeJs.Http.IncomingMessage.t => unit,
    ) => unit = "get"

    @bs.module("https")
    external getWithUrl: (string, NodeJs.Http.IncomingMessage.t => unit) => unit = "get"

    // @bs.module("https")
    // external get: (
    //   {"host": string, "path": string, "protocol": string, "port": int, "headers": string},
    //   NodeJs.Stream.readable,
    // ) => unit = "get"
  }

  module Url = {
    @bs.module("url")
    external parse: string => {"host": string, "path": string, "protocol": string, "port": int} =
      "parse"
  }
}
// https.get(options[, callback])

// module SupportedOS = {
//   type t = MacOS | Linux | Windows

//   let value =
//     switch Node_process.process["platform"] {
//     | "darwin" => Some(MacOS)
//     | "linux" => Some(Linux)
//     | "win32" => Some(Windows)
//     | _ => None
//     }

//   let toAssetName = x =>
//     switch x {
//     | MacOS => "gcl-macos.zip"
//     | Linux => "gcl-ubuntu.zip"
//     | Windows => "gcl-windows.zip"
//     }
// }

module Error = {
  type t =
    | NoRedirectLocation
    | ResponseParseError(string)
    | ResponseDecodeError(string, Js.Json.t)
    | ServerResponseError(Js.Exn.t)
    | NoMatchingVersion(string)
    | NotSupportedOS(string)
    | CannotWriteFile(Js.Exn.t)
    | CannotUnzipFile(Js.Exn.t)
}

module HTTP = {
  let gatherDataFromResponse = res => {
    open NodeJs.Http.IncomingMessage
    let (promise, resolve) = Promise.pending()
    let body = ref("")
    res->onData(buf => body := body.contents ++ NodeJs.Buffer.toString(buf))->ignore
    res->onError(error => resolve(Error(Error.ServerResponseError(error))))->ignore
    res->onClose(() => resolve(Ok(body.contents)))->ignore
    promise
  }

  // with HTTP 301/302 redirect handled
  let getWithRedirects = options => {
    let (promise, resolve) = Promise.pending()
    Nd.Https.get(options, res => {
      // check the response status code first
      let statusCode = NodeJs.Http.IncomingMessage.statusCode(res)
      switch statusCode {
      // redirect
      | 301
      | 302 =>
        let headers = NodeJs.Http.IncomingMessage.headers(res)
        switch headers.location {
        | None => resolve(Error(Error.NoRedirectLocation))
        | Some(urlAfterRedirect) =>
          Nd.Https.getWithUrl(urlAfterRedirect, resAfterRedirect => resolve(Ok(resAfterRedirect)))
        }
      // ok ?
      | _ => resolve(Ok(res))
      }
    })
    promise
  }
}

module Release = {
  type asset = {
    name: string,
    url: string,
  }

  type t = {
    tagName: string,
    assets: array<asset>,
  }

  let decodeAsset = json => {
    open Json.Decode
    {
      url: json |> field("browser_download_url", string),
      name: json |> field("name", string),
    }
  }

  let decode = json => {
    open Json.Decode
    {
      tagName: json |> field("tag_name", string),
      assets: json |> field("assets", array(decodeAsset)),
    }
  }

  let parseMetadata = json =>
    try {
      Ok(json |> Json.Decode.array(decode))
    } catch {
    | Json.Decode.DecodeError(e) => Error(Error.ResponseDecodeError(e, json))
    }

  // please refrain from invoking this too many times
  let getReleasesFromGitHub = () => {
    // the url is fixed for now
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/scmlab/gcl/releases",
      "headers": {
        "User-Agent": "gcl-vscode",
      },
    }
    HTTP.getWithRedirects(httpOptions)
    ->Promise.flatMapOk(HTTP.gatherDataFromResponse)
    ->Promise.flatMapOk(raw =>
      try {
        Promise.resolved(parseMetadata(Js.Json.parseExn(raw)))
      } catch {
      | _ => Promise.resolved(Error(Error.ResponseParseError(raw)))
      }
    )
  }

  // // NOTE: no caching for the moment
  // let getReleaseMetadata = context => {
  //   // let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  //   // let cachePath = Node_path.join2(globalStoragePath, "latestRelease.cache.json")
  //   // read from the cache and see if it is too old
  //   // Nd.Fs.readFile(cachePath)->Promise.map(result)
  //   getReleaseMetadataFromGitHub()
  // }
}

let downloadLanguageServer = context => {
  // create a directory for context.globalStoragePath if it doesn't exist
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  if !Node_fs.existsSync(globalStoragePath) {
    Nd.Fs.mkdirSync(globalStoragePath)
  }

  let getMatchingRelease = (releases: array<Release.t>) => {
    open Belt
    let matched = releases->Array.keep(release => release.tagName == Constant.version)
    switch matched[0] {
    | None => Promise.resolved(Error(Error.NoMatchingVersion(Constant.version)))
    | Some(release) => Promise.resolved(Ok(release))
    }
  }

  let getMatchingAsset = (release: Release.t) => {
    open Belt
    // expected asset name
    let os = Node_process.process["platform"]
    let expectedName = switch os {
    | "darwin" => Ok("gcl-macos.zip")
    | "linux" => Ok("gcl-ubuntu.zip")
    | "win32" => Ok("gcl-windows.zip")
    | others => Error(Error.NotSupportedOS(others))
    }

    // find the corresponding asset
    expectedName
    ->Result.flatMap(name => {
      let matched = release.assets->Array.keep(asset => asset.name == name)
      switch matched[0] {
      | None => Error(Error.NotSupportedOS(os))
      | Some(asset) => Ok((release, asset))
      }
    })
    ->Promise.resolved
  }

  Release.getReleasesFromGitHub()
  ->Promise.flatMapOk(getMatchingRelease)
  ->Promise.flatMapOk(getMatchingAsset)
  ->Promise.flatMapOk(((release, asset)) => {
    let url = Nd.Url.parse(asset.url)
    let httpOptions = {
      "host": url["host"],
      "path": url["path"],
      "headers": {
        "User-Agent": "gcl-vscode",
      },
    }
    HTTP.getWithRedirects(httpOptions)->Promise.mapOk(res => (release, asset, res))
  })
  ->Promise.flatMapOk(((release, asset, res)) => {
    let (promise, resolve) = Promise.pending()
    // take the "macos.zip" part from names like "gcl-macos.zip"
    let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
    // the final path to store the language server
    let outputPath = Node_path.join2(globalStoragePath, "gcl-" ++ release.tagName ++ "-" ++ osName)
    let zipPath = outputPath ++ ".zip"
    let zipFileStream = Nd.Fs.createWriteStream(zipPath)
    // download the zip file to the outputPath ++ ".zip"
    zipFileStream->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotUnzipFile(exn))))->ignore
    zipFileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok(outputPath)))->ignore
    res->NodeJs.Http.IncomingMessage.pipe(zipFileStream)->ignore 

    Js.log(release)
    Js.log(asset)
    Js.log(outputPath)

    promise
  })->Promise.tapOk(outputPath => {
    // unzip the downloaded file 

    // let zipPath = outputPath ++ ".zip"
    // let zipFileStream = Nd.Fs.createWriteStream(zipPath)

    // zipFileStream->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotUnzipFile(exn))))->ignore
    // zipFileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok(outputPath)))->ignore
    // res->NodeJs.Http.IncomingMessage.pipe(zipFileStream)->ignore 

    ()
    // Ok()
  })->Promise.tapOk(Js.log)

}



    // // res => outputFileStream
    // outputFileStream->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))->ignore
    // outputFileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok(outputPath)))->ignore
    // res->NodeJs.Http.IncomingMessage.pipe(outputFileStream)->ignore 
