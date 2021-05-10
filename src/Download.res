module Nd = {
  module Fs = {
    @bs.module("fs")
    external mkdirSync: string => unit = "mkdirSync"

    @bs.module("fs")
    external unlink_raw: (string, Js.null<Js.Exn.t> => unit) => unit = "unlink"
    let unlink = path => {
      let (promise, resolve) = Promise.pending()
      unlink_raw(path, error => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok())
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @bs.module("fs")
    external rename_raw: (string, string, Js.null<Js.Exn.t> => unit) => unit = "rename"
    let rename = (old, new) => {
      let (promise, resolve) = Promise.pending()
      rename_raw(old, new, error => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok())
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @bs.module("fs")
    external readFile_raw: (string, (Js.null<Js.Exn.t>, NodeJs.Buffer.t) => unit) => unit =
      "readFile"
    let readFile = path => {
      let (promise, resolve) = Promise.pending()
      readFile_raw(path, (error, buffer) => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok(buffer))
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @bs.module("fs")
    external createWriteStream: string => NodeJs.Fs.WriteStream.t = "createWriteStream"

    @bs.module("fs")
    external createWriteStreamWithOptions: (string, {"mode": int}) => NodeJs.Fs.WriteStream.t =
      "createWriteStream"
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

module Error = {
  type t =
    // parsing
    | ResponseParseError(string)
    | ResponseDecodeError(string, Js.Json.t)
    // network
    | NoRedirectLocation
    | ServerResponseError(Js.Exn.t)
    //
    | NoMatchingVersion(string)
    | NotSupportedOS(string)
    // file system
    | CannotDeleteFile(Js.Exn.t)
    | CannotRenameFile(Js.Exn.t)
    | CannotWriteFile(Js.Exn.t)
    | CannotUnzipFileWithExn(Js.Exn.t)
    | CannotUnzipFile
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

// module for unzipping downloaded files
module Unzip = {
  module Yauzl = {
    module Entry = {
      type t
    }
    module ZipFile = {
      type t
      @bs.send
      external openReadStream: (
        t,
        Entry.t,
        (Js.null<Js.Exn.t>, option<NodeJs.Stream.Readable.t<NodeJs.Buffer.t>>) => unit,
      ) => unit = "openReadStream"

      @bs.send
      external onEntry: (t, @as("entry") _, Entry.t => unit) => unit = "on"
    }

    @bs.module("yauzl")
    external open_: (string, (Js.null<Js.Exn.t>, option<ZipFile.t>) => unit) => unit = "open"
  }

  let run = (src, dest) => {
    Js.log("[ download ][ unzip ] " ++ src ++ " => " ++ dest)
    let (promise, resolve) = Promise.pending()

    // chmod 744 the executable
    let fileStream = Nd.Fs.createWriteStreamWithOptions(dest, {"mode": 0o744})
    // listens on "Error" and "Close"
    fileStream
    ->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotUnzipFileWithExn(exn))))
    ->ignore
    fileStream
    ->NodeJs.Fs.WriteStream.onClose(() => {
      resolve(Ok())
    })
    ->ignore

    // start unzipping the file
    Yauzl.open_(src, (err, result) => {
      switch Js.nullToOption(err) {
      | Some(err) => resolve(Error(Error.CannotUnzipFileWithExn(err)))
      | None =>
        switch result {
        | None => resolve(Error(CannotUnzipFile))
        | Some(zipFile) =>
          // We only expect *one* file inside each zip
          zipFile->Yauzl.ZipFile.onEntry(entry => {
            zipFile->Yauzl.ZipFile.openReadStream(entry, (err2, result2) => {
              switch Js.nullToOption(err2) {
              | Some(err2) => resolve(Error(Error.CannotUnzipFileWithExn(err2)))
              | None =>
                switch result2 {
                | None => resolve(Error(CannotUnzipFile))
                | Some(readStream) => readStream->NodeJs.Stream.Readable.pipe(fileStream)->ignore
                }
              }
            })
          })
        }
      }
    })
    promise
  }
}

module Release = {
  module Asset = {
    type t = {
      name: string,
      url: string,
    }

    let decode = json => {
      open Json.Decode
      {
        url: json |> field("browser_download_url", string),
        name: json |> field("name", string),
      }
    }
  }

  type t = {
    tagName: string,
    assets: array<Asset.t>,
  }

  let decode = json => {
    open Json.Decode
    {
      tagName: json |> field("tag_name", string),
      assets: json |> field("assets", array(Asset.decode)),
    }
  }

  let parseMetadata = json =>
    try {
      Ok(json |> Json.Decode.array(decode))
    } catch {
    | Json.Decode.DecodeError(e) => Error(Error.ResponseDecodeError(e, json))
    }

  // NOTE: no caching
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
}

let getCurrentReleaseAndAsset = () => {
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
}

let toDestPath = (context, (release: Release.t, asset: Release.Asset.t)) => {
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  // take the "macos" part from names like "gcl-macos.zip"
  let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
  // the final path to store the language server
  Node_path.join2(globalStoragePath, "gcl-" ++ release.tagName ++ "-" ++ osName)
}

let download = (srcUrl, destPath) => {
  let url = Nd.Url.parse(srcUrl)
  let httpOptions = {
    "host": url["host"],
    "path": url["path"],
    "headers": {
      "User-Agent": "gcl-vscode",
    },
  }

  HTTP.getWithRedirects(httpOptions)->Promise.flatMapOk(res => {
    let (promise, resolve) = Promise.pending()
    let fileStream = Nd.Fs.createWriteStream(destPath)
    fileStream
    ->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))
    ->ignore
    fileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok()))->ignore
    res->NodeJs.Http.IncomingMessage.pipe(fileStream)->ignore
    promise
  })
}

let downloadLanguageServer = (context, (release: Release.t, asset: Release.Asset.t)) => {
  let destPath = toDestPath(context, (release, asset))
  // suffix with ".download" whilst downloading
  download(asset.url, destPath ++ ".zip.download")
  // remove the ".download" suffix after download
  ->Promise.flatMapOk(() =>
    Nd.Fs.rename(
      destPath ++ ".zip.download",
      destPath ++ ".zip",
    )->Promise.mapError(e => Error.CannotRenameFile(e))
  )
  // unzip the downloaded file
  ->Promise.flatMapOk(() => {
    Unzip.run(destPath ++ ".zip", destPath)
  })
  // remove the zip file
  ->Promise.flatMapOk(() =>
    Nd.Fs.unlink(destPath ++ ".zip")->Promise.mapError(e => Error.CannotDeleteFile(e))
  )
  ->Promise.mapOk(() => destPath)
}

module State = {
  type t =
    | Downloaded(string)
    | Downloading(Promise.t<result<string, Error.t>>)

  let checkExistingDownload = context => {
    // let (promise, resolve) = Promise.pending()
    // create a directory for `context.globalStoragePath` if it doesn't exist
    let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
    if !Node.Fs.existsSync(globalStoragePath) {
      Nd.Fs.mkdirSync(globalStoragePath)
    }

    // devise the expected file name of the language server and see if the OS is supported
    let getExpectedFileName = {
      switch Node_process.process["platform"] {
      | "darwin" => Ok("gcl-" ++ Constant.version ++ "-macos")
      | "linux" => Ok("gcl-" ++ Constant.version ++ "-ubuntu")
      | "win32" => Ok("gcl-" ++ Constant.version ++ "-windows")
      | others => Error(Error.NotSupportedOS(others))
      }
    }

    getExpectedFileName->Belt.Result.map(expected => {
      // find the current asset from `context.globalStoragePath`
      let fileNames = NodeJs.Fs.readdirSync(globalStoragePath)
      let downloaded = fileNames->Js.Array2.some(actual => expected == actual)

      if downloaded {
        let path = NodeJs.Path.join2(globalStoragePath, expected)
        Some(path)
      } else {
        None
      }
    })
  }
}

module Module = {
  let state: ref<option<State.t>> = ref(None)

  let get = (context): Promise.t<result<string, Error.t>> => {
    switch state.contents {
    | None =>
      // not initialized yet
      switch State.checkExistingDownload(context) {
      | Ok(None) =>
        Js.log("[ download ][ init ] No existing download, fetch a new one")
        let (promise, resolve) = Promise.pending()
        state := Some(Downloading(promise))
        getCurrentReleaseAndAsset()
        ->Promise.flatMapOk(downloadLanguageServer(context))
        ->Promise.tap(resolve)
      | Ok(Some(path)) =>
        Js.log("[ download ][ init ] Got existing download")
        state := Some(Downloaded(path))
        Promise.resolved(Ok(path))
      | Error(error) => Promise.resolved(Error(error))
      }
    | Some(Downloaded(path)) =>
      Js.log("[ download ] Got existing download")
      Promise.resolved(Ok(path))
    // returns a promise that will be resolved once the download's been completed
    | Some(Downloading(promise)) =>
      Js.log("[ download ] Already downloading")
      promise
    }
  }
}

include Module

// // initiate download
// getCurrentReleaseAndAsset()
// ->Promise.flatMapOk(downloadLanguageServer(context))
// ->Promise.mapOk(_ => ())

// let downloadLanguageServerCached = (context, outputPath) => {
//   // let downloadPath = outputPath ++ ".zip.download"
//   // if !Node_fs.existsSync(downloadPath) {
//   //   Nd.Fs.mkdirSync(globalStoragePath)
//   // }
//   ()
// }

// // res => outputFileStream
// outputFileStream->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))->ignore
// outputFileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok(outputPath)))->ignore
// res->NodeJs.Http.IncomingMessage.pipe(outputFileStream)->ignore 
