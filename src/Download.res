module Nd = {
  module Fs = {
    @bs.module("fs")
    external mkdirSync: string => unit = "mkdirSync"
  }

  module Https = {
    @bs.module("https")
    external get: (
      {"host": string, "path": string, "headers": {"User-Agent": string}},
      NodeJs.Http.ServerResponse.t => unit,
    ) => unit = "get"

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

module SupportedOS = {
  type t = MacOS | Linux | Windows

  let value = () =>
    switch Node_process.process["platform"] {
    | "darwin" => Some(MacOS)
    | "linux" => Some(Linux)
    | "win32" => Some(Windows)
    | _ => None
    }

  let toAssetSuffix = x =>
    switch x {
    | MacOS => "macos"
    | Linux => "linux"
    | Windows => "windows"
    }
}

let getReleases = context => {
  let httpOptions = {
    "host": "api.github.com",
    "path": "/repos/scmlab/gcl/releases",
    "headers": {
      "User-Agent": "gcl-vscode",
    },
  }

  let (promise, resolve) = Promise.pending()
          // res.on('data', (d) => (data += d));
          // res.on('error', reject);
          // res.on('close', () => {
          //   resolve(data);
          // });

  let body = ref("")

  Nd.Https.get(httpOptions, res => {
    res->NodeJs.Http.ServerResponse.onData(buf => body := body.contents ++ NodeJs.Buffer.toString(buf) )->ignore
    res->NodeJs.Http.ServerResponse.onClose(() => resolve(body.contents) )->ignore
  })

  promise
}

let downloadLanguageServer = context => {
  // create a directory for context.globalStoragePath if it doesn't exist
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)
  if !Node_fs.existsSync(globalStoragePath) {
    Nd.Fs.mkdirSync(globalStoragePath)
  }

  let supportedOS = SupportedOS.value
  // let assetName = "gcl-" ++ SupportedOS.toAssetSuffix(supportedOS)

  // host: 'api.github.com',
  // path: '/repos/haskell/haskell-language-server/releases',

  // let srcUrl = Nd.Url.parse(src)

  // let httpOptions = {
  //   "host": srcUrl["host"],
  //   "path": srcUrl["path"],
  //   "protocol": srcUrl["protocol"],
  //   "port": srcUrl["port"],
  //   "headers": string,
  // }

  // let httpOptions = {
  //   "host": "api.github.com",
  //   "path": "/repos/scmlab/gcl/releases",
  //   "headers": string,
  // }
  // Nd.Https.get(httpOptions)
}
