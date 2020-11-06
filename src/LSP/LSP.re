// temp file for LSP client bindings
// https://github.com/microsoft/vscode-languageserver-node/blob/release/client/7.0.0-next.12/client/src/node/main.t

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L27-L44
module Executable = {
  type options = {
    cwd: option(string),
    env: array(string),
    detached: option(bool),
    shell: option(bool),
  };
  type t = {
    command: string,
    args: array(string),
    options: option(options),
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L53-L83
module Transport = {
  type t =
    | StdIO
    | IPC
    | Pipe
    | Socket(int);
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L85-L97
module NodeModule = {
  module ForkOptions = {
    type t = {
      cwd: option(string),
      env: array(string),
      encoding: option(bool),
      execArgv: array(string),
    };
  };

  [@unboxed]
  type transport =
    | Any('a): transport;

  type t = {
    [@bs.as "module"]
    module_: string,
    transport: option(transport),
    args: option(array(string)),
    runtime: option(string),
    options: option(ForkOptions.t),
  };
  let make = (path, ~transport=?, ~args=?, ~runtime=?, ~options=?, ()): t => {
    let transport =
      transport->Belt.Option.map(
        fun
        | Transport.StdIO => [%raw "1"]
        | IPC => [%raw "2"]
        | Pipe => [%raw "3"]
        | Socket(n) => [%raw "{ kind: 4 , port: n }"],
      );
    {module_: path, transport, args, runtime, options};
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L99-L110
module StreamInfo = {
  type t = {
    writer: NodeJs.Stream.writable,
    reader: NodeJs.Stream.readable,
    detached: option(bool),
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L112-L122
module ChildProcessInfo = {
  type t = {
    process: NodeJs.ChildProcess.t,
    detached: bool,
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L124
module ServerOptions = {
  // DOTO: MessageTransport
  type t = NodeModule.t;
  // | RunExecutable(Executable.t)
  // | RunAndDebugExecutable(Executable.t, Executable.t)
  // | RunNodeModule(NodeModule.t)
  // | RunAndDebugNodeModule(NodeModule.t, NodeModule.t)
  // | ChildProcess(unit => Promise.t(NodeJs.ChildProcess.t))
  // | StreamInfo(unit => Promise.t(StreamInfo.t))
  // | ChildProcessInfo(unit => Promise.t(ChildProcessInfo.t));
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L506
module LanguageClientOptions = {
  // type t = {documentSelector: option(VSCode.DocumentSelector.t)};
  // documentSelector?: DocumentSelector | string[];
  // synchronize?: SynchronizeOptions;
  // diagnosticCollectionName?: string;
  // outputChannel?: OutputChannel;
  // outputChannelName?: string;
  // traceOutputChannel?: OutputChannel;
  // revealOutputChannelOn?: RevealOutputChannelOn;
  // /**
  //  * The encoding use to read stdout and stderr. Defaults
  //  * to 'utf8' if ommitted.
  //  */
  // stdioEncoding?: string;
  // initializationOptions?: any | (() => any);
  // initializationFailedHandler?: InitializationFailedHandler;
  // progressOnInitialization?: boolean;
  // errorHandler?: ErrorHandler;
  // middleware?: Middleware;
  // uriConverters?: {
  // 	code2Protocol: c2p.URIConverter,
  // 	protocol2Code: p2c.URIConverter
  // };
  // workspaceFolder?: VWorkspaceFolder;
  // connectionOptions?: ConnectionOptions;
  // markdown?: {
  // 	isTrusted?: boolean;
  // }
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L126
module LanguageClient = {
  type t;
  // constructor
  [@bs.module "vscode-languageclient"] [@bs.new]
  external make:
    (string, string, ServerOptions.t, LSP__Client.LanguageClientOptions.t) => t =
    "LanguageClient";

  // methods
  [@bs.send] external stop: t => unit = "stop";
  [@bs.send] external start: t => VSCode.Disposable.t = "start";
};

// export type ServerOptions = Executable | { run: Executable; debug: Executable; } | { run: NodeModule; debug: NodeModule } | NodeModule | (() => Promise<ChildProcess | StreamInfo | MessageTransports | ChildProcessInfo>);

// type serverOptions = {
//     run: option()
