open LSP__Common;

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/node/main.ts#L99-L110

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L332-L350
module SynchronizeOptions = {
  type t = {fileEvents: option(array(VSCode.FileSystemWatcher.t))};
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L266-L285
module ErrorHandler = {
  type errorAction =
    | Continue
    | Shutdown;
  type closeAction =
    | DoNotRestart
    | Restart;
  type t = {
    error: (Error.t, option(Message.t), option(int)) => int,
    closed: unit => int,
  };
  let make = (f, g) => {
    error: (e, m, i) =>
      switch (f(e, m, i)) {
      | Continue => 1
      | Shutdown => 2
      },
    closed: () =>
      switch (g()) {
      | DoNotRestart => 1
      | Restart => 2
      },
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L470-L504
// module Middleware = {
//   type next('p, 'r) = (unit, 'p, 'p => 'r) => 'r;
//   [@bs.deriving abstract]
//   type t = {
//     // events
//     [@bs.optional]
//     didOpen: next(VSCode.TextDocument.t, unit),
//     [@bs.optional]
//     didChange: next(VSCode.TextDocumentChangeEvent.t, unit),
//     [@bs.optional]
//     willSave: next(VSCode.TextDocumentWillSaveEvent.t, unit),
//     [@bs.optional]
//     willSaveWaitUntil:
//       next(
//         VSCode.TextDocumentWillSaveEvent.t,
//         Promise.t(array(VSCode.TextEdit.t)),
//       ),
//     [@bs.optional]
//     didSave: next(VSCode.TextDocument.t, unit),
//     [@bs.optional]
//     didClose: next(VSCode.TextDocument.t, unit),
//     // middlewares
//     [@bs.optional]
//     handleDiagnostics:
//       (
//         unit,
//         VSCode.Uri.t,
//         array(VSCode.Diagnostic.t),
//         (unit, Uri.t, array(VSCode.Diagnostic.t)) => unit
//       ) =>
//       unit,
//     [@bs.optional]
//     provideCompletionItem:
//       (
//         unit,
//         VSCode.TextDocument.t,
//         VSCode.Position.t,
//         VSCode.CompletionContext.t,
//         VSCode.CancellationToken.t,
//         array(VSCode.Diagnostic.t),
//         (
//         unit,
//         VSCode.TextDocument.t,
//         VSCode.Position.t,
//         VSCode.CompletionContext.t,
//         VSCode.CancellationToken.t,
//         array(VSCode.Diagnostic.t),)
//         (unit, Uri.t, array(VSCode.Diagnostic.t)) => unit
//       ) =>
//       unit,
//     // provideCompletionItem?: (this: void, document: TextDocument, position: VPosition, context: VCompletionContext, token: CancellationToken, next: ProvideCompletionItemsSignature) => ProviderResult<VCompletionItem[] | VCompletionList>;
//     // resolveCompletionItem?: (this: void, item: VCompletionItem, token: CancellationToken, next: ResolveCompletionItemSignature) => ProviderResult<VCompletionItem>;
//     // provideHover?: (this: void, document: TextDocument, position: VPosition, token: CancellationToken, next: ProvideHoverSignature) => ProviderResult<VHover>;
//     // provideSignatureHelp?: (this: void, document: TextDocument, position: VPosition, context: VSignatureHelpContext, token: CancellationToken, next: ProvideSignatureHelpSignature) => ProviderResult<VSignatureHelp>;
//     // provideDefinition?: (this: void, document: TextDocument, position: VPosition, token: CancellationToken, next: ProvideDefinitionSignature) => ProviderResult<VDefinition | VDefinitionLink[]>;
//     // provideReferences?: (this: void, document: TextDocument, position: VPosition, options: { includeDeclaration: boolean; }, token: CancellationToken, next: ProvideReferencesSignature) => ProviderResult<VLocation[]>;
//     // provideDocumentHighlights?: (this: void, document: TextDocument, position: VPosition, token: CancellationToken, next: ProvideDocumentHighlightsSignature) => ProviderResult<VDocumentHighlight[]>;
//     // provideDocumentSymbols?: (this: void, document: TextDocument, token: CancellationToken, next: ProvideDocumentSymbolsSignature) => ProviderResult<VSymbolInformation[] | VDocumentSymbol[]>;
//     // provideWorkspaceSymbols?: (this: void, query: string, token: CancellationToken, next: ProvideWorkspaceSymbolsSignature) => ProviderResult<VSymbolInformation[]>;
//     // provideCodeActions?: (this: void, document: TextDocument, range: VRange, context: VCodeActionContext, token: CancellationToken, next: ProvideCodeActionsSignature) => ProviderResult<(VCommand | VCodeAction)[]>;
//     // resolveCodeAction?: (this: void, item:  VCodeAction, token: CancellationToken, next: ResolveCodeActionSignature) => ProviderResult<VCodeAction>;
//     // provideCodeLenses?: (this: void, document: TextDocument, token: CancellationToken, next: ProvideCodeLensesSignature) => ProviderResult<VCodeLens[]>;
//     // resolveCodeLens?: (this: void, codeLens: VCodeLens, token: CancellationToken, next: ResolveCodeLensSignature) => ProviderResult<VCodeLens>;
//     // provideDocumentFormattingEdits?: (this: void, document: TextDocument, options: VFormattingOptions, token: CancellationToken, next: ProvideDocumentFormattingEditsSignature) => ProviderResult<VTextEdit[]>;
//     // provideDocumentRangeFormattingEdits?: (this: void, document: TextDocument, range: VRange, options: VFormattingOptions, token: CancellationToken, next: ProvideDocumentRangeFormattingEditsSignature) => ProviderResult<VTextEdit[]>;
//     // provideOnTypeFormattingEdits?: (this: void, document: TextDocument, position: VPosition, ch: string, options: VFormattingOptions, token: CancellationToken, next: ProvideOnTypeFormattingEditsSignature) => ProviderResult<VTextEdit[]>;
//     // provideRenameEdits?: (this: void, document: TextDocument, position: VPosition, newName: string, token: CancellationToken, next: ProvideRenameEditsSignature) => ProviderResult<VWorkspaceEdit>;
//     // prepareRename?: (this: void, document: TextDocument, position: VPosition, token: CancellationToken, next: PrepareRenameSignature) => ProviderResult<VRange | { range: VRange, placeholder: string }>;
//     // provideDocumentLinks?: (this: void, document: TextDocument, token: CancellationToken, next: ProvideDocumentLinksSignature) => ProviderResult<VDocumentLink[]>;
//     // resolveDocumentLink?: (this: void, link: VDocumentLink, token: CancellationToken, next: ResolveDocumentLinkSignature) => ProviderResult<VDocumentLink>;
//     // executeCommand?: (this: void, command: string, args: any[], next: ExecuteCommandSignature) => ProviderResult<any>;
//     // workspace?: WorkspaceMiddleware;
//   };
// };

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L174-L177
module ConnectionOptions = {
  type t = {
    // TODO: CancellationStrategy
    // cancellationStrategy: LSP__Protocol.CancellationStrategy.t,
    maxRestartCount: option(int),
  };
};

// https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/client/src/common/client.ts#L506
module LanguageClientOptions = {
  type uriConverters = {
    code2Protocol: string => VSCode.Uri.t,
    protocol2Code: VSCode.Uri.t => string,
  };
  type markdown = {isTrusted: option(bool)};

  [@bs.deriving abstract]
  type t = {
    [@bs.optional]
    documentSelector: VSCode.DocumentSelector.t,
    [@bs.optional]
    synchronize: SynchronizeOptions.t,
    [@bs.optional]
    diagnosticCollectionName: string,
    [@bs.optional]
    outputChannel: VSCode.OutputChannel.t,
    [@bs.optional]
    outputChannelName: string,
    [@bs.optional]
    traceOutputChannel: VSCode.OutputChannel.t,
    // TODO: revealOutputChannelOn
    [@bs.optional]
    revealOutputChannelOn: int,
    [@bs.optional]
    stdioEncoding: string,
    // [@bs.optional]
    // initializationOptions: 'a,
    [@bs.optional]
    initializationFailedHandler: Error.t => bool,
    [@bs.optional]
    progressOnInitialization: bool,
    [@bs.optional]
    errorHandler: ErrorHandler.t,
    // [@bs.optional]
    // middleware: 'middleware,
    [@bs.optional]
    uriConverters,
    [@bs.optional]
    workspaceFolder: VSCode.WorkspaceFolder.t,
    [@bs.optional]
    connectionOptions: ConnectionOptions.t,
    [@bs.optional]
    markdown,
  };
};
