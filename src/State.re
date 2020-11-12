open VSCode;
module VSRange = Range;
open Belt;

//
// Editor Configuration
//
module Config = {
  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

type t = {
  editor: TextEditor.t,
  view: View.t,
  mutable decorations: array(TextEditorDecorationType.t),
  mutable specifications: array(Response.Specification.t),
  // mutable connection: option(Connection.t),
  client: Client.LanguageClient.t,
  onDestroyEventEmitter: AgdaModeVscode.Event.t(unit),
  disposables: array(Disposable.t),
};

//
// getters
//
let setSpecifications = (state, specifications) =>
  state.specifications = specifications;

//
// events
//
let onDestroy = (state, callback) => {
  state.onDestroyEventEmitter.on(callback)->Disposable.make;
};

let sendRequest = (state, request) => {
  let value = Request.encode(request);
  Js.log2("<<<", value);

  state.client
  ->Client.LanguageClient.onReady
  ->Promise.Js.toResult
  ->Promise.flatMapOk(() => {
      state.client
      ->Client.LanguageClient.sendRequest("guacamole", value)
      ->Promise.Js.toResult
    })
  ->Promise.flatMapOk(result => {
      Js.log2(">>>", result);
      // catching exceptions occured when decoding JSON values
      switch (result |> Response.decode) {
      | value => Promise.resolved(Ok(value))
      | exception (Json.Decode.DecodeError(msg)) =>
        Promise.resolved(Error(Error.Decode(msg, result)))
      };
    })
  ->Promise.mapError(error => {Error.LSP(Error.fromJsError(error))});
};

//
// construction/destruction
//

let destroy = state => {
  state.view->View.destroy;
  state.onDestroyEventEmitter.emit();
  state.onDestroyEventEmitter.destroy();
  state.decorations->Array.forEach(AgdaModeVscode.Editor.Decoration.destroy);
  state.disposables->Array.forEach(Disposable.dispose);
  state.client->Client.LanguageClient.stop;
};

// https://code.visualstudio.com/api/references/vscode-api#Command
module Command = {
  type t('a) = {
    arguments: option(array('a)),
    command: string,
    title: string,
    tooltip: option(string),
  };
};

// https://code.visualstudio.com/api/references/vscode-api#FoldingContext
module FoldingContext = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FoldingRangeKind
module FoldingRangeKind = {
  type t =
    | Comment
    | Imports
    | Region;
};

// https://code.visualstudio.com/api/references/vscode-api#FoldingRange
module FoldingRange = {
  type t;
  // constructor
  [@bs.module "vscode"] [@bs.new]
  external make: (int, int) => t = "FoldingRange";
  [@bs.module "vscode"] [@bs.new]
  external makeWithKind: (int, int, int) => t = "FoldingRange";
  let makeWithKind = (start, end_, kind: FoldingRangeKind.t) => {
    switch (kind) {
    | Comment => makeWithKind(start, end_, 1)
    | Imports => makeWithKind(start, end_, 2)
    | Region => makeWithKind(start, end_, 3)
    };
  };
  // properties
  [@bs.get] external end_: t => int = "end";
  [@bs.get] external kind: t => option(FoldingRangeKind.t) = "kind";
  [@bs.get] external start: t => int = "start";
};

// https://code.visualstudio.com/api/references/vscode-api#FoldingRangeProvider
module FoldingRangeProvider = {
  type t = {
    provideFoldingRanges:
      (TextDocument.t, FoldingContext.t, CancellationToken.t) =>
      ProviderResult.t(array(FoldingRange.t)),
  };
};

[@bs.module "vscode"] [@bs.scope "languages"]
external registerFoldingRangeProvider:
  (DocumentSelector.t, FoldingRangeProvider.t) => Disposable.t =
  "registerFoldingRangeProvider";

let make = (extentionPath, editor, client) => {
  let disposables = [||];
  // start the LSP client
  client->Client.LanguageClient.start;
  // view initialization
  let view = View.make(extentionPath, editor);

  // registering LSP features
  let docSelector = [|
    VSCode.DocumentFilterOrString.documentFilter(
      VSCode.DocumentFilter.{
        language: Some("gcl"),
        pattern: None,
        scheme: None,
      },
    ),
  |];

  module CodeLens = {
    // let codeLensProvider =
    //   CodeLensProvider.{
    //     onDidChangeCodeLenses: None,
    //     resolveCodeLens: (lens, _) => {
    //       Js.log(lens);
    //       Js.log("codeLensProvider.resolveCodeLens invoked");
    //       Some(Promise.resolved(lens));
    //     },
    //     provideCodeLenses: (_, _) => {
    //       Js.log("codeLensProvider.provideCodeLenses invoked");
    //       let lenses = [|
    //         CodeLens.makeWithCommand(
    //           VSCode.Range.make(
    //             VSCode.Position.make(0, 0),
    //             VSCode.Position.make(0, 0),
    //           ),
    //           Command.{
    //             arguments: None,
    //             command: "guacamole.reload",
    //             title: "title",
    //             tooltip: Some("tooltip"),
    //           },
    //         ),
    //         CodeLens.makeWithCommand(
    //           VSCode.Range.make(
    //             VSCode.Position.make(0, 0),
    //             VSCode.Position.make(0, 0),
    //           ),
    //           Command.{
    //             arguments: None,
    //             command: "guacamole.reload",
    //             title: "title 2",
    //             tooltip: Some("tooltip"),
    //           },
    //         ),
    //         CodeLens.makeWithCommand(
    //           VSCode.Range.make(
    //             VSCode.Position.make(0, 10),
    //             VSCode.Position.make(0, 10),
    //           ),
    //           Command.{
    //             arguments: None,
    //             command: "guacamole.reload",
    //             title: "title 2",
    //             tooltip: Some("tooltip"),
    //           },
    //         ),
    //       |];
    //       Some(Promise.resolved(lenses));
    //     },
    //   };
    // registerCodeLensProvider(docSelector, codeLensProvider)
    // ->Js.Array.push(disposables)
    // ->ignore;
  };

  let foldingRangeProvider =
    FoldingRangeProvider.{
      provideFoldingRanges: (_, _, _) => {
        let foldingRanges = [|FoldingRange.makeWithKind(3, 5, Region)|];
        Some(Promise.resolved(foldingRanges));
      },
    };
  registerFoldingRangeProvider(docSelector, foldingRangeProvider)
  ->Js.Array.push(disposables)
  ->ignore;

  let state = {
    editor,
    view,
    decorations: [||],
    specifications: [||],
    client,
    // connection: None,
    onDestroyEventEmitter: AgdaModeVscode.Event.make(),
    disposables,
  };

  state;
};

//
// View-related
//

let show = state => state.view->View.show;
let hide = state => state.view->View.hide;
let sendRequestToView = (state, request) => {
  View.send(state.view, request);
};
let display = (state, header, body) => {
  sendRequestToView(state, ViewType.Request.Display(header, body));
};
//
// Spec-related
//

module Spec = {
  // find the hole containing the cursor
  let fromCursorPosition = state => {
    let cursor = state.editor->TextEditor.selection->Selection.end_;
    // find the smallest hole containing the cursor, as there might be many of them
    let smallestHole = ref(None);
    state.specifications
    ->Array.keep(spec => {
        let range = GCL.Loc.toRange(spec.loc);
        VSRange.contains(range, cursor);
      })
    ->Array.forEach(spec =>
        switch (smallestHole^) {
        | None => smallestHole := Some(spec)
        | Some(spec') =>
          if (VSRange.containsRange(
                GCL.Loc.toRange(spec.loc),
                GCL.Loc.toRange(spec'.loc),
              )) {
            smallestHole := Some(spec);
          }
        }
      );
    smallestHole^;
  };

  let getPayloadRange = (doc, spec: Response.Specification.t) => {
    let range = GCL.Loc.toRange(spec.loc);
    let startingLine = Position.line(VSRange.start(range)) + 1;
    let endingLine = Position.line(VSRange.end_(range)) - 1;

    let start =
      TextDocument.lineAt(doc, startingLine)->TextLine.range->VSRange.start;
    let end_ =
      TextDocument.lineAt(doc, endingLine)->TextLine.range->VSRange.end_;
    VSRange.make(start, end_);
  };
  let getPayload = (doc, spec) => {
    // return the text in the targeted hole
    let innerRange = getPayloadRange(doc, spec);
    TextDocument.getText(doc, Some(innerRange));
  };

  let resolve = (state, i) => {
    let specs = state.specifications->Array.keep(spec => spec.id == i);
    specs[0]
    ->Option.forEach(spec => {
        let doc = TextEditor.document(state.editor);
        let payload = getPayload(doc, spec);
        let range = GCL.Loc.toRange(spec.loc);
        let start = VSRange.start(range);
        // delete text
        AgdaModeVscode.Editor.Text.delete(doc, range)
        ->Promise.flatMap(
            fun
            | false => Promise.resolved(false)
            | true =>
              AgdaModeVscode.Editor.Text.insert(
                doc,
                start,
                Js.String.trim(payload),
              ),
          )
        ->Promise.get(_ => ());
      });
    Promise.resolved();
  };

  let insert = (state, lineNo, expr) => {
    let assertion = "{ " ++ GCL.Syntax.Expr.toString(expr) ++ " }\n";
    let point = Position.make(lineNo - 1, 0);
    // insert the assertion
    let doc = TextEditor.document(state.editor);
    AgdaModeVscode.Editor.Text.insert(doc, point, assertion);
  };
};
