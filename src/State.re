open Belt;
open VSCode;

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
  mutable mode: GCL.mode,
  mutable decorations: array(TextEditorDecorationType.t),
  mutable specifications: array(Response.Specification.t),
  mutable connection: option(Connection.t),
  onDestroyEventEmitter: AgdaModeVscode.Event.t(unit),
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

//
// GCL connection/disconnection
//

// connect if not connected yet
let connect = state =>
  switch (state.connection) {
  | None =>
    Connection.make(Config.getGCLPath, Config.setGCLPath)
    ->Promise.mapError(e => Error.Connection(e))
    ->Promise.tapOk(conn => state.connection = Some(conn))
  | Some(connection) => Promise.resolved(Ok(connection))
  };
let disconnect = state =>
  switch (state.connection) {
  | None => Promise.resolved()
  | Some(connection) => Connection.disconnect(connection)
  };
let sendRequest = (state, request) => {
  let value = Request.encode(request);
  Js.log2("<<<", value);

  state
  ->connect
  ->Promise.flatMapOk(conn => {
      Connection.send(value, conn)
      ->Promise.mapError(e => Error.Connection(e))
    })
  ->Promise.flatMapOk(result => {
      Js.log2(
        ">>>",
        Js.String.substring(~from=0, ~to_=200, Js.Json.stringify(result)),
      );

      // catching exceptions occured when decoding JSON values
      switch (result |> Response.decode) {
      | value => Promise.resolved(Ok(value))
      | exception (Json.Decode.DecodeError(msg)) =>
        Promise.resolved(Error(Error.Decode(msg, result)))
      };
    });
};

//
// construction/destruction
//

let destroy = state => {
  state.view->View.destroy;
  state.onDestroyEventEmitter.emit();
  state.onDestroyEventEmitter.destroy();
  state.decorations->Array.forEach(Editor.Decoration.destroy);
  state->disconnect;
};

let make = (extentionPath, editor) => {
  // view initialization
  let view = View.make(extentionPath, editor);

  let state = {
    editor,
    view,
    mode: GCL.WP1,
    decorations: [||],
    specifications: [||],
    connection: None,
    onDestroyEventEmitter: AgdaModeVscode.Event.make(),
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
        Range.contains(range, cursor);
      })
    ->Array.forEach(spec =>
        switch (smallestHole^) {
        | None => smallestHole := Some(spec)
        | Some(spec') =>
          if (Range.containsRange(
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
    let startingLine = Position.line(Range.start(range)) + 1;
    let endingLine = Position.line(Range.end_(range)) - 1;

    let start =
      TextDocument.lineAt(doc, startingLine)->TextLine.range->Range.start;
    let end_ =
      TextDocument.lineAt(doc, endingLine)->TextLine.range->Range.end_;
    Range.make(start, end_);
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
        let start = Range.start(range);
        // delete text
        Editor.deleteText(doc, range)
        ->Promise.flatMap(
            fun
            | false => Promise.resolved(false)
            | true => Editor.insertText(doc, start, Js.String.trim(payload)),
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
    Editor.insertText(doc, point, assertion);
  };
};
