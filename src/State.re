open Belt;

type t = {
  editor: VSCode.TextEditor.t,
  document: VSCode.TextDocument.t,
  filePath: string,
  viewReq: Req.t(ViewType.Request.t, bool),
  viewResChan: Chan.t(ViewType.Response.t),
  mutable decorations: array(VSCode.TextEditorDecorationType.t),
  mutable subscriptions: array(VSCode.Disposable.t),
};

let subscribe = (disposable, state) =>
  disposable->Js.Array.push(state.subscriptions)->ignore;

let handleResponse = (state: t, response) =>
  switch (response) {
  | Response.Error(error) => Js.log(error)
  | OK(i, pos, _, props) =>
    state.viewReq
    ->Req.send(
        ViewType.Request.Display(
          Plain("Proof Obligations"),
          ProofObligations(i, pos, props),
        ),
      )
    ->ignore
  | Decorate(locs) =>
    // destroy old decorations
    state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose);
    state.decorations = [||];

    let ranges = locs->Array.map(GCL.Loc.toRange);
    let rangeBehavior =
      VSCode.DecorationRangeBehavior.toEnum(
        VSCode.DecorationRangeBehavior.ClosedClosed,
      );
    let rulerColor =
      VSCode.StringOr.others(
        VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
      );
    let overviewRulerLane: VSCode.OverviewRulerLane.raw =
      VSCode.OverviewRulerLane.Left->VSCode.OverviewRulerLane.toEnum;

    let backgroundColor =
      VSCode.StringOr.others(
        VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
      );
    let after =
      VSCode.ThemableDecorationAttachmentRenderOptions.t(
        ~contentText="*",
        // ~border="dotted 3px",
        // ~borderColor=backgroundColor,
        ~color=backgroundColor,
        (),
      );
    let options =
      VSCode.DecorationRenderOptions.t(
        ~overviewRulerColor=rulerColor,
        ~overviewRulerLane,
        ~after,
        // ~backgroundColor,
        // ~isWholeLine=true,
        ~rangeBehavior,
        (),
      );
    let decoration = VSCode.Window.createTextEditorDecorationType(options);
    decoration->Js.Array.push(state.decorations)->ignore;
    state.editor->VSCode.TextEditor.setDecorations(decoration, ranges);
  | _ => ()
  };

let make = editor => {
  let document = VSCode.TextEditor.document(editor);
  let filePath = VSCode.TextDocument.fileName(document);
  let state = {
    editor,
    document,
    filePath,
    viewReq: Req.make(),
    viewResChan: Chan.make(),
    decorations: [||],
    subscriptions: [||],
  };

  state;
};

// let sendRequest = (state, request) => {
//   let value = Request.encode(request);
//   // Js.log2("<<<", value);

//   state.client
//   ->LSP.LanguageClient.onReady
//   ->Promise.Js.toResult
//   ->Promise.flatMapOk(() => {
//       state.client
//       ->LSP.LanguageClient.sendRequest("guacamole", value)
//       ->Promise.Js.toResult
//     })
//   ->Promise.flatMapOk(json => {
//       // Js.log2(">>>", json);

//       let response = decodeResponse(json);
//       Promise.resolved(Ok(response));
//     });
//   // ->Promise.mapError(error => {Error.LSP(Error.fromJsError(error))});
// };

let destroy = state => {
  // state.client->LSP.LanguageClient.stop;
  // state.view->Option.forEach(View.destroy);
  state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose);
  state.decorations = [||];
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose);
  state.subscriptions = [||];
};

// view related
// let show = state => state.view->Option.forEach(View.show);
// let hide = state => state.view->Option.forEach(View.hide);
