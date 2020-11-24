open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  viewReq: Req.t<ViewType.Request.t, bool>,
  viewResChan: Chan.t<ViewType.Response.t>,
  mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  mutable subscriptions: array<VSCode.Disposable.t>,
}

let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

let display = (state, header, body) =>
  state.viewReq->Req.send(ViewType.Request.Display(header, body))->Promise.map(_ => ())

let handleStructError = (state: t, error) =>
  switch error {
  | Response.Error.StructError.MissingBound =>
    state->display(
      Error("Bound Missing"),
      Plain("Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""),
    )
  | MissingAssertion =>
    state->display(
      Error("Assertion Missing"),
      Plain("Assertion before the DO construct is missing"),
    )
  | ExcessBound =>
    state->display(Error("Excess Bound"), Plain("Unnecessary bound annotation at this assertion"))
  | MissingPostcondition =>
    state->display(
      Error("Postcondition Missing"),
      Plain("The last statement of the program should be an assertion"),
    )
  | DigHole => Promise.resolved()
  }

let handleTypeError = (state: t, error) =>
  switch error {
  | Response.Error.TypeError.NotInScope(name) =>
    state->display(Error("Type Error"), Plain("The definition " ++ name ++ " is not in scope"))
  | UnifyFailed(s, t) =>
    state->display(
      Error("Type Error"),
      Plain(
        "Cannot unify: " ++
        GCL.Syntax.Type.toString(s) ++
        "\nwith        : " ++
        GCL.Syntax.Type.toString(t),
      ),
    )
  | RecursiveType(var, t) =>
    state->display(
      Error("Type Error"),
      Plain(
        "Recursive type variable: " ++
        GCL.Syntax.Type.toString(GCL.Syntax.Type.Var(var)) ++
        "\n" ++
        "in type             : " ++
        GCL.Syntax.Type.toString(t),
      ),
    )
  | NotFunction(t) =>
    state->display(
      Error("Type Error"),
      Plain("The type " ++ GCL.Syntax.Type.toString(t) ++ " is not a function type"),
    )
  }

let handleError = (state: t, error: Response.Error.t) => {
  let Response.Error.Error(site, kind) = error
  switch kind {
  | Response.Error.LexicalError =>
    state->display(Error("Lexical Error"), Plain(Response.Error.Site.toString(site)))
  | SyntacticError(messages) =>
    state->display(Error("Parse Error"), Plain(messages->Js.String.concatMany("\n")))
  | StructError(error) => state->handleStructError(error)
  | TypeError(error) => state->handleTypeError(error)
  | CannotReadFile(string) =>
    state->display(Error("Server Internal Error"), Plain("Cannot read file\n" ++ string))
  | CannotSendRequest(string) =>
    state->display(Error("Client Internal Error"), Plain("Cannot send request\n" ++ string))
  | NotLoaded => state->display(Error("Client Internal Error"), Plain("Client not loaded yet"))
  }
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Error(errors) =>
    errors->Array.map(handleError(state))->Util.Promise.oneByOne->Promise.map(_ => ())
  | OK(i, pos, _, props) =>
    state->display(Plain("Proof Obligations"), ProofObligations(i, pos, props))
  | Decorate(locs) =>
    // destroy old decorations
    state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    state.decorations = []

    let ranges = locs->Array.map(GCL.Loc.toRange)
    let rangeBehavior = VSCode.DecorationRangeBehavior.toEnum(
      VSCode.DecorationRangeBehavior.ClosedClosed,
    )
    let rulerColor = VSCode.StringOr.others(
      VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
    )
    let overviewRulerLane: VSCode.OverviewRulerLane.raw =
      VSCode.OverviewRulerLane.Left->VSCode.OverviewRulerLane.toEnum

    let backgroundColor = VSCode.StringOr.others(
      VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
    )
    let after = VSCode.ThemableDecorationAttachmentRenderOptions.t(
      ~contentText="*",
      // ~border="dotted 3px",
      // ~borderColor=backgroundColor,
      ~color=backgroundColor,
      (),
    )
    let options = VSCode.DecorationRenderOptions.t(
      ~overviewRulerColor=rulerColor,
      ~overviewRulerLane,
      ~after,
      // ~backgroundColor,
      // ~isWholeLine=true,
      ~rangeBehavior,
      (),
    )
    let decoration = VSCode.Window.createTextEditorDecorationType(options)
    decoration->Js.Array.push(state.decorations)->ignore
    state.editor->VSCode.TextEditor.setDecorations(decoration, ranges)
    Promise.resolved()
  | _ => Promise.resolved()
  }

let make = editor => {
  let document = VSCode.TextEditor.document(editor)
  let filePath = VSCode.TextDocument.fileName(document)
  let state = {
    editor: editor,
    document: document,
    filePath: filePath,
    viewReq: Req.make(),
    viewResChan: Chan.make(),
    decorations: [],
    subscriptions: [],
  }

  state
}

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
  state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
  state.decorations = []
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.subscriptions = []
}

// view related
// let show = state => state.view->Option.forEach(View.show);
// let hide = state => state.view->Option.forEach(View.hide);
