open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  viewReq: Req.t<ViewType.Request.t, bool>,
  viewResChan: Chan.t<ViewType.Response.t>,
  // mutable decorations: array<VSCode.TextEditorDecorationType.t>,
  mutable subscriptions: array<VSCode.Disposable.t>,
}

let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

let display = (state, header, body) =>
  state.viewReq->Req.send(ViewType.Request.Display(header, body))->Promise.map(_ => ())

let focus = state =>
  VSCode.Window.showTextDocument(state.document, ~column=VSCode.ViewColumn.Beside, ())->ignore

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
  let Response.Error.Error(_site, kind) = error
  switch kind {
  // | Response.Error.LexicalError => Promise.resolved()
  // // state->display(Error("Lexical Error"), Plain(Response.Error.Site.toString(site)))
  // | SyntacticError(_messages) => Promise.resolved()
  // // state->display(Error("Parse Error"), Plain(messages->Js.String.concatMany("\n")))
  // | StructError(_error) => Promise.resolved()
  // // state->handleStructError(error)
  // | TypeError(error) => state->handleTypeError(error)
  | CannotReadFile(string) =>
    state->display(Error("Server Internal Error"), Plain("Cannot read file\n" ++ string))
  | CannotSendRequest(string) =>
    state->display(Error("Client Internal Error"), Plain("Cannot send request\n" ++ string))
  | NotLoaded => state->display(Error("Client Internal Error"), Plain("Client not loaded yet"))
  | _ => Promise.resolved()
  }
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Error(errors) =>
    errors->Array.map(handleError(state))->Util.Promise.oneByOne->Promise.map(_ => ())
  | OK(i, pos, _, props) =>
    state->display(Plain("Proof Obligations"), ProofObligations(i, pos, props))
  | Decorate(_locs) =>
    // destroy old decorations
    // state.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    // state.decorations = []

    // let ranges = locs->Array.map(GCL.Loc.toRange)
    // let rangeBehavior = VSCode.DecorationRangeBehavior.toEnum(
    //   VSCode.DecorationRangeBehavior.ClosedClosed,
    // )
    // let rulerColor = VSCode.StringOr.others(
    //   VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
    // )
    // let overviewRulerLane: VSCode.OverviewRulerLane.raw =
    //   VSCode.OverviewRulerLane.Left->VSCode.OverviewRulerLane.toEnum

    // let backgroundColor = VSCode.StringOr.others(
    //   VSCode.ThemeColor.make("editorOverviewRuler.warningForeground"),
    // )
    // let after = VSCode.ThemableDecorationAttachmentRenderOptions.t(
    //   ~contentText="*",
    //   ~color=backgroundColor,
    //   (),
    // )
    // let options = VSCode.DecorationRenderOptions.t(
    //   ~overviewRulerColor=rulerColor,
    //   ~overviewRulerLane,
    //   ~after,
    //   // ~backgroundColor,
    //   // ~isWholeLine=true,
    //   ~rangeBehavior,
    //   (),
    // )
    // let decoration = VSCode.Window.createTextEditorDecorationType(options)
    // decoration->Js.Array.push(state.decorations)->ignore
    // state.editor->VSCode.TextEditor.setDecorations(decoration, ranges)
    Promise.resolved()
  | _ => Promise.resolved()
  }

module type Decoration = {
  let addBackground: (t, string, VSCode.Range.t) => unit
  let remove: string => unit
  let removeAll: unit => unit
}
module Decoration = {
  open Js.Dict
  // a dictionary of decorations for <Link>
  let dict: Js.Dict.t<array<VSCode.TextEditorDecorationType.t>> = empty()

  // deletes a entry (does not destruct the value)
  let delete: (Js.Dict.t<array<VSCode.TextEditorDecorationType.t>>, string) => unit = %raw(
    `function (dict, id) {delete dict[id]}`
  )

  let addBackground = (state, key, range, color) => {
    // "editor.symbolHighlightBackground"
    let backgroundColor = VSCode.StringOr.others(VSCode.ThemeColor.make(color))
    let options = VSCode.DecorationRenderOptions.t(~backgroundColor, ())
    let decoration = VSCode.Window.createTextEditorDecorationType(options)
    state.editor->VSCode.TextEditor.setDecorations(decoration, [range])
    Js.Dict.set(dict, key, [decoration])
  }

  let remove = key => {
    Js.Dict.get(dict, key)->Option.forEach(decos =>
      decos->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    )
    delete(dict, key)
  }

  // deletes all entries
  let removeAll = () => entries(dict)->Array.forEach(((key, decos)) => {
      delete(dict, key)
      decos->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    })
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
    subscriptions: [],
  }

  state.viewResChan->Chan.on(response =>
    switch response {
    | ViewType.Response.Link(MouseOver(loc)) =>
      let key = GCL.Loc.toString(loc)
      let range = GCL.Loc.toRange(loc)
      Decoration.addBackground(state, key, range, "statusBar.debuggingBackground")
    | Link(MouseOut(loc)) =>
      let key = GCL.Loc.toString(loc)
      Decoration.remove(key)
    | Link(MouseClick(loc)) =>
      let key = GCL.Loc.toString(loc)
      let range = GCL.Loc.toRange(loc)
      // focus on the editor
      focus(state)
      // select the source on the editor
      let selection = VSCode.Selection.make(VSCode.Range.start(range), VSCode.Range.end_(range))
      state.editor->VSCode.TextEditor.setSelection(selection)
      Decoration.remove(key)
    | others => Js.log(others)
    }
  )->VSCode.Disposable.make->Js.Array.push(state.subscriptions)->ignore

  state
}

let destroy = state => {
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.subscriptions = []
}
