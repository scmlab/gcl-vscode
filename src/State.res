open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  // communications
  viewSendRequest: ViewType.Request.t => Promise.t<bool>,
  lspSendRequest: Request.Kind.t => Promise.t<Response.t>,
  // state
  mutable specifications: array<Response.Specification.t>,
  // garbage
  mutable subscriptions: array<VSCode.Disposable.t>,
}

let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

let display = (state, id, pos, props) =>
  state.viewSendRequest(ViewType.Request.Display(id, pos, props))->Promise.map(_ => ())

let focus = state =>
  VSCode.Window.showTextDocument(state.document, ~column=VSCode.ViewColumn.Beside, ())->ignore

module HandleError = {
  // let handleStructError = (state: t, site, error) =>
  //   switch error {
  //   | Response.Error.StructError.MissingBound =>
  //     state->display(
  //       Error("Bound Missing"),
  //       Plain("Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""),
  //     )
  //   | MissingAssertion =>
  //     state->display(
  //       Error("Assertion Missing"),
  //       Plain("Assertion before the DO construct is missing"),
  //     )
  //   | ExcessBound =>
  //     state->display(Error("Excess Bound"), Plain("Unnecessary bound annotation at this assertion"))
  //   | MissingPostcondition =>
  //     state->display(
  //       Error("Postcondition Missing"),
  //       Plain("The last statement of the program should be an assertion"),
  //     )
  //   | DigHole =>
  //     let range = Response.Error.Site.toRange(site, state.specifications, GCL.Loc.toRange)
  //     // replace the question mark "?" with a hole "{!  !}"
  //     let indent = Js.String.repeat(VSCode.Position.character(VSCode.Range.start(range)), " ")
  //     let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}"
  //     let holeRange = VSCode.Range.make(
  //       VSCode.Range.start(range),
  //       VSCode.Position.translate(VSCode.Range.start(range), 0, 1),
  //     )

  //     state.document->Editor.Text.replace(holeRange, holeText)->Promise.map(_ => {
  //       // set the cursor inside the hole
  //       let selectionRange = VSCode.Range.make(
  //         VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
  //         VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
  //       )
  //       Editor.Selection.set(state.editor, selectionRange)
  //     })->Promise.flatMap(_ => {
  //       // save the editor to trigger the server
  //       state.document->VSCode.TextDocument.save
  //     })->Promise.map(_ => ())
  //   }

  // let handleTypeError = (state: t, error) =>
  //   switch error {
  //   | Response.Error.TypeError.NotInScope(name) =>
  //     state->display(Error("Type Error"), Plain("The definition " ++ name ++ " is not in scope"))
  //   | UnifyFailed(s, t) =>
  //     state->display(
  //       Error("Type Error"),
  //       Plain(
  //         "Cannot unify: " ++
  //         GCL.Syntax.Type.toString(s) ++
  //         "\nwith        : " ++
  //         GCL.Syntax.Type.toString(t),
  //       ),
  //     )
  //   | RecursiveType(var, t) =>
  //     state->display(
  //       Error("Type Error"),
  //       Plain(
  //         "Recursive type variable: " ++
  //         GCL.Syntax.Type.toString(GCL.Syntax.Type.Var(var)) ++
  //         "\n" ++
  //         "in type             : " ++
  //         GCL.Syntax.Type.toString(t),
  //       ),
  //     )
  //   | NotFunction(t) =>
  //     state->display(
  //       Error("Type Error"),
  //       Plain("The type " ++ GCL.Syntax.Type.toString(t) ++ " is not a function type"),
  //     )
  //   }

  // let handleError = (state: t, error: Response.Error.t) => {
  //   let Response.Error.Error(site, kind) = error
  //   switch kind {
  //   // | Response.Error.LexicalError => Promise.resolved()
  //   // // state->display(Error("Lexical Error"), Plain(Response.Error.Site.toString(site)))
  //   // | SyntacticError(_messages) => Promise.resolved()
  //   // // state->display(Error("Parse Error"), Plain(messages->Js.String.concatMany("\n")))
  //   | StructError(error) => state->handleStructError(site, error)
  //   // | TypeError(error) => state->handleTypeError(error)
  //   | CannotReadFile(string) =>
  //     state->display(Error("Server Internal Error"), Plain("Cannot read file\n" ++ string))
  //   | CannotSendRequest(string) =>
  //     state->display(Error("Client Internal Error"), Plain("Cannot send request\n" ++ string))
  //   | NotLoaded => state->display(Error("Client Internal Error"), Plain("Client not loaded yet"))
  //   | _ => Promise.resolved()
  //   }
  // }

}
module Spec = {
  // find the hole containing the cursor
  let fromCursorPosition = state => {
    let cursor = state.editor->VSCode.TextEditor.selection->VSCode.Selection.end_
    // find the smallest hole containing the cursor, as there might be many of them
    let smallestHole = ref(None)
    state.specifications->Array.keep(spec => {
      let range = GCL.Loc.toRange(spec.loc)
      VSCode.Range.contains(range, cursor)
    })->Array.forEach(spec =>
      switch smallestHole.contents {
      | None => smallestHole := Some(spec)
      | Some(spec') =>
        if VSCode.Range.containsRange(GCL.Loc.toRange(spec.loc), GCL.Loc.toRange(spec'.loc)) {
          smallestHole := Some(spec)
        }
      }
    )
    smallestHole.contents
  }

  let getPayloadRange = (doc, spec: Response.Specification.t) => {
    let range = GCL.Loc.toRange(spec.loc)
    let startingLine = VSCode.Position.line(VSCode.Range.start(range)) + 1
    let endingLine = VSCode.Position.line(VSCode.Range.end_(range)) - 1

    let start =
      VSCode.TextDocument.lineAt(doc, startingLine)->VSCode.TextLine.range->VSCode.Range.start
    let end_ = VSCode.TextDocument.lineAt(doc, endingLine)->VSCode.TextLine.range->VSCode.Range.end_
    VSCode.Range.make(start, end_)
  }
  let getPayload = (doc, spec) => {
    // return the text in the targeted hole
    let innerRange = getPayloadRange(doc, spec)
    VSCode.TextDocument.getText(doc, Some(innerRange))
  }

  let resolve = (state, i) => {
    let specs = state.specifications->Array.keep(spec => spec.id == i)
    specs[0]->Option.forEach(spec => {
      let payload = getPayload(state.document, spec)
      let range = GCL.Loc.toRange(spec.loc)
      let start = VSCode.Range.start(range)
      // delete text
      Editor.Text.delete(state.document, range)->Promise.flatMap(result =>
        switch result {
        | false => Promise.resolved(false)
        | true => Editor.Text.insert(state.document, start, Js.String.trim(payload))
        }
      )->Promise.get(_ => ())
    })
    Promise.resolved()
  }

  let insert = (state, lineNo, expr) => {
    let assertion = "{ " ++ GCL.Syntax.Expr.toString(expr) ++ " }\n"
    let point = VSCode.Position.make(lineNo - 1, 0)
    // insert the assertion
    Editor.Text.insert(state.document, point, assertion)
  }
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Error(errors) =>
    let sites = errors->Array.keepMap(Response.Error.matchDigHole)
    switch sites[0] {
    | None => Promise.resolved()
    | Some(site) =>
      let range = Response.Error.Site.toRange(site, state.specifications, GCL.Loc.toRange)
      // replace the question mark "?" with a hole "{!  !}"
      let indent = Js.String.repeat(VSCode.Position.character(VSCode.Range.start(range)), " ")
      let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}"
      let holeRange = VSCode.Range.make(
        VSCode.Range.start(range),
        VSCode.Position.translate(VSCode.Range.start(range), 0, 1),
      )

      state.document->Editor.Text.replace(holeRange, holeText)->Promise.map(_ => {
        // set the cursor inside the hole
        let selectionRange = VSCode.Range.make(
          VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
          VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
        )
        Editor.Selection.set(state.editor, selectionRange)
      })->Promise.flatMap(_ => {
        // save the editor to trigger the server
        state.document->VSCode.TextDocument.save
      })->Promise.map(_ => ())
    }
  | OK(i, pos, specs, props) =>
    state.specifications = specs
    state->display(i, pos, props)
  | Substitute(id, expr) =>
    state.viewSendRequest(ViewType.Request.Substitute(id, expr))->Promise.map(_ => ())
  | Resolve(i) =>
    state
    ->Spec.resolve(i)
    ->Promise.flatMap(_ => state.document->VSCode.TextDocument.save)
    ->Promise.map(_ => ())
  | ConsoleLog(s) =>
    Js.log(s)
    Promise.resolved()
  }

let handleResponseWithState = (state, response) =>
  switch response {
  | Response.Res(_filePath, kinds) =>
    kinds->Array.map(handleResponseKind(state))->Util.Promise.oneByOne->Promise.map(_ => ())
  | CannotSendRequest(message) =>
    Js.Console.error("Client Internal Error\nCannot send request to the server\n" ++ message)
    Promise.resolved()
  | CannotDecodeRequest(message) =>
    Js.Console.error("Server Internal Error\nCannot decode request from the client\n" ++ message)
    Promise.resolved()
  | CannotDecodeResponse(message, json) =>
    Js.Console.error2(
      "Client Internal Error\nCannot decode response from the server\n" ++ message,
      json,
    )
    Promise.resolved()
  }

module type Decoration = {
  let addBackground: (t, string, VSCode.Range.t, string) => unit
  let remove: string => unit
  let removeAll: unit => unit
}
module Decoration: Decoration = {
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

let make = (editor, viewSendRequest, lspSendRequest) => {
  let document = VSCode.TextEditor.document(editor)
  let filePath = VSCode.TextDocument.fileName(document)
  let state = {
    editor: editor,
    document: document,
    filePath: filePath,
    // communitations
    viewSendRequest: viewSendRequest,
    // viewOnResponse: viewOnResponse,
    lspSendRequest: lspSendRequest,
    // state
    specifications: [],
    // garbage
    subscriptions: [],
  }

  state
}

let destroy = state => {
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.subscriptions = []
}
