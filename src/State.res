open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  // state
  mutable specifications: array<Response.Specification.t>,
  mutable specificationDecorations: array<VSCode.TextEditorDecorationType.t>,
  // garbage
  mutable subscriptions: array<VSCode.Disposable.t>,
}

let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

let displayErrorMessage = msg =>
  View.send(ViewType.Request.SetErrorMessages([msg]))->Promise.map(_ => ())

let displayErrorMessages = msgs =>
  View.send(ViewType.Request.SetErrorMessages(msgs))->Promise.map(_ => ())

let display = (id, pos, props, warnings) =>
  View.send(ViewType.Request.Display(id, pos, props, warnings))->Promise.map(_ => ())

let updatePOs = pos => View.send(ViewType.Request.UpdatePOs(pos))->Promise.map(_ => ())

let updateConnection = status => View.send(UpdateConnection(status))->Promise.map(_ => ())

let focus = state =>
  VSCode.Window.showTextDocument(state.document, ~column=VSCode.ViewColumn.Beside, ())->ignore

let sendLSPRequest = (state, kind) => {
  Connection.sendRequest(Request.Req(state.filePath, kind))
}

module HandleError = {
  // let handleStructErrorToErrorMessage = (_site, error): Promise.t<unit> =>
  //   switch error {
  //   | Response.Error.StructError.MissingBound => displayErrorMessages([])
  //   | MissingAssertion => displayErrorMessages([])
  //   | ExcessBound => displayErrorMessages([])
  //   | MissingPostcondition => displayErrorMessages([])
  //   | DigHole => Promise.resolved()
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

  // let handleStructError = (site, error) =>
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
    state.specifications
    ->Array.keep(spec => {
      let range = GCL.Loc.toRange(spec.loc)
      VSCode.Range.contains(range, cursor)
    })
    ->Array.forEach(spec =>
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

  // return the payload inside the spec
  // split into lines
  let getPayload = (doc, spec): array<string> => {
    // return the text in the targeted hole
    let innerRange = getPayloadRange(doc, spec)
    let payload = VSCode.TextDocument.getText(doc, Some(innerRange))
    // split the payload into lines
    let payloadLines = payload->Js.String2.match_(%re("/[^\\r\\n]+/g"))->Option.getWithDefault([])
    // calculate the level of indentation for each lines
    let indentLevels = payloadLines->Array.map(Js.String.search(%re("/\\S|$/")))
    // find the smallest level
    let smallestLevel = ref(None)
    indentLevels->Array.forEach(lvl =>
      switch smallestLevel.contents {
      | None => smallestLevel := Some(lvl)
      | Some(n) =>
        if lvl < n {
          smallestLevel := Some(lvl)
        }
      }
    )
    let smallestLevel = smallestLevel.contents->Option.getWithDefault(0)
    // remove the indentation
    payloadLines->Array.map(Js.String.sliceToEnd(~from=smallestLevel))
  }

  let resolve = (state, i) => {
    let specs = state.specifications->Array.keep(spec => spec.id == i)
    specs[0]->Option.forEach(spec => {
      let range = GCL.Loc.toRange(spec.loc)
      let start = VSCode.Range.start(range)

      let indentedPayload = {
        let payload = getPayload(state.document, spec)
        let indentationLevel = VSCode.Position.character(start)
        let indentation = Js.String.repeat(indentationLevel, " ")
        payload->Js.Array2.joinWith("\n" ++ indentation)
      }

      // delete text
      Editor.Text.delete(state.document, range)
      ->Promise.flatMap(result =>
        switch result {
        | false => Promise.resolved(false)
        | true => Editor.Text.insert(state.document, start, indentedPayload)
        }
      )
      ->Promise.get(_ => ())
    })
    Promise.resolved()
  }

  let insert = (state, lineNo, expr) => {
    let assertion = "{ " ++ GCL.Syntax.Expr.toString(expr) ++ " }\n"
    let point = VSCode.Position.make(lineNo - 1, 0)
    // insert the assertion
    Editor.Text.insert(state.document, point, assertion)
  }

  let decorate = state => {
    // remove previous decorations
    state.specificationDecorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    // generate new decorations
    let decorations =
      state.specifications
      ->Array.map(spec => {
        let range = GCL.Loc.toRange(spec.loc)
        let startPosition = VSCode.Range.start(range)
        let endPosition = VSCode.Range.end_(range)
        // range of [!
        let startRange = VSCode.Range.make(
          startPosition,
          VSCode.Position.translate(startPosition, 0, 2),
        )
        // range of !]
        let endRange = VSCode.Range.make(VSCode.Position.translate(endPosition, 0, -2), endPosition)
        // helper function for trimming long predicates
        let trim = s =>
          if String.length(s) > 77 {
            String.sub(s, 0, 73) ++ " ..."
          } else {
            s
          }
        // text decorations
        let preCondText = " " ++ trim(GCL.Syntax.Pred.toString(spec.pre))
        let postCondText = " " ++ trim(GCL.Syntax.Pred.toString(spec.post))
        // see if the Spec's precondition and the post-condition look the same (i.e. the Q_Q case)
        let isQQ = preCondText == postCondText

        let highlightBackground = ranges => {
          let backgroundColor = VSCode.StringOr.others(
            VSCode.ThemeColor.make("editor.wordHighlightStrongBackground"),
          )
          let options = VSCode.DecorationRenderOptions.t(~backgroundColor, ())
          let decoration = VSCode.Window.createTextEditorDecorationType(options)
          state.editor->VSCode.TextEditor.setDecorations(decoration, ranges)
          decoration
        }

        let overlayText = (text, ranges) => {
          let color = VSCode.StringOr.others(VSCode.ThemeColor.make("descriptionForeground"))
          let after = VSCode.ThemableDecorationAttachmentRenderOptions.t(
            ~contentText=text,
            ~color,
            (),
          )
          let options = VSCode.DecorationRenderOptions.t(~after, ())
          let decoration = VSCode.Window.createTextEditorDecorationType(options)
          state.editor->VSCode.TextEditor.setDecorations(decoration, ranges)
          decoration
        }

        [
          overlayText(isQQ ? "" : preCondText, [startRange]),
          overlayText(postCondText, [endRange]),
          highlightBackground([startRange, endRange]),
        ]
      })
      ->Array.concatMany

    // persist new decorations
    state.specificationDecorations = decorations
  }
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Error(errors) =>
    let errorToMessage = (error: Response.Error.t) => {
      let Response.Error.Error(site, kind) = error
      switch kind {
      | Response.Error.LexicalError => [("Lexical Error", Response.Error.Site.toString(site))]
      | SyntacticError(messages) => [("Parse Error", messages->Js.String.concatMany("\n"))]
      | StructError(MissingAssertion) => [
          ("Missing Loop Invariant", "There should be a loop invariant before the DO construct"),
        ]
      // | StructError(MissingBound) => [
      //     (
      //       "Missing Bound",
      //       "There should be a Bound at the end of the assertion before the DO construct \" , bnd : ... }\"",
      //     ),
      //   ]
      // | StructError(ExcessBound) => [
      //     ("Excess Bound", "The bound annotation at this assertion is unnecessary"),
      //   ]
      | StructError(MissingPostcondition) => [
          ("Missing Postcondition", "The last statement of the program should be an assertion"),
        ]
      | StructError(DigHole) => []
      | CannotReadFile(string) => [("Server Internal Error", "Cannot read file\n" ++ string)]
      | CannotSendRequest(string) => [("Client Internal Error", "Cannot send request\n" ++ string)]
      | TypeError(NotInScope(name)) => [
          (
            "Not In Scope",
            "The identifier \"" ++
            name ++
            "\" " ++
            Response.Error.Site.toString(site) ++ " is not in scope",
          ),
        ]
      | TypeError(UnifyFailed(s, t)) => [
          (
            "Cannot unify types",
            "Cannot unify: " ++
            GCL.Syntax.Type.toString(s) ++
            "\nwith        : " ++
            GCL.Syntax.Type.toString(t),
          ),
        ]
      | TypeError(RecursiveType(var, t)) => [
          (
            "Recursive type variable",
            "Recursive type variable: " ++
            string_of_int(var) ++
            "\nin type             : " ++
            GCL.Syntax.Type.toString(t),
          ),
        ]
      | TypeError(NotFunction(t)) => [
          (
            "Not a function",
            "The type " ++ GCL.Syntax.Type.toString(t) ++ " is not a function type",
          ),
        ]
      }
    }

    let errorToSideEffects = error => {
      let Response.Error.Error(_site, kind) = error
      switch kind {
      | Response.Error.StructError(DigHole) =>
        let sites = errors->Array.keepMap(Response.Error.matchDigHole)
        switch sites[0] {
        | None => Promise.resolved()
        | Some(site) =>
          let range = Response.Error.Site.toRange(site, state.specifications, GCL.Loc.toRange)
          // replace the question mark "?" with a hole "[!  !]"
          let indent = Js.String.repeat(VSCode.Position.character(VSCode.Range.start(range)), " ")
          let holeText = "[!\n" ++ indent ++ "\n" ++ indent ++ "!]"
          let holeRange = VSCode.Range.make(
            VSCode.Range.start(range),
            VSCode.Position.translate(VSCode.Range.start(range), 0, 1),
          )

          state.document
          ->Editor.Text.replace(holeRange, holeText)
          ->Promise.map(_ => {
            // set the cursor inside the hole
            let selectionRange = VSCode.Range.make(
              VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
              VSCode.Position.translate(VSCode.Range.start(range), 1, 0),
            )
            Editor.Selection.set(state.editor, selectionRange)
          })
          ->Promise.flatMap(_ => {
            // save the editor to trigger the server
            state.document->VSCode.TextDocument.save
          })
          ->Promise.map(_ => ())
        }
      | _ => Promise.resolved()
      }
    }
    //
    let errorMessages = errors->Array.map(errorToMessage)->Array.concatMany
    errors
    ->Array.map(errorToSideEffects)
    ->Util.Promise.oneByOne
    ->Promise.flatMap(_ => displayErrorMessages(errorMessages))

  | OK(i, pos, specs, props, warnings) =>
    state.specifications = specs
    Spec.decorate(state)
    // clear error messages before display othe stuff
    displayErrorMessages([])->Promise.flatMap(() => display(i, pos, props, warnings))
  | Inspect(pos) => updatePOs(pos)
  | Substitute(id, expr) => View.send(ViewType.Request.Substitute(id, expr))->Promise.map(_ => ())
  | Resolve(i) =>
    state
    ->Spec.resolve(i)
    ->Promise.flatMap(_ => state.document->VSCode.TextDocument.save)
    ->Promise.map(_ => ())
  | ConsoleLog(s) =>
    Js.log(s)
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
  let delete: (
    Js.Dict.t<array<VSCode.TextEditorDecorationType.t>>,
    string,
  ) => unit = %raw(`function (dict, id) {delete dict[id]}`)

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
  let removeAll = () =>
    entries(dict)->Array.forEach(((key, decos)) => {
      delete(dict, key)
      decos->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    })
}

let make = (editor) => {
  let document = VSCode.TextEditor.document(editor)
  let filePath = VSCode.TextDocument.fileName(document)
  let state = {
    editor: editor,
    document: document,
    filePath: filePath,
    // state
    specifications: [],
    specificationDecorations: [],
    // garbage
    subscriptions: [],
  }

  state
}

let destroy = state => {
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.subscriptions = []
}
