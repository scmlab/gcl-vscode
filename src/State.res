open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  globalStoragePath: string,
  // state
  mutable specifications: array<Response.Specification.t>,
  // garbage
  mutable subscriptions: array<VSCode.Disposable.t>,
}

let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

let display = (id, blocks) => View.send(ViewType.Request.Display(id, blocks))->Promise.map(_ => ())

let updateConnection = status => View.send(UpdateConnection(status))->Promise.map(_ => ())

let focus = state =>
  VSCode.Window.showTextDocument(state.document, ~column=VSCode.ViewColumn.Beside, ())->ignore

let sendLSPRequest = (state, kind) => {
  Connection.sendRequest(state.globalStoragePath, Request.Req(state.filePath, kind))
}

module Spec = {
  // find the hole containing the cursor
  let fromCursorPosition = state => {
    let cursor = state.editor->VSCode.TextEditor.selection->VSCode.Selection.end_
    // find the smallest hole containing the cursor, as there might be many of them
    let smallestHole = ref(None)
    state.specifications
    ->Array.keep(spec => {
      let range = SrcLoc.Loc.toVSCodeRange(spec.loc)
      VSCode.Range.contains(range, cursor)
    })
    ->Array.forEach(spec =>
      switch smallestHole.contents {
      | None => smallestHole := Some(spec)
      | Some(spec') =>
        if VSCode.Range.containsRange(SrcLoc.Loc.toVSCodeRange(spec.loc), SrcLoc.Loc.toVSCodeRange(spec'.loc)) {
          smallestHole := Some(spec)
        }
      }
    )
    smallestHole.contents
  }

  let getPayloadRange = (doc, spec: Response.Specification.t) => {
    let range = SrcLoc.Loc.toVSCodeRange(spec.loc)
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
    // find the corresponding Spec

    let spec = (state.specifications->Array.keep(spec => spec.id == i))[0]
    switch spec {
    | None => Promise.resolved()
    | Some(spec) =>
      let range = SrcLoc.Loc.toVSCodeRange(spec.loc)
      // get text inside the Spec
      let start = VSCode.Range.start(range)
      let indentedPayload = {
        let payload = getPayload(state.document, spec)
        let indentationLevel = VSCode.Position.character(start)
        let indentation = Js.String.repeat(indentationLevel, " ")
        payload->Js.Array2.joinWith("\n" ++ indentation)
      }
      // delete the whole Spec
      Editor.Text.delete(state.document, range)
      // restore the original text inside that Spec
      ->Promise.flatMap(result =>
        switch result {
        | false => Promise.resolved(false)
        | true => Editor.Text.insert(state.document, start, indentedPayload)
        }
      )
      // remove decorations
      ->Promise.map(_ => {
        spec.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
      })
    }
  }

  let redecorate = (state, specs) => {
    // dispose old decorations
    state.specifications->Array.forEach(spec =>
      spec.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
    )

    // persist new spects
    state.specifications = specs
    // apply new decorations
    state.specifications->Array.forEach(spec => {
      // devise and apply new decorations
      let decorations = {
        let range = SrcLoc.Loc.toVSCodeRange(spec.loc)
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
        let preCondText = " " ++ trim(spec.pre)
        let postCondText = " " ++ trim(spec.post)
        // see if the Spec's precondition and the post-condition look the same (i.e. the Q_Q case)
        let isQQ = preCondText == postCondText

        let highlightBackground = ranges => {
          let backgroundColor = VSCode.StringOr.others(
            VSCode.ThemeColor.make("editor.wordHighlightStrongBackground"),
          )
          let rangeBehavior = VSCode.DecorationRangeBehavior.toEnum(
            VSCode.DecorationRangeBehavior.ClosedClosed,
          )
          let options = VSCode.DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ())
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
          let rangeBehavior = VSCode.DecorationRangeBehavior.toEnum(
            VSCode.DecorationRangeBehavior.ClosedClosed,
          )
          let options = VSCode.DecorationRenderOptions.t(~after, ~rangeBehavior, ())
          let decoration = VSCode.Window.createTextEditorDecorationType(options)
          state.editor->VSCode.TextEditor.setDecorations(decoration, ranges)
          decoration
        }
        [
          overlayText(isQQ ? "" : preCondText, [startRange]),
          overlayText(postCondText, [endRange]),
          highlightBackground([startRange, endRange]),
        ]
      }

      // persist new decoraitons
      spec.decorations = decorations
    })
  }

  let updateLocations = (state, locations) => {
    state.specifications->Array.forEachWithIndex((index, spec) => {
      locations[index]->Option.forEach(loc => {
        // Js.log(SrcLoc.Loc.toString(spec.loc) ++ " ===>" ++ SrcLoc.Loc.toString(loc))
        spec.loc = loc
      })
    })
    redecorate(state, state.specifications)
  }
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Display(i, blocks) => display(i, blocks)
  | UpdateSpecs(specs) =>
    Spec.redecorate(state, specs)
    Promise.resolved()
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

let make = (globalStoragePath, editor) => {
  let document = VSCode.TextEditor.document(editor)
  let filePath = VSCode.TextDocument.fileName(document)
  let state = {
    editor: editor,
    document: document,
    filePath: filePath,
    globalStoragePath: globalStoragePath,
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
  state.specifications->Array.forEach(Response.Specification.destroy)
  state.specifications = []
}
