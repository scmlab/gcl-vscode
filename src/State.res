open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  mutable filePath: string,
  globalStoragePath: string,
  // decorations for Specs & POs
  mutable specifications: array<Response.Specification.t>,
  mutable proofObligations: array<Response.ProofObligation.t>,
  // garbage collection
  mutable subscriptions: array<VSCode.Disposable.t>
}

// helper function for registering new garbage
let subscribe = (disposable, state) => disposable->Js.Array.push(state.subscriptions)->ignore

// displays something at the panel
let display = (id, sections) =>
  View.send(ViewType.Request.Display(id, sections))->Promise.map(_ => ())

// displays Error at the panel
let displayError = (header, message) =>
  display(
    0,
    [
      {
        Element.Section.deco: Red,
        blocks: [
          Element.Block.Header(header, None),
          Element.Block.Paragraph(Element.Inlines.string(message)),
        ],
      },
    ],
  )

// sets focus on the editor
let focus = state =>
  VSCode.Window.showTextDocument(state.document, ~column=VSCode.ViewColumn.Beside, ())->ignore

// updates the connection status you see on the top-right corner of the panel
let updateConnectionStatus = status =>
  View.send(UpdateConnectionStatus(status))->Promise.map(_ => ())

// callback that uses `updateConnectionStatus` to update the progress of download
let onDownload = event => {
  open LanguageServerMule.Source.GitHub.Download.Event
  let message = switch event {
  | Start => "Downloading ..."
  | Progress(accum, total) =>
    // if the file is larger than 10MB than we use MB as the unit
    total > 10485760
      ? "Downloading ( " ++
        string_of_int(accum / 1048576) ++
        " MB / " ++
        string_of_int(total / 1048576) ++ " MB )"
      : "Downloading ( " ++
        string_of_int(accum / 1024) ++
        " KB / " ++
        string_of_int(total / 1024) ++ " MB )"
  | Finish => "Downloaded"
  }
  updateConnectionStatus(message)->ignore
}

// performs substitution on some expression in the panel
// each redex in the panel comes with a unique ID
let substitute = (id, expr) =>
  View.send(ViewType.Request.Substitute(id, expr))->Promise.map(_ => ())

// sends request to the backend!
let sendLSPRequest = (state, kind) => {
  Connection.sendRequest(state.globalStoragePath, onDownload, Request.Req(state.filePath, kind))
}

// decorates Specs on the editor (and updates State.t)
let decorateAndUpdateSpecs = (state, specs) => {
  // dispose old decorations
  state.specifications->Array.forEach(spec =>
    spec.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
  )

  // persist new specs
  state.specifications = specs
  // apply new decorations
  state.specifications->Array.forEach(spec => {
    // devise and apply new decorations
    let decorations = {
      let range = SrcLoc.Range.toVSCodeRange(spec.range)
      let startPosition = VSCode.Range.start(range)
      let endPosition = VSCode.Range.end_(range)

      let singleLine = VSCode.Position.line(startPosition) == VSCode.Position.line(endPosition)

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

      // only overlay texts when the Spec spans multiple lines
      if singleLine {
        [highlightBackground([startRange, endRange])]
      } else {
        [
          overlayText(isQQ ? "" : preCondText, [startRange]),
          overlayText(postCondText, [endRange]),
          highlightBackground([startRange, endRange]),
        ]
      }
    }

    // persist new decoraitons
    spec.decorations = decorations
  })
}

// decorates POs on the editor (and updates State.t)
let decorateAndUpdatePOs = (state, pos) => {
  // dispose old decorations
  state.proofObligations->Array.forEach(po =>
    po.decorations->Array.forEach(VSCode.TextEditorDecorationType.dispose)
  )

  // persist new proof obligations
  state.proofObligations = pos
  // apply new decorations
  state.proofObligations->Array.forEach(po => {
    // devise and apply new decorations
    let decorations = {
      let range = SrcLoc.Range.toVSCodeRange(po.range)

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

      [highlightBackground([range])]
    }

    // persist new decoraitons
    po.decorations = decorations
  })
}

let handleResponseKind = (state: t, kind) =>
  switch kind {
  | Response.Kind.Display(i, sections) => display(i, sections)
  | UpdateSpecs(specs) =>
    decorateAndUpdateSpecs(state, specs)
    Promise.resolved()
  | MarkPOs(pos) =>
    decorateAndUpdatePOs(state, pos)
    Promise.resolved()
  | Substitute(i, result) =>
    // we need to relay this response for substitution to the view
    substitute(i, result)
  | ConsoleLog(s) =>
    Js.log(s)
    Promise.resolved()
  }

// for the mouse hover effects (orange background)
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

  let addBackground = (state, key, range, color) =>
    switch Js.Dict.get(dict, key) {
    | Some(_) => () // already exists
    | None =>
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
  {
    editor: editor,
    document: document,
    filePath: VSCode.TextDocument.fileName(document),
    globalStoragePath: globalStoragePath,
    // state
    specifications: [],
    proofObligations: [],
    // garbage
    subscriptions: [],
  }
}

let destroy = state => {
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.subscriptions = []
  state.specifications->Array.forEach(Response.Specification.destroy)
  state.specifications = []
  state.proofObligations->Array.forEach(Response.ProofObligation.destroy)
  state.proofObligations = []
}
