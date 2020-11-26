open VSCode

module Selection = {
  let set = (editor, range) => {
    let selection = Selection.make(Range.start(range), Range.end_(range))
    editor->TextEditor.setSelection(selection)
  }
  let setMany = (editor, ranges) => {
    let selections =
      ranges->Belt.Array.map(range => Selection.make(Range.start(range), Range.end_(range)))
    editor->TextEditor.setSelections(selections)
  }
  let get = editor => {
    let selection = editor->TextEditor.selection
    Range.make(Selection.start(selection), Selection.end_(selection))
  }
  let getMany = editor => {
    editor
    ->TextEditor.selections
    ->Belt.Array.map(selection => Range.make(Selection.start(selection), Selection.end_(selection)))
  }
}
module Text = {
  let get = (document, range) => document->TextDocument.getText(Some(range))
  let getAll = document => document->TextDocument.getText(None)

  let replace = (document, range, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.replace(document->TextDocument.uri, range, text, None)
    Workspace.applyEdit(workspaceEdit)
  }
  let batchReplace = (document, replacements) => {
    let workspaceEdit = WorkspaceEdit.make()
    replacements->Belt.Array.forEach(((range, text)) =>
      workspaceEdit->WorkspaceEdit.replace(document->TextDocument.uri, range, text, None)
    )
    Workspace.applyEdit(workspaceEdit)
  }

  let insert = (document, point, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.insert(document->TextDocument.uri, point, text, None)
    Workspace.applyEdit(workspaceEdit)
  }
  let batchInsert = (document, points, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    let textEdits = points->Belt.Array.map(point => TextEdit.insert(point, text))
    workspaceEdit->WorkspaceEdit.set(document->TextDocument.uri, textEdits)
    Workspace.applyEdit(workspaceEdit)
  }
  let delete = (document, range) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.delete(document->TextDocument.uri, range, None)
    Workspace.applyEdit(workspaceEdit)
  }
}
