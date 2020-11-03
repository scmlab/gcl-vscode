open VSCode;
open Belt;

module Decoration = {
  type t = TextEditorDecorationType.t;
  type backgroundStyle = string;
  type foregroundStyle = string;
  type color = string;

  let decorate =
      (editor: TextEditor.t, decoration: t, ranges: array(VSCode.Range.t)) => {
    editor->TextEditor.setDecorations(decoration, ranges);
  };

  //
  let highlightBackgroundPrim =
      (
        editor: TextEditor.t,
        backgroundColor: ThemeColor.stringOrThemeColor,
        ranges: array(VSCode.Range.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options =
      DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let highlightBackground =
      (
        editor: TextEditor.t,
        style: backgroundStyle,
        ranges: array(VSCode.Range.t),
      ) =>
    highlightBackgroundPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      ranges,
    );

  let highlightBackgroundWithColor =
      (editor: TextEditor.t, color: color, ranges: array(VSCode.Range.t)) =>
    highlightBackgroundPrim(editor, ThemeColor.string(color), ranges);

  let decorateTextPrim =
      (
        editor: TextEditor.t,
        color: ThemeColor.stringOrThemeColor,
        ranges: array(VSCode.Range.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options = DecorationRenderOptions.t(~color, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let decorateText =
      (
        editor: TextEditor.t,
        style: backgroundStyle,
        ranges: array(VSCode.Range.t),
      ) =>
    decorateTextPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      ranges,
    );

  let decorateTextWithColor =
      (editor: TextEditor.t, color: color, ranges: array(VSCode.Range.t)) =>
    decorateTextPrim(editor, ThemeColor.string(color), ranges);

  let overlayTextPrim =
      (
        editor: TextEditor.t,
        color: ThemeColor.stringOrThemeColor,
        text: string,
        range: VSCode.Range.t,
      ) => {
    let after =
      ThemableDecorationAttachmentRenderOptions.t(
        ~contentText=text,
        ~color,
        (),
      );

    let options = DecorationRenderOptions.t(~after, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  let overlayText =
      (
        editor: TextEditor.t,
        style: foregroundStyle,
        text: string,
        range: VSCode.Range.t,
      ) =>
    overlayTextPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      text,
      range,
    );

  let overlayTextWithColor =
      (
        editor: TextEditor.t,
        color: color,
        text: string,
        range: VSCode.Range.t,
      ) =>
    overlayTextPrim(editor, ThemeColor.string(color), text, range);

  let underlineText = (editor: TextEditor.t, range: VSCode.Range.t) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.OpenOpen);
    let textDecoration = "underline dotted";
    let options =
      DecorationRenderOptions.t(~rangeBehavior, ~textDecoration, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  let destroy = TextEditorDecorationType.dispose;
};

module Text = {
  let getAll = document => document->TextDocument.getText(None);
  let get = (document, range) =>
    document->TextDocument.getText(Some(range));
  let insert = (document, point, text) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.insert(
      document->TextDocument.uri,
      point,
      text,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };

  let select = (editor, range) => {
    let start = VSCode.Range.start(range);
    let end_ = VSCode.Range.end_(range);
    let selection = Selection.make(start, end_);
    editor->TextEditor.setSelection(selection);
  };

  let replace = (document, range, text) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.replace(
      document->TextDocument.uri,
      range,
      text,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };
  let delete = (document, range) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.delete(
      document->TextDocument.uri,
      range,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };
};
