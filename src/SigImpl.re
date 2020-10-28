open AgdaModeVscode.VSCode;
open! Belt;

type editor = TextEditor.t;
type context = ExtensionContext.t;
module Disposable = {
  type t = Disposable.t;
  let make = Disposable.make;
  let dispose = Disposable.dispose;
};
type view = ViewController.t;

module Point = {
  type t = Position.t;
  let make = Position.make;
  let line = Position.line;
  let column = Position.character;
  let translate = Position.translate;

  let fromPos =
    fun
    | GCL.Pos.Pos(_, line, column) => Position.make(line - 1, column - 1);

  let toPos = (point, filepath) =>
    GCL.Pos.Pos(
      filepath,
      Position.line(point) + 1,
      Position.character(point) + 1,
    );
};

module Range = {
  type t = AgdaModeVscode.VSCode.Range.t;
  let make = AgdaModeVscode.VSCode.Range.make;
  let start = AgdaModeVscode.VSCode.Range.start;
  let end_ = AgdaModeVscode.VSCode.Range.end_;
  // let make =  VSCode.Range.make;

  let fromLoc =
    fun
    | GCL.Loc.NoLoc =>
      AgdaModeVscode.VSCode.Range.make(
        Position.make(0, 0),
        Position.make(0, 0),
      )
    | Loc(x, Pos(_, line, column)) =>
      AgdaModeVscode.VSCode.Range.make(
        Point.fromPos(x),
        Position.make(line - 1, column),
      );
  let toLoc = (range, filepath) => {
    let start = AgdaModeVscode.VSCode.Range.start(range);
    let end_ = AgdaModeVscode.VSCode.Range.end_(range);
    GCL.Loc.Loc(
      Pos(
        filepath,
        Position.line(start) + 1,
        Position.character(start) + 1,
      ),
      Pos(filepath, Position.line(end_) + 1, Position.character(end_)),
    );
  };

  let contains = AgdaModeVscode.VSCode.Range.contains;
  let containsRange = AgdaModeVscode.VSCode.Range.containsRange;
};

type fileName = string;

let editorType = Sig.VsCode;

let getExtensionPath = context => context->ExtensionContext.extensionPath;

let getFileName = editor =>
  Some(editor->TextEditor.document->TextDocument.fileName);

let save = editor => editor->TextEditor.document->TextDocument.save;

let addToSubscriptions = (disposable, context) =>
  disposable->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;
// let onOpenEditor = callback => Workspace.onDidRenameFiles(event => ());
// when the editor got closed
let onDidCloseEditor = callback =>
  Workspace.onDidCloseTextDocument(textDoc =>
    textDoc->Option.forEach(textDoc =>
      callback(textDoc->TextDocument.fileName)
    )
  );

let onDidChangeFileName = callback =>
  Workspace.onDidRenameFiles(event =>
    event
    ->Option.map(AgdaModeVscode.VSCode.FileRenameEvent.files)
    ->Option.forEach(files => {
        files->Array.forEach(file =>
          callback(
            Some(file##oldUri->Uri.path),
            Some(file##newUri->Uri.path),
          )
        )
      })
  );
let onDidChangeActivation = callback => {
  let previous = ref(Window.activeTextEditor->Option.flatMap(getFileName));

  Window.onDidChangeActiveTextEditor(next => {
    let next = next->Option.flatMap(getFileName);
    if (next != previous^) {
      callback(previous^, next);
      previous := next;
    };
  });
};

let registerCommand = (name, callback) =>
  Commands.registerCommand(
    "guacamole." ++ name,
    () => {
      Js.log("guacamole." ++ name);
      Window.activeTextEditor->Option.forEach(editor => {
        editor
        ->getFileName
        ->Option.forEach(fileName => {
            let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
            if (isGCL(fileName)) {
              callback(editor);
            };
          })
      });
    },
  );

//
// Configuration
//
module Config = {
  let setGCLPath = path =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("gclPath", path, None);
  let getGCLPath = () =>
    Workspace.getConfiguration(Some("guacamole"), None)
    ->WorkspaceConfiguration.get("gclPath");
};

//
// View
//
module View = {
  include ViewController;
  // override View.make to inject editor-dependent arguments
  let make = ViewController.make(getExtensionPath);
};

//
// Decoration
//

module Decoration = {
  type t = TextEditorDecorationType.t;

  type kind =
    | Error
    | Highlight
    | Spec;

  let digHole = (editor: editor, range: AgdaModeVscode.VSCode.Range.t) => {
    let start = AgdaModeVscode.VSCode.Range.start(range);
    // add indentation to the hole
    let indent = Js.String.repeat(Position.character(start), " ");
    let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
    let holeRange =
      AgdaModeVscode.VSCode.Range.make(
        start,
        Position.translate(start, 0, 1),
      );

    let editCallback = edit => {
      edit->TextEditorEdit.replaceAtRange(holeRange, holeText);
    };
    editor->TextEditor.edit(editCallback, None)->ignore;
    // set the cursor inside the hole

    let pos = Position.translate(start, 1, 0);
    let selection = Selection.make(pos, pos);
    editor->TextEditor.setSelection(selection);
  };

  let highlightBackground = (editor: editor, kind: kind, range: Range.t) => {
    let backgroundColor =
      ThemeColor.themeColor(
        switch (kind) {
        | Error => ThemeColor.make("inputValidation.errorBackground")
        | Highlight => ThemeColor.make("editor.symbolHighlightBackground")
        | Spec => ThemeColor.make("editor.wordHighlightStrongBackground")
        },
      );
    let options = DecorationRenderOptions.t(~backgroundColor, ());
    let handle = Window.createTextEditorDecorationType(options);
    editor->TextEditor.setDecorations(handle, [|range|]);
    [|handle|];
  };

  let overlayText = (editor: editor, kind: kind, text: string, range: Range.t) => {
    let after =
      ThemableDecorationAttachmentRenderOptions.t(
        ~contentText=text,
        ~color=
          ThemeColor.themeColor(
            switch (kind) {
            | Error => ThemeColor.make("errorForeground")
            | Highlight => ThemeColor.make("descriptionForeground")
            | Spec => ThemeColor.make("descriptionForeground")
            },
          ),
        (),
      );

    let options = DecorationRenderOptions.t(~after, ());
    let handle = Window.createTextEditorDecorationType(options);
    editor->TextEditor.setDecorations(handle, [|range|]);
    [|handle|];
  };

  let destroy = TextEditorDecorationType.dispose;
};

let getCursorPosition = editor => editor->TextEditor.selection->Selection.end_;

let rangeForLine = (editor, line) =>
  editor->TextEditor.document->TextDocument.lineAt(line)->TextLine.range;

let getText = (editor, range) =>
  editor->TextEditor.document->TextDocument.getText(Some(range));
let selectText = (editor, range) => {
  let start = Range.start(range);
  let end_ = Range.end_(range);
  let selection = Selection.make(start, end_);
  editor->TextEditor.setSelection(selection);
};
let insertText = (editor, point, text) => {
  let editCallback = edit => {
    edit->TextEditorEdit.insert(point, text);
  };
  editor->TextEditor.edit(editCallback, None);
};
let deleteText = (editor, range) => {
  let editCallback = edit => {
    edit->TextEditorEdit.delete(range);
  };
  editor->TextEditor.edit(editCallback, None);
};