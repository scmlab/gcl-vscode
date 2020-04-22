open Vscode;
open! Belt;

type editor = Vscode.TextEditor.t;
type context = Vscode.ExtensionContext.t;
module Disposable = {
  type t = Disposable.t;
  let make = Disposable.make;
  let dispose = Disposable.dispose;
};
type view = Impl__View.t;

module Point = {
  type t = Position.t;
  let make = Position.make;
  let line = Position.line;
  let column = Position.character;
  let translate = Position.translate;

  let fromPos =
    fun
    | GCL.Pos.Pos(_, line, column) => Position.make(line - 1, column - 1);

  let toPos = (filepath, point) =>
    GCL.Pos.Pos(
      filepath,
      Position.line(point) + 1,
      Position.character(point) + 1,
    );
};

module Range = {
  type t = Vscode.Range.t;
  let make = Vscode.Range.make;
  let start = Vscode.Range.start;
  let end_ = Vscode.Range.end_;
  // let make =  Vscode.Range.make;

  let fromLoc =
    fun
    | GCL.Loc.NoLoc =>
      Vscode.Range.make(Position.make(0, 0), Position.make(0, 0))
    | Loc(x, Pos(_, line, column)) =>
      Vscode.Range.make(Point.fromPos(x), Position.make(line - 1, column));
  let toLoc = (filepath, range) => {
    let start = Vscode.Range.start(range);
    let end_ = Vscode.Range.end_(range);
    GCL.Loc.Loc(
      Pos(
        filepath,
        Position.line(start) + 1,
        Position.character(start) + 1,
      ),
      Pos(filepath, Position.line(end_) + 1, Position.character(end_)),
    );
  };
};

type decoration = Vscode.TextEditorDecorationType.t;
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
    ->Option.map(Vscode.FileRenameEvent.files)
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
  Commands.registerCommand("extension." ++ name, () => {
    Window.activeTextEditor->Option.forEach(editor => {
      editor
      ->getFileName
      ->Option.forEach(fileName => {
          let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);
          if (isGCL(fileName)) {
            callback(editor);
          };
        })
    })
  });

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
  include Impl__View;
  // override View.make to inject editor-dependent arguments
  let make = Impl__View.make(getExtensionPath);
};

//
// Decoration
//

module Decoration = {
  type kind =
    | Error
    | Spec;

  let digHole = (editor: editor, range: Vscode.Range.t) => {
    let start = Vscode.Range.start(range);
    // add indentation to the hole
    let indent = Js.String.repeat(Vscode.Position.character(start), " ");
    let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
    let holeRange =
      Vscode.Range.make(start, Vscode.Position.translate(start, 0, 1));

    let editCallback = edit => {
      edit->TextEditorEdit.replaceAtRange(holeRange, holeText);
    };
    editor->TextEditor.edit(editCallback, None)->ignore;
    // set the cursor inside the hole

    let pos = Vscode.Position.translate(start, 1, 0);
    let selection = Selection.make(pos, pos);
    editor->TextEditor.setSelection(selection);
  };

  let markBackground = (editor: editor, kind: kind, range: Range.t) => {
    let options =
      DecorationRenderOptions.t(
        ~backgroundColor=
          ThemeColor.themeColor(
            ThemeColor.make("inputValidation.errorBackground"),
          ),
        (),
      );
    let handle = Window.createTextEditorDecorationType(options);
    editor->TextEditor.setDecorations(handle, [|range|]);
    [|handle|];
  };

  let destroy = TextEditorDecorationType.dispose;
};