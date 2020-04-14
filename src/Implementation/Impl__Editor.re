open Vscode;
open! Belt;

type editor = Vscode.TextEditor.t;
type context = Vscode.ExtensionContext.t;
type disposable = Vscode.Disposable.t;
type view = Vscode.WebviewPanel.t;

type range = Vscode.Range.t;
type point = Vscode.Position.t;

type fileName = string;

// let make = (editor, context) => {editor, context};

let editorType = Sig.VsCode;

let getExtensionPath = context => context->ExtensionContext.extensionPath;

let getFileName = editor =>
  Some(editor->TextEditor.document->TextDocument.fileName);

let save = editor => editor->TextEditor.document->TextDocument.save;

let toPoint =
  fun
  | GCL.Pos.Pos(_, line, column) => Position.make(line - 1, column - 1);

let fromPoint = (filepath, point) =>
  GCL.Pos.Pos(
    filepath,
    Position.line(point) + 1,
    Position.character(point) + 1,
  );

let toRange =
  fun
  | GCL.Loc.NoLoc =>
    Vscode.Range.make(Position.make(0, 0), Position.make(0, 0))
  | Loc(x, Pos(_, line, column)) =>
    Vscode.Range.make(toPoint(x), Position.make(line - 1, column));
let fromRange = (filepath, range) => {
  let start = Vscode.Range.start(range);
  let end_ = Vscode.Range.end_(range);
  GCL.Loc.Loc(
    Pos(filepath, Position.line(start) + 1, Position.character(start) + 1),
    Pos(filepath, Position.line(end_) + 1, Position.character(end_)),
  );
};

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