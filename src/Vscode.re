// https://code.visualstudio.com/api/references/vscode-api#ThemeColor
module ThemeColor = {
  type t;
  // constructors
  [@bs.module "vscode"] [@bs.new] external make: string => t = "ThemeColor";

  module StringOrThemeColor: {
    type themeColor = t;
    type t;
    type case =
      | String(string)
      | ThemeColor(themeColor);
    let themeColor: themeColor => t;
    let string: string => t;
    let classify: t => case;
  } = {
    type themeColor = t;
    [@unboxed]
    type t =
      | Any('a): t;
    type case =
      | String(string)
      | ThemeColor(themeColor);
    let themeColor = (v: themeColor) => Any(v);
    let string = (v: string) => Any(v);
    let classify = (Any(v): t): case =>
      if (Js.typeof(v) == "string") {
        String(Obj.magic(v): string);
      } else {
        ThemeColor(Obj.magic(v): themeColor);
      };
  };

  let string = StringOrThemeColor.string;
  let themeColor = StringOrThemeColor.themeColor;
  let classify = StringOrThemeColor.classify;
  type stringOrThemeColor = StringOrThemeColor.t;
};

[@unboxed]
type any =
  | Any('a): any;

module Api = {
  type t;

  [@bs.val] external acquireVsCodeApi: unit => t = "acquireVsCodeApi";

  [@bs.send] external postMessage: (t, 'a) => unit = "postMessage";
  let onMessage = (callback: 'a => unit): unit => {
    Webapi.Dom.window
    |> Webapi.Dom.Window.addEventListener("message", _event => {
         callback([%raw "_event.data"])
       });
  };
};

module Disposable = {
  type t;
  // static
  [@bs.val] external from: array({. "dispose": unit => 'a}) => t = "from";
  // constructor
  [@bs.module "vscode"] [@bs.new]
  external make: (unit => unit) => t = "Disposable";
  // methods
  [@bs.send] external dispose: t => 'a = "dispose";
};

// https://code.visualstudio.com/api/references/vscode-api#Memento
module Memento = {
  type t;
  // methods
  [@bs.send] external get: (t, string) => option('a) = "get";
  [@bs.send] external getWithDefault: (t, string, 'a) => 'a = "get";
  [@bs.send] external update: (t, string, 'a) => Promise.t(unit) = "update";
};

module ExtensionContext = {
  type t;
  // properties
  [@bs.get] external extensionPath: t => string = "extensionPath";
  [@bs.get] external globalState: t => Memento.t = "globalState";
  [@bs.get] external globalStoragePath: t => string = "globalStoragePath";
  [@bs.get] external logPath: t => string = "logPath";
  [@bs.get] external storagePath: t => option(string) = "storagePath";
  [@bs.get]
  external subscriptions: t => array(Disposable.t) = "subscriptions";
  [@bs.get] external workspaceState: t => Memento.t = "workspaceState";
  // methods
  [@bs.send] external asAbsolutePath: (t, string) => string = "asAbsolutePath";
};

module Commands = {
  [@bs.module "vscode"] [@bs.scope "commands"]
  external registerCommand: (string, 'a => unit) => Disposable.t =
    "registerCommand";

  // [@bs.module "vscode"] [@bs.scope "commands"]
  // external executeCommand: string => Promise.t('a) = "executeCommand";

  module Layout = {
    [@unboxed]
    type group =
      | Group('a): group;

    type sized = {
      groups: array(group),
      size: float,
    };

    let simple = Group(Js.Dict.empty());
    let sized = (v: sized) => Group(v);

    type t = {
      orientation: int,
      groups: array(group),
    };
  };

  [@bs.module "vscode"] [@bs.scope "commands"]
  external executeCommand:
    (
    [@bs.string]
    [ | [@bs.as "vscode.setEditorLayout"] `setEditorLayout(Layout.t)]
    ) =>
    Promise.t('a) =
    "executeCommand";
};

module Uri = {
  type t;

  [@bs.module "vscode"] [@bs.scope "Uri"] external file: string => t = "file";
  [@bs.module "vscode"] [@bs.scope "Uri"]
  external parse: (string, option(bool)) => t = "file";

  [@bs.new]
  external make: (string, string, string, string, string) => t = "Uri";

  [@bs.get] external authority: t => string = "authority";
  [@bs.get] external fragment: t => string = "fragment";
  [@bs.get] external fsPath: t => string = "fsPath";
  [@bs.get] external path: t => string = "path";
  [@bs.get] external query: t => string = "query";
  [@bs.get] external scheme: t => string = "scheme";

  [@bs.send] external toJSON: t => Js.Json.t = "toJSON";
  [@bs.send] external toString: t => string = "toString";

  type change = {
    authority: option(string),
    fragment: option(string),
    path: option(string),
    query: option(string),
    scheme: option(string),
  };

  let makeChange =
      (~authority=?, ~fragment=?, ~path=?, ~query=?, ~scheme=?, ()): change => {
    authority,
    fragment,
    path,
    query,
    scheme,
  };
  [@bs.send] external with_: (t, change) => t = "with";

  module StringOrUri: {
    type uri = t;
    type t;
    type case =
      | String(string)
      | Uri(uri);
    let uri: uri => t;
    let string: string => t;
    let classify: t => case;
  } = {
    type uri = t;
    [@unboxed]
    type t =
      | Any('a): t;
    type case =
      | String(string)
      | Uri(uri);
    let uri = (v: uri) => Any(v);
    let string = (v: string) => Any(v);
    let classify = (Any(v): t): case =>
      if (Js.typeof(v) == "string") {
        String(Obj.magic(v): string);
      } else {
        Uri(Obj.magic(v): uri);
      };
  };

  let string = StringOrUri.string;
  let uri = StringOrUri.uri;
  let classify = StringOrUri.classify;
  type stringOrUri = StringOrUri.t;
};

module ViewColumn = {
  type t =
    | Active
    | Beside
    | Eight
    | Five
    | Four
    | Nine
    | One
    | Seven
    | Six
    | Three
    | Two;

  let toEnum =
    fun
    | Active => (-1)
    | Beside => (-2)
    | Eight => 8
    | Five => 5
    | Four => 4
    | Nine => 9
    | One => 1
    | Seven => 7
    | Six => 6
    | Three => 3
    | Two => 2;
  let fromEnum =
    fun
    | (-1) => Active
    | (-2) => Beside
    | 8 => Eight
    | 5 => Five
    | 4 => Four
    | 9 => Nine
    | 1 => One
    | 7 => Seven
    | 6 => Six
    | 3 => Three
    | _ => Two;
};
module WebviewOptions = {
  type portMapping;
  type t = {
    enableCommandUris: option(bool),
    enableScripts: option(bool),
    localResourceRoots: option(array(Uri.t)),
    portMapping: option(array(portMapping)),
  };
};

// https://code.visualstudio.com/api/references/vscode-api#Webview
module Webview = {
  type t;
  // events
  [@bs.send]
  external onDidReceiveMessage: (t, 'a) => Disposable.t =
    "onDidReceiveMessage";
  // properties
  [@bs.get] external cspSource: t => string = "cspSource";
  [@bs.get] external html: t => string = "html";
  [@bs.set] external setHtml: (t, string) => unit = "html";
  [@bs.get] external options: t => WebviewOptions.t = "options";
  // methods
  [@bs.send] external asWebviewUri: (t, Uri.t) => Uri.t = "asWebviewUri";
  [@bs.send] external postMessage: (t, 'a) => Promise.t(bool) = "postMessage";
};

module WebviewPanel = {
  type t;

  // https://code.visualstudio.com/api/references/vscode-api#WebviewPanelOnDidChangeViewStateEvent
  module OnDidChangeViewStateEvent = {
    type webviewPanel = t;
    type t;
    // properties
    [@bs.get] external webviewPanel: t => webviewPanel = "webviewPanel";
  };

  // https://code.visualstudio.com/api/references/vscode-api#WebviewPanelOptions
  module Options = {
    type t;
    // properties
    [@bs.get]
    external enableFindWidget: t => option(bool) = "enableFindWidget";
    [@bs.get]
    external retainContextWhenHidden: t => option(bool) =
      "retainContextWhenHidden";
  };

  // events
  [@bs.send]
  external onDidChangeViewState:
    (t, OnDidChangeViewStateEvent.t => unit) => Disposable.t =
    "onDidChangeViewState";
  [@bs.send]
  external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose";

  // properties
  [@bs.get] external active: t => bool = "active";
  type uriOrLightAndDark =
    | Uri(Uri.t)
    | LightAndDark(
        {
          .
          "dark": Uri.t,
          "light": Uri.t,
        },
      );

  [@bs.get]
  external iconPath_raw: t => option(Js.Dict.t(Uri.t)) = "iconPath";
  let iconPath = (self): option(uriOrLightAndDark) => {
    iconPath_raw(self)
    ->Belt.Option.map(case =>
        if (Belt.Option.isSome(Js.Dict.get(case, "dark"))) {
          LightAndDark(
            Obj.magic(case): {
                              .
                              "dark": Uri.t,
                              "light": Uri.t,
                            },
          );
        } else {
          Uri(Obj.magic(case): Uri.t);
        }
      );
  };
  [@bs.get] external options: t => Options.t = "options";
  [@bs.get] external title: t => string = "title";
  [@bs.get] external viewColumn_raw: t => option(int) = "viewColumn";
  let viewColumn = (self: t): option(ViewColumn.t) =>
    viewColumn_raw(self)->Belt.Option.map(ViewColumn.fromEnum);

  [@bs.get] external viewType: t => string = "viewType";
  [@bs.get] external visible: t => bool = "visible";
  [@bs.get] external webview: t => Webview.t = "webview";
  // methods
  [@bs.send] external dispose: t => unit = "dispose";
  [@bs.send]
  external reveal_raw:
    (t, ~viewColumn: int=?, ~preserveFocus: bool=?, unit) => unit =
    "reveal";
  let reveal = (self: t, ~viewColumn=?, ~preserveFocus=?, ()): unit => {
    let viewColumn = viewColumn->Belt.Option.map(ViewColumn.toEnum);
    switch (viewColumn, preserveFocus) {
    | (None, None) => reveal_raw(self, ())
    | (None, Some(preserveFocus)) => reveal_raw(self, ~preserveFocus, ())
    | (Some(viewColumn), None) => reveal_raw(self, ~viewColumn, ())
    | (Some(viewColumn), Some(preserveFocus)) =>
      reveal_raw(self, ~viewColumn, ~preserveFocus, ())
    };
  };
};

// https://code.visualstudio.com/api/references/vscode-api#Position
module Position = {
  type t;
  // constructor
  [@bs.module "vscode"] [@bs.new] external make: (int, int) => t = "Position";
  // properties
  [@bs.get] external character: t => int = "character";
  [@bs.get] external line: t => int = "line";
  // methods
  [@bs.send] external compareTo: (t, t) => unit = "compareTo";
  [@bs.send] external isAfter: (t, t) => bool = "isAfter";
  [@bs.send] external isAfterOrEqual: (t, t) => bool = "isAfterOrEqual";
  [@bs.send] external isBefore: (t, t) => bool = "isBefore";
  [@bs.send] external isBeforeOrEqual: (t, t) => bool = "isBeforeOrEqual";
  [@bs.send] external isEqual: (t, t) => bool = "isEqual";
  [@bs.send] external translate: (t, int, int) => t = "translate";
  [@bs.send] external with_: (t, int, int) => t = "with";
};

// https://code.visualstudio.com/api/references/vscode-api#Range
module Range = {
  type t;
  // constructor
  [@bs.module "vscode"] [@bs.new]
  external make: (Position.t, Position.t) => t = "Range";
  [@bs.module "vscode"] [@bs.new]
  external makeWithNumbers: (int, int, int, int) => t = "Range";
  // properties
  [@bs.get] external end_: t => Position.t = "end";
  [@bs.get] external isEmpty: t => bool = "isEmpty";
  [@bs.get] external isSingleLine: t => bool = "isSingleLine";
  [@bs.get] external start: t => Position.t = "start";
  // methods
  [@bs.send] external contains: (t, Position.t) => bool = "contains";
  [@bs.send] external containsRange: (t, t) => bool = "contains";
  [@bs.send] external intersection: (t, t) => option(t) = "intersection";
  [@bs.send] external isEqual: (t, t) => bool = "isEqual";
  [@bs.send] external union: (t, t) => t = "union";
  [@bs.send] external with_: (t, Position.t, Position.t) => t = "with";
};

// https://code.visualstudio.com/api/references/vscode-api#TextLine
module TextLine = {
  type t;
  // properties
  [@bs.get]
  external firstNonWhitespaceCharacterIndex: t => int =
    "firstNonWhitespaceCharacterIndex";
  [@bs.get] external isEmptyOrWhitespace: t => bool = "isEmptyOrWhitespace";
  [@bs.get] external lineNumber: t => int = "lineNumber";
  [@bs.get] external range: t => Range.t = "range";
  [@bs.get]
  external rangeIncludingLineBreak: t => Range.t = "rangeIncludingLineBreak";
  [@bs.get] external text: t => string = "text";
};

// https://code.visualstudio.com/api/references/vscode-api#EndOfLine
module EndOfLine = {
  type t =
    | CRLF
    | LF;

  let toEnum =
    fun
    | CRLF => 2
    | LF => 1;
  let fromEnum =
    fun
    | 2 => CRLF
    | _ => LF;
};

module TextDocument = {
  type t;
  // properties
  [@bs.get] external eol_raw: t => int = "eol";
  let eol = (self: t): EndOfLine.t => EndOfLine.fromEnum(eol_raw(self));
  [@bs.get] external fileName: t => string = "fileName";
  [@bs.get] external isClosed: t => bool = "isClosed";
  [@bs.get] external isDirty: t => bool = "isDirty";
  [@bs.get] external isUntitled: t => bool = "isUntitled";
  [@bs.get] external languageId: t => string = "languageId";
  [@bs.get] external lineCount: t => int = "lineCount";
  [@bs.get] external uri: t => Uri.t = "uri";
  [@bs.get] external version: t => int = "version";
  // methods
  [@bs.send] external getText: (t, option(Range.t)) => string = "getText";
  [@bs.send]
  external getWordRangeAtPosition:
    (t, Position.t, option(Js.Re.t)) => option(Range.t) =
    "getWordRangeAtPosition";
  [@bs.send] external lineAt: (t, int) => TextLine.t = "lineAt";
  [@bs.send] external lineAtPosition: (t, Position.t) => TextLine.t = "lineAt";
  [@bs.send] external offsetAt: (t, Position.t) => int = "offsetAt";
  [@bs.send] external positionAt: (t, int) => Position.t = "positionAt";
  [@bs.send] external save: t => Promise.t(bool) = "save";
  [@bs.send]
  external validatePosition: (t, Position.t) => Position.t =
    "validatePosition";
  [@bs.send] external validateRange: (t, Range.t) => Range.t = "validateRange";
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorCursorStyle
module TextEditorCursorStyle = {
  type t =
    | Block
    | BlockOutline
    | Line
    | LineThing
    | Underline
    | UnderlineThin;
  let toEnum =
    fun
    | Block => 2
    | BlockOutline => 5
    | Line => 1
    | LineThing => 4
    | Underline => 3
    | UnderlineThin => 6;
  let fromEnum =
    fun
    | 2 => Block
    | 5 => BlockOutline
    | 1 => Line
    | 4 => LineThing
    | 3 => Underline
    | _ => UnderlineThin;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorLineNumbersStyle
module TextEditorLineNumbersStyle = {
  type t =
    | Off
    | On
    | Relative;
  let toEnum =
    fun
    | Off => 0
    | On => 1
    | Relative => 2;
  let fromEnum =
    fun
    | 0 => Off
    | 1 => On
    | _ => Relative;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptions
module TextEditorOptions = {
  type t;
  // properties
  [@bs.get] external cursorStyle_raw: t => option(int) = "cursorStyle";
  let cursorStyle = (self: t): option(TextEditorCursorStyle.t) =>
    cursorStyle_raw(self)->Belt.Option.map(TextEditorCursorStyle.fromEnum);

  type boolOrString =
    | Bool(bool)
    | String(string);
  type numberOrString =
    | Number(int)
    | String(string);

  [@bs.get] external insertSpaces_raw: t => option(any) = "insertSpaces";
  let insertSpaces = (self): option(boolOrString) => {
    insertSpaces_raw(self)
    ->Belt.Option.map(case =>
        if (Js.typeof(case) == "boolean") {
          Bool(Obj.magic(case): bool);
        } else {
          String(Obj.magic(case): string);
        }
      );
  };
  [@bs.get] external lineNumbers_raw: t => option(int) = "lineNumbers";

  let lineNumbers = (self: t): option(TextEditorLineNumbersStyle.t) =>
    lineNumbers_raw(self)
    ->Belt.Option.map(TextEditorLineNumbersStyle.fromEnum);

  [@bs.get] external tabSize_raw: t => option(any) = "tabSize";
  let tabSize = (self): option(numberOrString) => {
    tabSize_raw(self)
    ->Belt.Option.map(case =>
        if (Js.typeof(case) == "number") {
          Number(Obj.magic(case): int);
        } else {
          String(Obj.magic(case): string);
        }
      );
  };
};

// https://code.visualstudio.com/api/references/vscode-api#Selection
module Selection = {
  type t;
  // constructors
  [@bs.module "vscode"] [@bs.new]
  external make: (Position.t, Position.t) => t = "Selection";
  [@bs.module "vscode"] [@bs.new]
  external makeWithNumbers: (int, int, int, int) => t = "Selection";
  // properties
  [@bs.get] external active: t => Position.t = "active";
  [@bs.get] external anchor: t => Position.t = "anchor";
  [@bs.get] external end_: t => Position.t = "end";
  [@bs.get] external isEmpty: t => bool = "isEmpty";
  [@bs.get] external isReversed: t => bool = "isReversed";
  [@bs.get] external isSingleLine: t => bool = "isSingleLine";
  [@bs.get] external start: t => Position.t = "start";
  // methods
  [@bs.send] external contains: (t, Position.t) => bool = "contains";
  [@bs.send] external containsRange: (t, Range.t) => bool = "contains";
  [@bs.send]
  external intersection: (t, Range.t) => option(Range.t) = "intersection";
  [@bs.send] external isEqual: (t, Range.t) => bool = "isEqual";
  [@bs.send] external union: (t, Range.t) => Range.t = "union";
  [@bs.send] external with_: (t, Position.t, Position.t) => Range.t = "with";
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorEdit
module TextEditorEdit = {
  type t;
  // methods
  [@bs.send] external delete: (t, Range.t) => unit = "delete";
  [@bs.send] external deleteAtSelection: (t, Selection.t) => unit = "delete";
  [@bs.send] external insert: (t, Position.t, string) => unit = "insert";
  [@bs.send] external replace: (t, Position.t, string) => unit = "replace";
  [@bs.send] external replaceAtRange: (t, Range.t, string) => unit = "replace";
  [@bs.send]
  external replaceAtSelection: (t, Selection.t, string) => unit = "replace";
  [@bs.send] external setEndOfLine_raw: (t, int) => unit = "setEndOfLine";
  let setEndOfLine = (self: t, eol: EndOfLine.t): unit =>
    setEndOfLine_raw(self, EndOfLine.toEnum(eol));
};

// https://code.visualstudio.com/api/references/vscode-api#SnippetString
module SnippetString = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorRevealType
module TextEditorRevealType = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorDecorationType
module TextEditorDecorationType = {
  type t;
  // properties
  [@bs.get] external key: t => string = "key";
  // methods
  [@bs.send] external dispose: t => unit = "dispose";
};

// https://code.visualstudio.com/api/references/vscode-api#MarkedString;
module MarkdownString = {
  type t;
  // constructors
  [@bs.module "vscode"] [@bs.new]
  external make: (string, bool) => t = "MarkdownString";
  // properties
  [@bs.get] external isTrusted: t => option(bool) = "isTrusted";
  [@bs.get] external value: t => string = "value";
  // methods
  [@bs.send]
  external appendCodeblock: (t, string, option(string)) => t =
    "appendCodeblock";
  [@bs.send] external appendMarkdown: (t, string) => t = "appendMarkdown";
  [@bs.send] external appendText: (t, string) => t = "appendText";
};

// https://code.visualstudio.com/api/references/vscode-api#ThemableDecorationAttachmentRenderOptions
module ThemableDecorationAttachmentRenderOptions = {
  type t;
  // properties
  [@bs.get] external backgroundColor: t => option(string) = "backgroundColor";
  [@bs.get] external border: t => option(string) = "border";
  [@bs.get] external borderColor: t => option(string) = "borderColor";
  [@bs.get] external color: t => option(string) = "color";
  [@bs.get] external contentIconPath: t => option(string) = "contentIconPath";
  [@bs.get] external contentText: t => option(string) = "contentText";
  [@bs.get] external fontStyle: t => option(string) = "fontStyle";
  [@bs.get] external fontWeight: t => option(string) = "fontWeight";
  [@bs.get] external height: t => option(string) = "height";
  [@bs.get] external margin: t => option(string) = "margin";
  [@bs.get] external textDecoration: t => option(string) = "textDecoration";
  [@bs.get] external width: t => option(string) = "width";
};

// https://code.visualstudio.com/api/references/vscode-api#ThemableDecorationInstanceRenderOptions
module ThemableDecorationInstanceRenderOptions = {
  type t;
  // properties
  [@bs.get]
  external after: t => option(ThemableDecorationAttachmentRenderOptions.t) =
    "after";
  [@bs.get]
  external before: t => option(ThemableDecorationAttachmentRenderOptions.t) =
    "before";
};

// https://code.visualstudio.com/api/references/vscode-api#DecorationInstanceRenderOptions;
module DecorationInstanceRenderOptions = {
  type t;
  // properties
  [@bs.get]
  external after: t => option(ThemableDecorationAttachmentRenderOptions.t) =
    "after";
  [@bs.get]
  external before: t => option(ThemableDecorationAttachmentRenderOptions.t) =
    "before";
  [@bs.get]
  external dark: t => option(ThemableDecorationInstanceRenderOptions.t) =
    "dark";
  [@bs.get]
  external light: t => option(ThemableDecorationInstanceRenderOptions.t) =
    "light";
};

// https://code.visualstudio.com/api/references/vscode-api#DecorationOptions
module DecorationOptions = {
  type t;
  // properties
  [@bs.get]
  external hoverMessage: t => option(MarkdownString.t) = "hoverMessage";
  [@bs.get] external range: t => Range.t = "range";
  [@bs.get]
  external renderOptions: t => option(DecorationInstanceRenderOptions.t) =
    "renderOptions";
};

module TextEditor = {
  type t;
  // properties
  [@bs.get] external document: t => TextDocument.t = "document";
  [@bs.get] external options: t => TextEditorOptions.t = "options";
  [@bs.get] external selection: t => Selection.t = "selection";
  [@bs.set] external setSelection: (t, Selection.t) => unit = "selection";
  [@bs.get] external selections: t => array(Selection.t) = "selections";
  [@bs.get] external viewColumn_raw: t => option(int) = "viewColumn";
  let viewColumn = (self: t): option(ViewColumn.t) =>
    viewColumn_raw(self)->Belt.Option.map(ViewColumn.fromEnum);
  [@bs.get] external visibleRanges: t => array(Range.t) = "visibleRanges";
  // methods
  [@bs.send]
  external edit:
    (
      t,
      TextEditorEdit.t => unit,
      option({
        .
        "undoStopAfter": bool,
        "undoStopBefore": bool,
      })
    ) =>
    Promise.t(bool) =
    "edit";
  [@bs.send] external hide: (t, unit) => unit = "hide";
  [@bs.send]
  external insertSnippet:
    (
      t,
      SnippetString.t,
      Position.t,
      option({
        .
        "undoStopAfter": bool,
        "undoStopBefore": bool,
      })
    ) =>
    Promise.t(bool) =
    "insertSnippet";
  [@bs.send]
  external insertSnippetAtRange:
    (
      t,
      SnippetString.t,
      Range.t,
      option({
        .
        "undoStopAfter": bool,
        "undoStopBefore": bool,
      })
    ) =>
    Promise.t(bool) =
    "insertSnippet";
  [@bs.send]
  external insertSnippetAtPositions:
    (
      t,
      SnippetString.t,
      array(Position.t),
      option({
        .
        "undoStopAfter": bool,
        "undoStopBefore": bool,
      })
    ) =>
    Promise.t(bool) =
    "insertSnippet";
  [@bs.send]
  external insertSnippetAtRanges:
    (
      t,
      SnippetString.t,
      array(Range.t),
      option({
        .
        "undoStopAfter": bool,
        "undoStopBefore": bool,
      })
    ) =>
    Promise.t(bool) =
    "insertSnippet";
  [@bs.send]
  external revealRange: (t, Range.t, option(TextEditorRevealType.t)) => unit =
    "revealRange";
  [@bs.send]
  external setDecorations:
    (t, TextEditorDecorationType.t, array(Range.t)) => unit =
    "setDecorations";
  [@bs.send]
  external setDecorationsWithOptions:
    (t, TextEditorDecorationType.t, array(DecorationOptions.t)) => unit =
    "setDecorations";
  [@bs.send] external show_raw: (t, option(int)) => unit = "show";
  let show = (self: t, viewColumn: option(ViewColumn.t)): unit =>
    show_raw(self, viewColumn->Belt.Option.map(ViewColumn.toEnum));
};

module Terminal = {
  type t;
};

module WindowState = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptionsChangeEvent
module TextEditorOptionsChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeEvent
module TextEditorSelectionChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeKind
module TextEditorSelectionChangeKind = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorViewColumnChangeEvent
module TextEditorViewColumnChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorVisibleRangesChangeEvent
module TextEditorVisibleRangesChangeEvent = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#InputBox
module InputBox = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#OutputChannel
module OutputChannel = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItem
module QuickPickItem = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#QuickPick
module QuickPick = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#StatusBarItem
module StatusBarItem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TerminalOptions
module TerminalOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ExtensionTerminalOptions
module ExtensionTerminalOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#OverviewRulerLane;
module OverviewRulerLane = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#DecorationRangeBehavior;
module DecorationRangeBehavior = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#DecorationRenderOptions
module DecorationRenderOptions = {
  [@bs.deriving abstract]
  type t = {
    [@bs.optional]
    after: ThemableDecorationAttachmentRenderOptions.t,
    [@bs.optional]
    backgroundColor: ThemeColor.stringOrThemeColor,
    [@bs.optional]
    before: ThemableDecorationAttachmentRenderOptions.t,
    [@bs.optional]
    border: string,
    [@bs.optional]
    borderColor: ThemeColor.stringOrThemeColor,
    [@bs.optional]
    borderRadius: string,
    [@bs.optional]
    borderSpacing: string,
    [@bs.optional]
    borderStyle: string,
    [@bs.optional]
    borderWidth: string,
    [@bs.optional]
    color: ThemeColor.stringOrThemeColor,
    [@bs.optional]
    cursor: string,
    [@bs.optional]
    dark: ThemableDecorationInstanceRenderOptions.t,
    [@bs.optional]
    fontStyle: string,
    [@bs.optional]
    fontWeight: string,
    [@bs.optional]
    gutterIconPath: Uri.stringOrUri,
    [@bs.optional]
    gutterIconSize: string,
    [@bs.optional]
    isWholeLine: bool,
    [@bs.optional]
    letterSpacing: string,
    [@bs.optional]
    light: ThemableDecorationInstanceRenderOptions.t,
    [@bs.optional]
    opacity: string,
    [@bs.optional]
    outline: string,
    [@bs.optional]
    outlineColor: ThemeColor.stringOrThemeColor,
    [@bs.optional]
    outlineStyle: string,
    [@bs.optional]
    outlineWidth: string,
    [@bs.optional]
    overviewRulerColor: ThemeColor.stringOrThemeColor,
    [@bs.optional]
    overviewRulerLane: OverviewRulerLane.t,
    [@bs.optional]
    rangeBehavior: DecorationRangeBehavior.t,
    [@bs.optional]
    textDecoration: string,
  };
};

// https://code.visualstudio.com/api/references/vscode-api#TreeViewOptions
module TreeViewOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TreeView
module TreeView = {
  type t;
};

// WebviewPanelOptions & WebviewOptions
module WebviewAndWebviewPanelOptions = {
  type t = {
    enableCommandUris: option(bool),
    enableScripts: option(bool),
    localResourceRoots: option(array(Uri.t)),
    portMapping: option(array(WebviewOptions.portMapping)),
    enableFindWidget: option(bool),
    retainContextWhenHidden: option(bool),
  };

  let make =
      (
        ~enableCommandUris=?,
        ~enableScripts=?,
        ~localResourceRoots=?,
        ~portMapping=?,
        ~enableFindWidget=?,
        ~retainContextWhenHidden=?,
        (),
      )
      : t => {
    enableCommandUris,
    enableScripts,
    localResourceRoots,
    portMapping,
    enableFindWidget,
    retainContextWhenHidden,
  };
};

module ViewColumnAndPreserveFocus = {
  type t = {
    preserveFocus: bool,
    viewColumn: int,
  };
};

// https://code.visualstudio.com/api/references/vscode-api#TreeDataProvider
module TreeDataProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#UriHandler
module UriHandler = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#WebviewPanelSerializer
module WebviewPanelSerializer = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#MessageOptions
module MessageOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#MessageItem
module MessageItem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#InputBoxOptions
module InputBoxOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#CancellationToken
module CancellationToken = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#OpenDialogOptions
module OpenDialogOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#QuickPickOptions
module QuickPickOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#SaveDialogOptions
module SaveDialogOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolderPickOptions
module WorkspaceFolderPickOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolder
module WorkspaceFolder = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ProgressOptions
module ProgressOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#Progress
module Progress = {
  type t('a);
};

// https://code.visualstudio.com/api/references/vscode-api#window
module Window = {
  // variables
  [@bs.module "vscode"] [@bs.scope "window"]
  external activeTerminal: option(Terminal.t) = "activeTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external activeTextEditor: option(TextEditor.t) = "activeTextEditor";
  [@bs.module "vscode"] [@bs.scope "window"]
  external state: WindowState.t = "state";
  [@bs.module "vscode"] [@bs.scope "window"]
  external terminals: array(Terminal.t) = "terminals";
  [@bs.module "vscode"] [@bs.scope "window"]
  external visibleTextEditors: array(TextEditor.t) = "visibleTextEditors";
  // events
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTerminal:
    (option(Terminal.t) => unit) => Disposable.t =
    "onDidChangeActiveTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTextEditor:
    (option(TextEditor.t) => unit) => Disposable.t =
    "onDidChangeActiveTextEditor";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorOptions:
    (option(TextEditorOptionsChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorOptions";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorSelection:
    (option(TextEditorSelectionChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorSelection";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorViewColumn:
    (option(TextEditorViewColumnChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorViewColumn";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorVisibleRanges:
    (option(TextEditorVisibleRangesChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorVisibleRanges";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeVisibleTextEditors:
    (option(TextEditor.t) => unit) => Disposable.t =
    "onDidChangeVisibleTextEditors";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeWindowState:
    (option(WindowState.t) => unit) => Disposable.t =
    "onDidChangeWindowState";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidCloseTerminal: (option(Terminal.t) => unit) => Disposable.t =
    "onDidCloseTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidOpenTerminal: (option(Terminal.t) => unit) => Disposable.t =
    "onDidOpenTerminal";
  // functions
  [@bs.module "vscode"] [@bs.scope "window"]
  external createInputBox: unit => InputBox.t = "createInputBox";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createOutputChannel: string => OutputChannel.t =
    "createOutputChannel";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createQuickPick: QuickPickItem.t => QuickPick.t = "createQuickPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createStatusBarItem:
    (~alignment: [@bs.int] [ | `Left | `Right]=?, ~priority: int=?, unit) =>
    StatusBarItem.t =
    "createStatusBarItem";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminal:
    (
      ~name: string=?,
      ~shellPath: string=?,
      ~shellArgs: array(string)=?,
      unit
    ) =>
    Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminalWithTerminalOptions: TerminalOptions.t => Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminalWithExtensionTerminalOptions:
    ExtensionTerminalOptions.t => Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTextEditorDecorationType:
    DecorationRenderOptions.t => TextEditorDecorationType.t =
    "createTextEditorDecorationType";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTreeView: (string, TreeViewOptions.t) => TreeView.t =
    "createTreeView";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createWebviewPanel:
    (
      string,
      string,
      ViewColumnAndPreserveFocus.t,
      option(WebviewAndWebviewPanelOptions.t)
    ) =>
    WebviewPanel.t =
    "createWebviewPanel";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerTreeDataProvider:
    (string, TreeDataProvider.t) => Disposable.t =
    "registerTreeDataProvider";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerUriHandler: UriHandler.t => Disposable.t =
    "registerUriHandler";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerWebviewPanelSerializer:
    (string, WebviewPanelSerializer.t) => Disposable.t =
    "registerWebviewPanelSerializer";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessageAndHideAfterTimeout:
    (string, int) => Disposable.t =
    "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessageAndHideWhenDone:
    (string, Promise.t('a)) => Disposable.t =
    "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessage: string => Disposable.t = "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showErrorMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showErrorMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showErrorMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showErrorMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showInformationMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showInformationMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showInformationMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showInformationMessage";

  // module InputBoxOptions = {
  //   type t = {
  //     ignoreFocusOut: bool,
  //     password: bool,
  //     placeHolder: string,
  //     value: string,
  //     valueSelection: (int, int),
  //   };
  // };
  // module CancellationToken = {
  //   type t = {isCancellationRequested: bool};
  // };
  [@bs.module "vscode"] [@bs.scope "window"]
  external showInputBox:
    (~option: InputBoxOptions.t=?, ~token: CancellationToken.t=?, unit) =>
    Promise.t(option(string)) =
    "showInputBox";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showOpenDialog: OpenDialogOptions.t => Promise.t(option(Uri.t)) =
    "shoeOpenDialog";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showQuickPick:
    (
      Promise.t(array(string)),
      QuickPickOptions.t,
      option(CancellationToken.t)
    ) =>
    Promise.t(option(array(string))) =
    "showQuickPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showSaveDialog: SaveDialogOptions.t => Promise.t(option(Uri.t)) =
    "showSaveDialog";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showTextDocument:
    (TextDocument.t, ~column: ViewColumn.t=?, ~preserveFocus: bool=?, unit) =>
    Promise.t(option(Uri.t)) =
    "showTextDocument";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showWarningMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showWarningMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showWarningMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showWarningMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showWorkspaceFolderPick:
    option(WorkspaceFolderPickOptions.t) =>
    Promise.t(option(WorkspaceFolder.t)) =
    "showWorkspaceFolderPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external withProgress:
    (
      ProgressOptions.t,
      (
        Progress.t({
          .
          "increment": int,
          "message": string,
        }),
        CancellationToken.t
      ) =>
      Promise.t('a)
    ) =>
    Promise.t('a) =
    "withProgress";
  [@bs.module "vscode"] [@bs.scope "window"]
  external withScmProgress:
    ((Progress.t(int), CancellationToken.t) => Promise.t('a)) =>
    Promise.t('a) =
    "withScmProgress";
};

// https://code.visualstudio.com/api/references/vscode-api#FileSystem
module FileSystem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ConfigurationChangeEvent
module ConfigurationChangeEvent = {
  type t;

  [@bs.send]
  external affectsConfiguration:
    (
      t,
      string,
      [@bs.unwrap] [
        | `Uri(Uri.t)
        | `TextDocument(TextDocument.t)
        | `WorkspaceFolder(WorkspaceFolder.t)
        | `Others(
            option({
              .
              "languageId": string,
              "uri": Uri.t,
            }),
          )
      ]
    ) =>
    bool =
    "affectsConfiguration";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentChangeEvent
module TextDocumentContentChangeEvent = {
  type t;
  // properties
  [@bs.get] external range: t => Range.t = "range";
  [@bs.get] external rangeLength: t => int = "rangeLength";
  [@bs.get] external rangeOffset: t => int = "rangeOffset";
  [@bs.get] external text: t => string = "text";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentChangeEvent
module TextDocumentChangeEvent = {
  type t;
  // properties
  [@bs.get]
  external contentChanges: t => array(TextDocumentContentChangeEvent.t) =
    "contentChanges";
  [@bs.get] external document: t => TextDocument.t = "document";
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFoldersChangeEvent
module WorkspaceFoldersChangeEvent = {
  type t;
  // properties
  [@bs.get] external added: t => array(WorkspaceFolder.t) = "added";
  [@bs.get] external removed: t => array(WorkspaceFolder.t) = "removed";
};

// https://code.visualstudio.com/api/references/vscode-api#FileCreateEvent
module FileCreateEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
};

// https://code.visualstudio.com/api/references/vscode-api#FileDeleteEvent
module FileDeleteEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
};

// https://code.visualstudio.com/api/references/vscode-api#FileRenameEvent
module FileRenameEvent = {
  type t;
  // properties
  [@bs.get]
  external files:
    t =>
    array({
      .
      "newUri": Uri.t,
      "oldUri": Uri.t,
    }) =
    "files";
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceEdit
module WorkspaceEdit = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileWillCreateEvent
module FileWillCreateEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};
// https://code.visualstudio.com/api/references/vscode-api#FileWillDeleteEvent
module FileWillDeleteEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};
// https://code.visualstudio.com/api/references/vscode-api#FileWillRenameEvent
module FileWillRenameEvent = {
  type t;
  // properties
  [@bs.get]
  external files:
    t =>
    array({
      .
      "newUri": Uri.t,
      "oldUri": Uri.t,
    }) =
    "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentSaveReason
module TextDocumentSaveReason = {
  type t =
    | AfterDelay
    | FocusOut
    | Manual;

  let toEnum =
    fun
    | AfterDelay => 2
    | FocusOut => 3
    | Manual => 1;
  let fromEnum =
    fun
    | 2 => AfterDelay
    | 3 => FocusOut
    | _ => Manual;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEdit
module TextEdit = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextDocumentWillSaveEvent
module TextDocumentWillSaveEvent = {
  type t;
  // properties
  [@bs.get] external document: t => TextDocument.t = "document";
  [@bs.get] external reason_raw: t => int = "reason";
  let reason = self => TextDocumentSaveReason.fromEnum(self->reason_raw);
  // methods
  [@bs.send]
  external waitUntilWithTextEdit: (t, Promise.t(TextEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};

// https://code.visualstudio.com/api/references/vscode-api#GlobPattern
module GlobPattern = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileSystemWatcher
module FileSystemWatcher = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceConfiguration
module WorkspaceConfiguration = {
  type t;
  // methods
  [@bs.send] external get: (t, string) => option('a) = "get";
  [@bs.send] external getWithDefault: (t, string, 'a) => 'a = "get";
  [@bs.send] external has: (t, string) => bool = "has";
  [@bs.send]
  external inspect:
    (t, string) =>
    option({
      .
      "defaultLanguageValue": 'a,
      "defaultValue": 'a,
      "globalLanguageValue": 'a,
      "globalValue": 'a,
      "key": string,
      "languageIds": array(string),
      "workspaceFolderLanguageValue": 'a,
      "workspaceFolderValue": 'a,
      "workspaceLanguageValue": 'a,
      "workspaceValue": 'a,
    }) =
    "inspect";
  [@bs.send]
  external updateGlobalSettings:
    (t, string, 'a, [@bs.as 1] _, option(bool)) => Promise.t(unit) =
    "update";
  [@bs.send]
  external updateWorkspaceSettings:
    (t, string, 'a, [@bs.as 2] _, option(bool)) => Promise.t(unit) =
    "update";
  [@bs.send]
  external updateWorkspaceFolderSettings:
    (t, string, 'a, [@bs.as 3] _, option(bool)) => Promise.t(unit) =
    "update";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentProvider
module TextDocumentContentProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TaskProvider
module TaskProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileSystemProvider
module FileSystemProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#workspace
module Workspace = {
  // variables
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external fs: FileSystem.t = "fs";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external name: option(string) = "name";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external rootPath: option(string) = "rootPath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external textDocuments: array(TextDocument.t) = "textDocuments";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external workspaceFile: option(Uri.t) = "workspaceFile";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external workspaceFolders: option(array(WorkspaceFolder.t)) =
    "workspaceFolders";

  // events
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeConfiguration:
    (option(ConfigurationChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeConfiguration";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeTextDocument:
    (option(TextDocumentChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeWorkspaceFolders:
    (option(WorkspaceFoldersChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeWorkspaceFolders";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidCloseTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidCloseTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidCreateFiles:
    (option(FileCreateEvent.t) => unit) => Disposable.t =
    "onDidCreateFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidDeleteFiles:
    (option(FileDeleteEvent.t) => unit) => Disposable.t =
    "onDidDeleteFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidOpenTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidOpenTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidRenameFiles:
    (option(FileRenameEvent.t) => unit) => Disposable.t =
    "onDidRenameFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidSaveTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidSaveTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillCreateFiles:
    (option(FileWillCreateEvent.t) => unit) => Disposable.t =
    "onWillCreateFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillDeleteFiles:
    (option(FileWillDeleteEvent.t) => unit) => Disposable.t =
    "onWillDeleteFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillRenameFiles:
    (option(FileWillRenameEvent.t) => unit) => Disposable.t =
    "onWillRenameFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillSaveTextDocument:
    (option(TextDocumentWillSaveEvent.t) => unit) => Disposable.t =
    "onWillSaveTextDocument";
  // functions
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external applyEdit: WorkspaceEdit.t => Promise.t(bool) = "applyEdit";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external asRelativePath: (string, option(bool)) => string =
    "asRelativePath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external asRelativePathWithUri: (Uri.t, option(bool)) => string =
    "asRelativePath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external createFileSystemWatcher:
    (
      GlobPattern.t,
      ~ignoreCreateEvents: bool=?,
      ~ignoreChangeEvents: bool=?,
      ~ignoreDeleteEvents: bool=?
    ) =>
    FileSystemWatcher.t =
    "createFileSystemWatcher";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external findFiles:
    (
      GlobPattern.t,
      ~exclude: Js.nullable(GlobPattern.t)=?,
      ~token: CancellationToken.t=?
    ) =>
    Promise.t(array(Uri.t)) =
    "findFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external getConfiguration:
    (option(string), option(Uri.t)) => WorkspaceConfiguration.t =
    "getConfiguration";
  external getConfigurationOfTextDocument:
    (option(string), option(TextDocument.t)) => WorkspaceConfiguration.t =
    "getConfiguration";
  external getConfigurationOfWorkspaceFolder:
    (option(string), option(WorkspaceFolder.t)) => WorkspaceConfiguration.t =
    "getConfiguration";
  external getConfigurationOfLanguage:
    (
      option(string),
      option({
        .
        "languageId": string,
        "uri": Uri.t,
      })
    ) =>
    WorkspaceConfiguration.t =
    "getConfiguration";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external getWorkspaceFolder: Uri.t => option(WorkspaceFolder.t) =
    "getWorkspaceFolder";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocument: Uri.t => Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocumentWithFileName: string => Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocumentWithOptions:
    option({
      .
      "content": string,
      "language": string,
    }) =>
    Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerFileSystemProvider:
    (
      string,
      FileSystemProvider.t,
      option({
        .
        "isCaseSensitive": bool,
        "isReadonly": bool,
      })
    ) =>
    Disposable.t =
    "registerFileSystemProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerTaskProvider: (string, TaskProvider.t) => Disposable.t =
    "registerTaskProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerTextDocumentContentProvider:
    (string, TextDocumentContentProvider.t) => Disposable.t =
    "registerTextDocumentContentProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external saveAll: option(bool) => Promise.t(bool) = "saveAll";
  [@bs.module "vscode"] [@bs.scope "workspace"] [@bs.variadic]
  external updateWorkspaceFolders:
    (
      int,
      option(int),
      array({
        .
        "name": string,
        "uri": Uri.t,
      })
    ) =>
    Promise.t(bool) =
    "updateWorkspaceFolders";
};