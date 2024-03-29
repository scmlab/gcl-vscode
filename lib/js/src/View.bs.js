// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Path = require("path");
var Curry = require("rescript/lib/js/curry.js");
var VSCode = require("rescript-vscode/lib/js/src/VSCode.bs.js");
var Vscode = require("vscode");
var Js_math = require("rescript/lib/js/js_math.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Chan$Guabao = require("./Util/Chan.bs.js");
var ViewType$Guabao = require("./View/ViewType.bs.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

function makeHTML(webview, extensionPath) {
  var extensionUri = Vscode.Uri.file(extensionPath);
  var text = "";
  var charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  var cardinality = charaterSet.length;
  for(var _for = 0; _for <= 32; ++_for){
    text = text + charaterSet.charAt(Js_math.floor(Math.random() * cardinality));
  }
  var nonce = text;
  var scriptUri = webview.asWebviewUri(Vscode.Uri.joinPath(extensionUri, "dist", "bundled-view.js")).toString();
  var cspSourceUri = webview.cspSource;
  var styleUri = webview.asWebviewUri(Vscode.Uri.joinPath(extensionUri, "dist", "style.css")).toString();
  var codiconsUri = webview.asWebviewUri(Vscode.Uri.joinPath(extensionUri, "dist", "codicon/codicon.css")).toString();
  var scriptSrc = "script-src 'nonce-" + nonce + "'; ";
  var styleSrc = "style-src " + cspSourceUri + "; ";
  var fontSrc = "font-src " + cspSourceUri + "; ";
  var scp = "default-src 'none'; " + fontSrc + scriptSrc + styleSrc;
  return "\n      <!DOCTYPE html>\n      <html lang=\"en\">\n      <head>\n        <meta charset=\"utf-8\">\n        <meta name=\"viewport\" content=\"width=device-width,initial-scale=1,shrink-to-fit=no\">\n        <meta name=\"theme-color\" content=\"#000000\">\n\n        <!--\n					Use a content security policy to only allow loading images from https or from our extension directory,\n					and only allow scripts that have a specific nonce.\n				-->\n        <meta http-equiv=\"Content-Security-Policy\" content=\"" + scp + "\">\n\n        <title>React App</title>\n        <link href=\"" + styleUri + "\"    rel=\"stylesheet\" type=\"text/css\" >\n        <link href=\"" + codiconsUri + "\" rel=\"stylesheet\" />\n      </head>\n      <body>\n        <noscript>You need to enable JavaScript to run this app.</noscript>\n        <div id=\"root\"></div>\n        <script nonce=\"" + nonce + "\" src=\"" + scriptUri + "\"></script>\n      </body>\n      </html>\n    ";
}

function make(title, extensionPath) {
  var distPath = Path.join(extensionPath, "dist");
  var panel = Vscode.window.createWebviewPanel("panel", title, {
        preserveFocus: true,
        viewColumn: 3
      }, VSCode.WebviewAndWebviewPanelOptions.make(undefined, true, [Vscode.Uri.file(distPath)], undefined, undefined, true, undefined));
  var html = makeHTML(panel.webview, extensionPath);
  panel.webview.html = html;
  return panel;
}

function destroy(prim) {
  prim.dispose();
  
}

function send(panel, message) {
  return panel.webview.postMessage(message);
}

function recv(panel, callback) {
  return panel.webview.onDidReceiveMessage(callback);
}

function onDestroyed(panel, callback) {
  return panel.onDidDispose(callback);
}

function reveal(panel) {
  return VSCode.WebviewPanel.reveal(panel, undefined, true, undefined);
}

function focus(panel) {
  return VSCode.WebviewPanel.reveal(panel, undefined, undefined, undefined);
}

var Panel = {
  make: make,
  destroy: destroy,
  send: send,
  recv: recv,
  onDestroyed: onDestroyed,
  reveal: reveal,
  focus: focus
};

function send$1(view, req) {
  var queued = view.status;
  if (queued) {
    queued._0.push(req);
    return $$Promise.resolved(false);
  }
  var stringified = JSON.stringify(ViewType$Guabao.$$Request.encode(req));
  return view.panel.webview.postMessage(stringified);
}

function on(view, callback) {
  return new Vscode.Disposable(Chan$Guabao.on(view.onResponse, callback));
}

function make$1(extensionPath) {
  var view = {
    panel: make("Guabao", extensionPath),
    onResponse: Chan$Guabao.make(undefined),
    subscriptions: [],
    status: /* Uninitialized */{
      _0: []
    }
  };
  view.subscriptions.push(view.panel.webview.onDidReceiveMessage(function (json) {
            var result;
            try {
              result = Curry._1(ViewType$Guabao.$$Response.decode, json);
            }
            catch (raw_e){
              var e = Caml_js_exceptions.internalToOCamlException(raw_e);
              console.log("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e);
              return ;
            }
            return Chan$Guabao.emit(view.onResponse, result);
          }));
  view.subscriptions.push(view.panel.onDidDispose(function (param) {
            return Chan$Guabao.emit(view.onResponse, /* Destroyed */1);
          }));
  var match = $$Promise.pending(undefined);
  var resolve = match[1];
  view.subscriptions.push(new Vscode.Disposable(Chan$Guabao.on(view.onResponse, (function (x) {
                  if (x !== 0) {
                    return ;
                  }
                  var queued = view.status;
                  if (queued) {
                    view.status = /* Initialized */0;
                    Curry._1(resolve, view);
                    return Belt_Array.forEach(queued._0, (function (req) {
                                  send$1(view, req);
                                  
                                }));
                  }
                  
                }))));
  return match[0];
}

function destroy$1(view) {
  Chan$Guabao.destroy(view.onResponse);
  view.panel.dispose();
  return Belt_Array.forEach(view.subscriptions, (function (prim) {
                return prim.dispose();
              }));
}

function onceDestroyed(view) {
  var match = $$Promise.pending(undefined);
  var resolve = match[1];
  var disposable = Chan$Guabao.on(view.onResponse, (function (response) {
          if (typeof response === "number" && response !== 0) {
            return Curry._1(resolve, undefined);
          }
          
        }));
  return $$Promise.tap(match[0], disposable);
}

function reveal$1(view) {
  return reveal(view.panel);
}

function focus$1(view) {
  return focus(view.panel);
}

var Instance = {
  make: make$1,
  destroy: destroy$1,
  send: send$1,
  on: on,
  onceDestroyed: onceDestroyed,
  reveal: reveal$1,
  focus: focus$1
};

var singleton = {
  contents: undefined
};

var listenersBeforeAcvitation = [];

function send$2(req) {
  var view = singleton.contents;
  if (view !== undefined) {
    return send$1(Caml_option.valFromOption(view), req);
  } else {
    return $$Promise.resolved(false);
  }
}

function on$1(callback) {
  var view = singleton.contents;
  if (view !== undefined) {
    return $$Promise.resolved(on(Caml_option.valFromOption(view), callback));
  }
  var match = $$Promise.pending(undefined);
  listenersBeforeAcvitation.push([
        callback,
        match[1]
      ]);
  return match[0];
}

function activate(extensionPath) {
  var match = singleton.contents;
  if (match !== undefined) {
    return $$Promise.resolved(undefined);
  } else {
    return $$Promise.map(make$1(extensionPath), (function (view) {
                  singleton.contents = Caml_option.some(view);
                  listenersBeforeAcvitation.forEach(function (param) {
                        return Curry._1(param[1], on(view, param[0]));
                      });
                  $$Promise.get(onceDestroyed(view), (function (param) {
                          singleton.contents = undefined;
                          
                        }));
                  console.log("[ view ] activated");
                  
                }));
  }
}

function deactivate(param) {
  var view = singleton.contents;
  if (view !== undefined) {
    destroy$1(Caml_option.valFromOption(view));
    singleton.contents = undefined;
    console.log("[ view ] deactivated");
    return ;
  }
  
}

function isActivated(param) {
  return Belt_Option.isSome(singleton.contents);
}

function focus$2(param) {
  var view = singleton.contents;
  if (view !== undefined) {
    return reveal(Caml_option.valFromOption(view).panel);
  }
  
}

var Singleton = {
  activate: activate,
  deactivate: deactivate,
  isActivated: isActivated,
  send: send$2,
  on: on$1,
  focus: focus$2
};

exports.Panel = Panel;
exports.Instance = Instance;
exports.Singleton = Singleton;
exports.activate = activate;
exports.deactivate = deactivate;
exports.isActivated = isActivated;
exports.send = send$2;
exports.on = on$1;
exports.focus = focus$2;
/* path Not a pure module */
