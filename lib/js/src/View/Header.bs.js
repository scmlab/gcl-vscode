// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");

function Header(Props) {
  var header = Props.header;
  var editorType = Props.editorType;
  var mode = Props.mode;
  var onChangeMode = Props.onChangeMode;
  var onChange = function (param) {
    return Curry._1(onChangeMode, mode ? /* WP1 */0 : /* WP2 */1);
  };
  var modeToggle = editorType ? React.createElement("button", {
          className: "gcl-mode-vscode",
          onClick: onChange
        }, mode ? "WP2" : "WP") : React.createElement("div", {
          className: "gcl-mode-atom"
        }, React.createElement("label", {
              className: "input-label"
            }, React.createElement("input", {
                  className: "input-toggle",
                  checked: mode ? true : false,
                  type: "checkbox",
                  onChange: onChange
                }), "WP2"));
  var tmp;
  tmp = typeof header === "number" ? React.createElement("div", {
          className: "text-plain"
        }, "Loading ...") : (
      header.tag ? React.createElement("div", {
              className: "text-error"
            }, header[0]) : React.createElement("div", undefined, header[0])
    );
  return React.createElement("h2", {
              className: "gcl-header"
            }, tmp, modeToggle);
}

var make = Header;

exports.make = make;
/* react Not a pure module */