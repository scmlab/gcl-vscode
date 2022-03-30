// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var React = require("react");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Chan$Guabao = require("../Util/Chan.bs.js");

var $$Event = {};

var emitter = Chan$Guabao.make(undefined);

var context = React.createContext(emitter);

function makeProps(value, children, param) {
  return {
          value: value,
          children: children
        };
}

var make = context.Provider;

var Provider = {
  makeProps: makeProps,
  make: make
};

function Substitution(Props) {
  var makeInline = Props.makeInline;
  var id = Props.id;
  var expr = Props.expr;
  var onSubst = Props.onSubst;
  var match = React.useState(function () {
        return /* Unreduced */0;
      });
  var setStatus = match[1];
  var status = match[0];
  var match$1 = React.useState(function () {
        return false;
      });
  var setHoverSubstitutee = match$1[1];
  var undo = function (param) {
    return Curry._1(setStatus, (function (param) {
                  return /* Unreduced */0;
                }));
  };
  var channel = React.useContext(context);
  React.useEffect((function () {
          return Chan$Guabao.on(channel, (function ($$event) {
                        if ($$event.TAG === /* SubstReq */0) {
                          return ;
                        }
                        if ($$event._0 !== id) {
                          return ;
                        }
                        var result = $$event._1;
                        return Curry._1(setStatus, (function (param) {
                                      return /* Reduced */{
                                              _0: result
                                            };
                                    }));
                      }));
        }), []);
  var onClick = function (ev) {
    Chan$Guabao.emit(channel, {
          TAG: /* SubstReq */0,
          _0: id
        });
    Curry._1(setHoverSubstitutee, (function (param) {
            return false;
          }));
    Curry._1(setStatus, (function (param) {
            return /* Reducing */1;
          }));
    Belt_Option.forEach(onSubst, (function (onSubst) {
            return Curry._1(onSubst, {
                        expr: expr,
                        undo: undo
                      });
          }));
    ev.stopPropagation();
    
  };
  var onMouseOver = function (ev) {
    Curry._1(setHoverSubstitutee, (function (param) {
            return true;
          }));
    ev.stopPropagation();
    
  };
  var onMouseOut = function (ev) {
    Curry._1(setHoverSubstitutee, (function (param) {
            return false;
          }));
    ev.stopPropagation();
    
  };
  if (typeof status === "number") {
    var expr$1 = Curry._2(makeInline, /* Element */{
          _0: expr
        }, onSubst);
    var className = match$1[0] ? "element-sbst element-sbst-hovered" : "element-sbst";
    return React.createElement("span", {
                className: className,
                onClick: onClick,
                onMouseOut: onMouseOut,
                onMouseOver: onMouseOver
              }, expr$1);
  }
  var result = Curry._2(makeInline, status._0, onSubst);
  return React.createElement("span", {
              className: "element-sbst"
            }, result);
}

var Inlines;

var make$1 = Substitution;

exports.Inlines = Inlines;
exports.$$Event = $$Event;
exports.emitter = emitter;
exports.context = context;
exports.Provider = Provider;
exports.make = make$1;
/* emitter Not a pure module */