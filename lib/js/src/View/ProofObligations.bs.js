// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Expr$Guacamole = require("./Expr.bs.js");
var Link$Guacamole = require("./Link.bs.js");
var Pred$Guacamole = require("./Pred.bs.js");
var ReqID$Guacamole = require("./ReqID.bs.js");
var Response$Guacamole = require("../Response.bs.js");

function ProofObligations$ProofObligation(Props) {
  var payload = Props.payload;
  var o = payload._3;
  var origin = React.createElement(Link$Guacamole.make, {
        loc: Response$Guacamole.Origin.locOf(o),
        children: Response$Guacamole.Origin.toString(o)
      });
  return React.createElement("li", {
              className: "gcl-list-item native-key-bindings",
              tabIndex: -1
            }, React.createElement("span", {
                  className: "gcl-proof-obligation-message"
                }, origin), React.createElement("span", {
                  className: "gcl-proof-obligation-antecedent"
                }, React.createElement(Pred$Guacamole.make, {
                      value: payload._1
                    })), React.createElement("span", {
                  className: "gcl-proof-obligation-arrow"
                }, "⇒"), React.createElement("span", {
                  className: "gcl-proof-obligation-consequent"
                }, React.createElement(Pred$Guacamole.make, {
                      value: payload._2
                    })));
}

var ProofObligation = {
  make: ProofObligations$ProofObligation
};

function ProofObligations$GlobalProp(Props) {
  var payload = Props.payload;
  return React.createElement("li", {
              className: "gcl-list-item native-key-bindings",
              tabIndex: -1
            }, React.createElement(Expr$Guacamole.make, {
                  value: payload
                }));
}

var GlobalProp = {
  make: ProofObligations$GlobalProp
};

function ProofObligations(Props) {
  var id = Props.id;
  var pos = Props.pos;
  var onExport = Props.onExport;
  var pos$1 = Belt_Array.mapWithIndex(pos, (function (i, payload) {
          return React.createElement(ProofObligations$ProofObligation, {
                      payload: payload,
                      key: String(i)
                    });
        }));
  var onClick = function (param) {
    return Curry._1(onExport, undefined);
  };
  return React.createElement(ReqID$Guacamole.Provider.make, ReqID$Guacamole.Provider.makeProps(id, React.createElement("div", {
                      className: "gcl-proof-obligation"
                    }, React.createElement("h2", {
                          className: "gcl-proof-obligation-header"
                        }, "Proof Obligations", React.createElement("button", {
                              onClick: onClick
                            }, React.createElement("div", {
                                  className: "icon"
                                }, "export", React.createElement("i", {
                                      className: "codicon right codicon-export"
                                    })))), React.createElement("ul", {
                          className: "gcl-proof-obligation-list"
                        }, pos$1)), undefined));
}

var make = ProofObligations;

exports.ProofObligation = ProofObligation;
exports.GlobalProp = GlobalProp;
exports.make = make;
/* react Not a pure module */