// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Element__Inlines$Guabao = require("./Element__Inlines.bs.js");

function TraceView(Props) {
  var hidden = Props.hidden;
  var trace = Props.trace;
  var className = "element-block-code-trace" + (
    hidden ? " hidden" : ""
  );
  var traceView = Belt_Array.mapWithIndex(trace, (function (i, param) {
          var line = Belt_Array.concatMany([[{
                    TAG: /* Text */1,
                    _0: "---------",
                    _1: []
                  }]]);
          var expr = Belt_Array.concatMany([
                [{
                    TAG: /* Text */1,
                    _0: "expr: ",
                    _1: []
                  }],
                param.expr
              ]);
          var inlines = [{
              TAG: /* Vert */6,
              _0: [
                line,
                expr
              ]
            }];
          return React.createElement(Element__Inlines$Guabao.make, {
                      value: /* Element */{
                        _0: inlines
                      },
                      key: String(i)
                    });
        }));
  return React.createElement("div", {
              className: className
            }, "DEBUG: displaying the whole history (" + String(trace.length) + " steps)", traceView);
}

var Inlines;

var make = TraceView;

exports.Inlines = Inlines;
exports.make = make;
/* react Not a pure module */
