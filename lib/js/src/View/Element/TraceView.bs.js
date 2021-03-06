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
          var before = Belt_Array.concatMany([
                [{
                    TAG: /* Text */1,
                    _0: "before: ",
                    _1: []
                  }],
                param.before
              ]);
          var mapping = Belt_Array.concatMany([
                [{
                    TAG: /* Text */1,
                    _0: "mapping: ",
                    _1: []
                  }],
                param.mapping
              ]);
          var after = Belt_Array.concatMany([
                [{
                    TAG: /* Text */1,
                    _0: "after: ",
                    _1: []
                  }],
                param.after
              ]);
          var inlines = [{
              TAG: /* Vert */7,
              _0: [
                line,
                before,
                mapping,
                after
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
