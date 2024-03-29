// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var React = require("react");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Json_decode = require("@glennsl/bs-json/lib/js/src/Json_decode.bs.js");
var Json_encode = require("@glennsl/bs-json/lib/js/src/Json_encode.bs.js");
var Element__Block$Guabao = require("./Element__Block.bs.js");

function toClassName(x) {
  switch (x) {
    case /* Plain */0 :
        return "";
    case /* Red */1 :
        return "element-deco-red";
    case /* Yellow */2 :
        return "element-deco-yellow";
    case /* Green */3 :
        return "element-deco-green";
    case /* Blue */4 :
        return "element-deco-blue";
    
  }
}

function decode(param) {
  return Json_decode.map((function (tag) {
                switch (tag) {
                  case "Blue" :
                      return /* Blue */4;
                  case "Green" :
                      return /* Green */3;
                  case "Red" :
                      return /* Red */1;
                  case "Yellow" :
                      return /* Yellow */2;
                  default:
                    return /* Plain */0;
                }
              }), Json_decode.string, param);
}

function encode(x) {
  switch (x) {
    case /* Plain */0 :
        return "Plain";
    case /* Red */1 :
        return "Red";
    case /* Yellow */2 :
        return "Yellow";
    case /* Green */3 :
        return "Green";
    case /* Blue */4 :
        return "Blue";
    
  }
}

var Deco = {
  toClassName: toClassName,
  decode: decode,
  encode: encode
};

function decode$1(param) {
  return Json_decode.map((function (param) {
                return {
                        deco: param[0],
                        blocks: param[1]
                      };
              }), (function (param) {
                return Json_decode.tuple2(decode, (function (param) {
                              return Json_decode.array(Element__Block$Guabao.decode, param);
                            }), param);
              }), param);
}

function encode$1(param) {
  return Json_encode.tuple2(encode, (function (param) {
                return Json_encode.array(Element__Block$Guabao.encode, param);
              }), [
              param.deco,
              param.blocks
            ]);
}

function Element__Section(Props) {
  var value = Props.value;
  var onInsertAnchor = Props.onInsertAnchor;
  var onClickSolveButton = Props.onClickSolveButton;
  var isPO = Belt_Option.isSome(Belt_Option.flatMap(Belt_Array.get(value.blocks, 0), (function (block) {
              if (block.TAG === /* HeaderWithButtons */1) {
                return Caml_option.some(undefined);
              }
              
            })));
  var match = React.useState(function () {
        return false;
      });
  var setDisplayExplanation = match[1];
  var displayExplanation = match[0];
  var onDisplayExplanation = function (x) {
    return Curry._1(setDisplayExplanation, (function (param) {
                  return x;
                }));
  };
  var className = "element-section " + toClassName(value.deco);
  var blocks = Belt_Array.mapWithIndex(Belt_Array.keepWithIndex(value.blocks, (function (param, index) {
              var isForExplanation = isPO && index === (value.blocks.length - 1 | 0);
              if (isForExplanation) {
                if (isForExplanation) {
                  return displayExplanation;
                } else {
                  return false;
                }
              } else {
                return true;
              }
            })), (function (index, block) {
          return React.createElement(Element__Block$Guabao.make, {
                      value: block,
                      onInsertAnchor: onInsertAnchor,
                      onClickSolveButton: onClickSolveButton,
                      onDisplayExplanation: onDisplayExplanation,
                      key: String(index)
                    });
        }));
  return React.createElement("li", {
              className: className
            }, blocks);
}

var Inlines;

var Block;

var make = Element__Section;

exports.Inlines = Inlines;
exports.Block = Block;
exports.Deco = Deco;
exports.decode = decode$1;
exports.encode = encode$1;
exports.make = make;
/* react Not a pure module */
