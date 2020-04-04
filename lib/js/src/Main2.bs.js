// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Main$Guacamole = require("./Main.bs.js");
var State$Guacamole = require("./State.bs.js");
var VscodeImpl$Guacamole = require("./Editor/VscodeImpl.bs.js");

var partial_arg = {
  getExtensionPath: VscodeImpl$Guacamole.getExtensionPath,
  getFileName: VscodeImpl$Guacamole.getFileName,
  onDidChangeFileName: VscodeImpl$Guacamole.onDidChangeFileName,
  onDidChangeActivation: VscodeImpl$Guacamole.onDidChangeActivation,
  onDidCloseEditor: VscodeImpl$Guacamole.onDidCloseEditor,
  registerCommand: VscodeImpl$Guacamole.registerCommand,
  getGCLPath: VscodeImpl$Guacamole.getGCLPath,
  setGCLPath: VscodeImpl$Guacamole.setGCLPath,
  addToSubscriptions: VscodeImpl$Guacamole.addToSubscriptions,
  createView: VscodeImpl$Guacamole.createView,
  destroyView: VscodeImpl$Guacamole.destroyView
};

var partial_arg$1 = Main$Guacamole.Impl;

var include = (function (param) {
      return partial_arg$1(partial_arg, param);
    })(State$Guacamole.Impl);

var States = include.States;

var State = include.State;

var isGCL = include.isGCL;

var addToSubscriptions = include.addToSubscriptions;

var activate = include.activate;

var deactivate = include.deactivate;

exports.States = States;
exports.State = State;
exports.isGCL = isGCL;
exports.addToSubscriptions = addToSubscriptions;
exports.activate = activate;
exports.deactivate = deactivate;
/* include Not a pure module */