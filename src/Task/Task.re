module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  type t =
    | WithState(State.t => Promise.t(list(t)))
    | DispatchCommand(Command.t)
    // GCL
    | Connect
    | SendRequest(Request.t)
    // View related
    | Display(option(int), View.Request.Header.t, View.Request.Body.t)
    | ViewRequest(option(int), View.Request.t)
    | ViewResponse(View.Response.t)
    // Decorations
    | MarkError(Response.Error.Site.t)
    | MarkSpec(Response.Specification.t)
    | DigHole(Response.Error.Site.t)
    | RemoveDecorations;
};