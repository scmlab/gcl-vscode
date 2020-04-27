module Impl = (Editor: Sig.Editor) => {
  module State = Impl__State.Impl(Editor);
  type t =
    | WithState(State.t => Promise.t(list(t)))
    | Connect
    | MarkError(Response.Error.Site.t)
    | MarkSpec(Response.Specification.t)
    | DigHole(Response.Error.Site.t)
    | RemoveDecorations
    | DispatchCommand(Command.t)
    | SendRequest(Request.t)
    | Display(View.Request.Header.t, View.Request.Body.t);
};