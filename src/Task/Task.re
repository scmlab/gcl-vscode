type t =
  | WithState(State.t => Promise.t(list(t)))
  | DispatchCommand(Command.t)
  // GCL
  | Connect
  | SendRequest(Request.t)
  // View related
  | Display(ViewType.Request.Header.t, ViewType.Request.Body.t)
  | ViewRequest(ViewType.Request.t)
  | ViewResponse(ViewType.Response.t)
  // Decorations
  | MarkError(Response.Error.Site.t)
  | MarkSpec(Response.Specification.t)
  | DigHole(Response.Error.Site.t)
  | RemoveDecorations;
