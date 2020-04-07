module Request = {
  type header =
    | Loading
    | Plain(string)
    | Error(string);

  type body =
    | Nothing
    | ProofObligations(array(GCL.Response.ProofObligation.t))
    | Plain(string);

  type t =
    | Show
    | Hide
    | Display(header, body);
};

type message =
  | Display(string, string);

module Response = {
  type mode =
    | WP1
    | WP2;

  type linkEvent =
    | MouseOver(GCL.loc)
    | MouseOut(GCL.loc)
    | MouseClick(GCL.loc);

  type t =
    | SetMode(mode)
    | Link(linkEvent);
};