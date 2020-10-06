open GCL.Syntax;

open! Response.Error;

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module StructError = {
    open Response.Error.StructError;
    let handle = site =>
      fun
      | MissingBound => [
          Task.MarkError(site),
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            )
          ),
        ]
      | MissingAssertion => [
          MarkError(site),
          Display(
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing")
          ),
        ]
      | ExcessBound => [
          MarkError(site),
          Display(
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion")
          ),
        ]
      | MissingPostcondition => [
          MarkError(site),
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion")
          ),
        ]
      | DigHole => [DigHole(site), DispatchCommand(Reload)];
  };

  module StructError2 = {
    open Response.Error.StructError2;
    let handle = site =>
      fun
      | MissingBound => [
          Task.MarkError(site),
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            )
          ),
        ]
      | MissingLoopInvariant => [
          MarkError(site),
          Display(
            Error("Loop Invariant Missing"),
            Plain("Loop invariant before the DO construct is missing")
          ),
        ]
      | MissingPrecondition => [
          MarkError(site),
          Display(
            Error("Precondition Missing"),
            Plain(
              "The first statement of the program should be an assertion"
            )
          ),
        ]
      | MissingPostcondition => [
          MarkError(site),
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion")
          ),
        ]
      | PreconditionUnknown => [
          MarkError(site),
          Display(Error("Precondition Unknown"), Plain("")),
        ]
      | DigHole => [DigHole(site), DispatchCommand(Reload)];
  };
  let handle = error => {
    let Error(site, kind) = error;
    switch (kind) {
    | LexicalError => [
        Task.MarkError(site),
        Display(
          Error("Lexical Error"),
          Plain(Response.Error.Site.toString(site))
        ),
      ]
    | SyntacticError(messages) => [
        MarkError(site),
        Display(
          Error("Parse Error"),
          Plain(messages->Js.String.concatMany("\n"))
        ),
      ]
    | StructError(error) => StructError.handle(site, error)
    | StructError2(error) => StructError2.handle(site, error)
    | TypeError(NotInScope(name)) => [
        MarkError(site),
        Display(
          Error("Type Error"),
          Plain("The definition " ++ name ++ " is not in scope")
        ),
      ]
    | TypeError(UnifyFailed(s, t)) => [
        MarkError(site),
        Display(
          Error("Type Error"),
          Plain(
            "Cannot unify: "
            ++ Type.toString(s)
            ++ "\nwith        : "
            ++ Type.toString(t),
          )
        ),
      ]
    | TypeError(RecursiveType(var, t)) => [
        MarkError(site),
        Display(
          Error("Type Error"),
          Plain(
            "Recursive type variable: "
            ++ Type.toString(Type.Var(var))
            ++ "\n"
            ++ "in type             : "
            ++ Type.toString(t),
          )
        ),
      ]
    | TypeError(NotFunction(t)) => [
        MarkError(site),
        Display(
          Error("Type Error"),
          Plain(
            "The type " ++ Type.toString(t) ++ " is not a function type",
          )
        ),
      ]
    | CannotDecodeRequest(req) => [
        MarkError(site),
        Display(Error("Cannot Decode Request"), Plain(req)),
      ]
    | CannotReadFile(path) => [
        MarkError(site),
        Display(
          Error("Cannot Read File"),
          Plain("Cannot read file of path: " ++ path)
        ),
      ]
    | NotLoaded => [
        MarkError(site),
        Display(Error("Not Loaded"), Plain("Please load the file first")), 
      ]
    };
  };
};