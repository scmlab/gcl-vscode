open GCL.Syntax;

open! Response.Error;

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module StructError = {
    open Response.Error.StructError;
    let handle = (id, site) =>
      fun
      | MissingBound => [
          Task.MarkError(site),
          Display(
            Some(id),
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        ]
      | MissingAssertion => [
          MarkError(site),
          Display(
            Some(id),
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing"),
          ),
        ]
      | ExcessBound => [
          MarkError(site),
          Display(
            Some(id),
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion"),
          ),
        ]
      | MissingPostcondition => [
          MarkError(site),
          Display(
            Some(id),
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        ]
      | DigHole => [DigHole(site), DispatchCommand(Reload)];
  };

  module StructError2 = {
    open Response.Error.StructError2;
    let handle = (id, site) =>
      fun
      | MissingBound => [
          Task.MarkError(site),
          Display(
            Some(id),
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        ]
      | MissingLoopInvariant => [
          MarkError(site),
          Display(
            Some(id),
            Error("Loop Invariant Missing"),
            Plain("Loop invariant before the DO construct is missing"),
          ),
        ]
      | MissingPrecondition => [
          MarkError(site),
          Display(
            Some(id),
            Error("Precondition Missing"),
            Plain(
              "The first statement of the program should be an assertion",
            ),
          ),
        ]
      | MissingPostcondition => [
          MarkError(site),
          Display(
            Some(id),
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        ]
      | PreconditionUnknown => [
          MarkError(site),
          Display(Some(id), Error("Precondition Unknown"), Plain("")),
        ]
      | DigHole => [DigHole(site), DispatchCommand(Reload)];
  };
  let handle = (id, error) => {
    let Error(site, kind) = error;
    switch (kind) {
    | LexicalError => [
        Task.MarkError(site),
        Display(
          Some(id),
          Error("Lexical Error"),
          Plain(Response.Error.Site.toString(site)),
        ),
      ]
    | SyntacticError(messages) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Parse Error"),
          Plain(messages->Js.String.concatMany("\n")),
        ),
      ]
    | StructError(error) => StructError.handle(id, site, error)
    | StructError2(error) => StructError2.handle(id, site, error)
    | TypeError(NotInScope(name)) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Type Error"),
          Plain("The definition " ++ name ++ " is not in scope"),
        ),
      ]
    | TypeError(UnifyFailed(s, t)) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Type Error"),
          Plain(
            "Cannot unify: "
            ++ Type.toString(s)
            ++ "\nwith        : "
            ++ Type.toString(t),
          ),
        ),
      ]
    | TypeError(RecursiveType(var, t)) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Type Error"),
          Plain(
            "Recursive type variable: "
            ++ Type.toString(Type.Var(var))
            ++ "\n"
            ++ "in type             : "
            ++ Type.toString(t),
          ),
        ),
      ]
    | TypeError(NotFunction(t)) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Type Error"),
          Plain(
            "The type " ++ Type.toString(t) ++ " is not a function type",
          ),
        ),
      ]
    | CannotDecodeRequest(req) => [
        MarkError(site),
        Display(Some(id), Error("Cannot Decode Request"), Plain(req)),
      ]
    | CannotReadFile(path) => [
        MarkError(site),
        Display(
          Some(id),
          Error("Cannot Read File"),
          Plain("Cannot read file of path: " ++ path),
        ),
      ]
    | NotLoaded => [
        MarkError(site),
        Display(
          Some(id),
          Error("Not Loaded"),
          Plain("Please load the file first"),
        ),
      ]
    };
  };
};