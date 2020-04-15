open Guacamole.GCL.Syntax;

open! GCL.Response.Error;

module Impl = (Editor: Sig.Editor) => {
  module State = Impl__State.Impl(Editor);
  module Task__Types = Task__Types.Impl(Editor);
  module StructError = {
    open Guacamole.GCL.Response.Error.StructError;
    let handle = _site =>
      fun
      | MissingBound => [
          Task__Types.AddDecorations([||]),
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        ]
      | MissingAssertion => [
          AddDecorations([||]),
          // AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing"),
          ),
        ]
      | MissingLoopInvariant => [
          AddDecorations([||]),
          // AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Loop Invariant Missing"),
            Plain("Loop invariant before the DO construct is missing"),
          ),
        ]
      | ExcessBound => [
          AddDecorations([||]),
          // AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion"),
          ),
        ]
      | MissingPrecondition => [
          Display(
            Error("Precondition Missing"),
            Plain(
              "The first statement of the program should be an assertion",
            ),
          ),
        ]
      | MissingPostcondition => [
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        ]
      | DigHole => []; // WithState(
    //   state => {
    //     let%P _ = state |> Spec.digHole(site);
    //     switch (state.history) {
    //     | Some(Types.Request.Refine(_)) =>
    //       Promise.resolved([
    //         DispatchCommand(Save),
    //         DispatchCommand(Refine),
    //       ])
    //     | _ => Promise.resolved([DispatchCommand(Save)])
    //     };
    //   },
    // ),
  };

  let handle = error => {
    let Error(site, kind) = error;
    switch (kind) {
    | LexicalError => [
        Task__Types.AddDecorations([||]),
        // Task__Types.AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Lexical Error"),
          Plain(Guacamole.GCL.Response.Error.Site.toString(site)),
        ),
      ]
    | SyntacticError(messages) => [
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Parse Error"),
          Plain(messages->Js.String.concatMany("\n")),
        ),
      ]
    | StructError(error) => StructError.handle(site, error)
    | TypeError(NotInScope(name)) => [
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain("The definition " ++ name ++ " is not in scope"),
        ),
      ]
    | TypeError(UnifyFailed(s, t)) => [
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
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
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
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
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain(
            "The type " ++ Type.toString(t) ++ " is not a function type",
          ),
        ),
      ]
    | CannotReadFile(path) => [
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Cannot Read File"),
          Plain("Cannot read file of path: " ++ path),
        ),
      ]
    | NotLoaded => [
        AddDecorations([||]),
        // AddDecorations(Decoration.markSite(site)),
        Display(Error("Not Loaded"), Plain("Please load the file first")),
      ]
    };
  };
};