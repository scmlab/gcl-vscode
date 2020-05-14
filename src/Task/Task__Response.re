open Belt;

open! Response;

module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module Task__Error = Task__Error.Impl(Editor);
  // from GCL response to Task
  let handle = (response): list(Task.t) => {
    switch (response) {
    | Error(errors) =>
      errors->Array.map(Task__Error.handle)->List.fromArray->Js.List.flatten
    | OK(obligations, specifications, _) =>
      List.concat(
        specifications->List.fromArray->List.map(spec => Task.MarkSpec(spec)),
        [
          WithState(
            state => {
              state.specifications = specifications;
              Promise.resolved([]);
            },
          ),
          Display(
            Plain("Proof Obligations"),
            ProofObligations(obligations),
          ),
        ],
      )
    | Resolve(i) => [
        WithState(
          state => {
            let%P _ = State.Spec.resolve(state, i);
            Promise.resolved([]);
          },
        ),
        DispatchCommand(Reload),
      ]
    | Substitute(expr) =>
      Js.log("SUBST");
      [];
    | InsertAssertion(i, expr) => [
        WithState(
          state => {State.Spec.insert(state, i, expr)->Promise.map(_ => [])},
        ),
        DispatchCommand(Reload),
      ]
    | UnknownResponse(json) => [
        Display(
          Error("Panic: unknown response from GCL"),
          Plain(Js.Json.stringify(json)),
        ),
      ]
    };
  };
};