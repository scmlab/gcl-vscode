open Belt;

open! Guacamole.GCL.Response;

module Impl = (Editor: Sig.Editor) => {
  module State = Impl__State.Impl(Editor);
  module Task__Types = Task__Types.Impl(Editor);
  module Task__Error = Task__Error.Impl(Editor);
  // from GCL response to Task
  let handle = (response): list(Task__Types.t) => {
    switch (response) {
    | Error(errors) =>
      errors->Array.map(Task__Error.handle)->List.fromArray->Js.List.flatten
    | OK(obligations, specifications) =>
      List.concat(
        specifications
        ->List.fromArray
        ->List.map(spec => Task__Types.MarkSpec(spec)),
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
            Promise.resolved([Task__Types.DispatchCommand(Reload)]);
          },
        ),
      ]
    | InsertAssertion(_i, _expr) =>
      // WithState(
      //   state => {
      //     Spec.insert(i, expr, state);
      //     Promise.resolved([DispatchCommand(Save)]);
      //   },
      // ),
      []
    | UnknownResponse(json) => [
        Display(
          Error("Panic: unknown response from GCL"),
          Plain(Js.Json.stringify(json)),
        ),
      ]
    };
  };
};