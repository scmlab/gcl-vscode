open Belt;

open! Response;

// from GCL response to Task
let handle = (response): list(Task.t) => {
  switch (response) {
  | Error(errors) =>
    errors->Array.map(Task__Error.handle)->List.fromArray->Js.List.flatten
  | OK(id, obligations, specifications, globalProps) =>
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
          ProofObligations(id, obligations, globalProps),
        ),
      ],
    )
  | Resolve(i) => [
      WithState(
        state => {State.Spec.resolve(state, i)->Promise.map(_ => [])},
      ),
      DispatchCommand(Reload),
    ]
  | Substitute(i, expr) => [ViewRequest(Substitute(i, expr))]
  | UnknownResponse(json) => [
      Display(
        Error("Panic: unknown response from GCL"),
        Plain(Js.Json.stringify(json)),
      ),
    ]
  };
};
