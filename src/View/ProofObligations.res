// open Type.View;
open Belt
open React

open Response

module ProofObligation = {
  @react.component
  let make = (~payload: ProofObligation.t) =>
    switch payload {
    | ProofObligation(_, p, q, o) =>
      let origin = <Link loc={Origin.locOf(o)}> {string(Origin.toString(o))} </Link>
      <li className="gcl-list-item native-key-bindings" tabIndex={-1}>
        <span className="gcl-proof-obligation-message"> origin </span>
        <span className="gcl-proof-obligation-antecedent"> <Pred value=p /> </span>
        <span className="gcl-proof-obligation-arrow"> {string(j`⇒`)} </span>
        <span className="gcl-proof-obligation-consequent"> <Pred value=q /> </span>
      </li>
    }
}

module GlobalProp = {
  @react.component
  let make = (~payload: GlobalProp.t) =>
    <li className="gcl-list-item native-key-bindings" tabIndex={-1}> <Expr value=payload /> </li>
}

@react.component
let make = (~id, ~pos: array<Response.ProofObligation.t>) => {
  let pos =
    pos
    ->Array.mapWithIndex((i, payload) => <ProofObligation payload key={string_of_int(i)} />)
    ->React.array

  <ReqID.Provider value=Some(id)>
    <div className="gcl-proof-obligation">
      <h2 className="gcl-proof-obligation-header"> {string("Proof Obligations")} </h2>
      <ul className="gcl-proof-obligation-list"> pos </ul>
    </div>
  </ReqID.Provider>
}
