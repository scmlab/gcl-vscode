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
      <li className="gcl-body-item native-key-bindings" tabIndex={-1}>
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
    <li className="gcl-body-item native-key-bindings" tabIndex={-1}> <Expr value=payload /> </li>
}

@react.component
let make = (~body: ViewType.Request.Body.t) =>
  switch body {
  | Nothing => <> </>
  | ProofObligations(id, pos, globalProps) =>
    let pos =
      pos
      ->Array.mapWithIndex((i, payload) => <ProofObligation payload key={string_of_int(i)} />)
      ->React.array
    let globalPropHeader = if Array.length(globalProps) !== 0 {
      <h2> {string("Global properties")} </h2>
    } else {
      <> </>
    }
    let globalProps =
      globalProps
      ->Array.mapWithIndex((i, payload) => <GlobalProp payload key={string_of_int(i)} />)
      ->React.array
    <ReqID.Provider value=Some(id)>
      <div className="gcl-body">
        <ul className="gcl-proof-obligation-list"> pos </ul>
        globalPropHeader
        <ul className="gcl-global-property-list"> globalProps </ul>
      </div>
    </ReqID.Provider>
  | Plain(s) =>
    let paragraphs =
      Js.String.split("\n", s)
      ->Array.keep(x => x !== "")
      ->Array.mapWithIndex((i, s) => <p key={string_of_int(i)}> {string(s)} </p>)
      ->React.array
    <div className="gcl-body">
      <div className="gcl-plain-text gcl-body-item"> paragraphs </div>
    </div>
  }
