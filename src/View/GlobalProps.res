// open Type.View;
open Belt
open React

open Response

module GlobalProp = {
  @react.component
  let make = (~payload: GlobalProp.t) =>
    <li className="gcl-list-item native-key-bindings" tabIndex={-1}> <Expr value=payload /> </li>
}

@react.component
let make = (~id, ~props: array<Response.GlobalProp.t>) => {
  let props =
    props
    ->Array.mapWithIndex((i, payload) => <GlobalProp payload key={string_of_int(i)} />)
    ->React.array

  <ReqID.Provider value=Some(id)>
    <div className="gcl-global-props">
      <h2> {string("Global Properties")} </h2> <ul className="gcl-global-property-list"> props </ul>
    </div>
  </ReqID.Provider>
}
