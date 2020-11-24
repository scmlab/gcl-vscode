open React

@react.component
let make = (~header: ViewType.Request.Header.t) => {
  open! ViewType.Request.Header

  <div className="gcl-header">
    {switch header {
    | Loading => <h2 className="text-plain"> {string("Loading ...")} </h2>
    | Plain(s) => <h2> {string(s)} </h2>
    | Error(s) => <h2 className="text-error"> {string(s)} </h2>
    }}
  </div>
}
