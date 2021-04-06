open Belt
open React
open Common

@react.component
let make = (~onRequest: Chan.t<ViewType.Request.t>, ~onResponse: Chan.t<ViewType.Response.t>) => {
  let (connection, setConnection) = React.useState(_ => None)
  let ((id, pos, props, warnings), setDisplay) = React.useState(() => (0, [], [], []))
  let (errorMessages, setErrorMessages) = React.useState(_ => [])
  let onClickLink = React.useRef(Chan.make())
  let onSubstitute = React.useRef(Chan.make())

  // response with Initialized on mount
  React.useEffect1(() => {
    onResponse->Chan.emit(ViewType.Response.Initialized)
    None
  }, [])

  // for receiving requests from the extension
  React.useEffect1(() => {
    open ViewType.Request
    let destructor = onRequest->Chan.on(req =>
      switch req {
      | ViewType.Request.UpdateConnection(method) => setConnection(_ => method)
      | Display(id, pos, props, warnings) => setDisplay(_ => (id, pos, props, warnings))
      | SetErrorMessages(msgs) => setErrorMessages(_ => msgs)
      | Substitute(i, expr) => onSubstitute.current->Chan.emit(Subst.Response(i, expr))
      }
    )
    Some(destructor)
  }, [])

  // relay <Link> events to "onResponse"
  React.useEffect1(
    () => Some(onClickLink.current->Chan.on(ev => onResponse->Chan.emit(Link(ev)))),
    [],
  )

  // relay <Subst> substitution to "onResponse"
  React.useEffect1(() => Some(
    onSubstitute.current->Chan.on(x =>
      switch x {
      | Subst.Request(i, expr, subst) => onResponse->Chan.emit(Substitute(i, expr, subst))
      | Response(_, _) => ()
      }
    ),
  ), [])

  let onExport = () => onResponse->Chan.emit(ExportProofObligations)

  let className = "gcl-panel native-key-bindings"

  let errorMessagesBlock = if Array.length(errorMessages) == 0 {
    <> </>
  } else {
    <div className="gcl-global-props">
      <h2> {string("Error Messages")} </h2>
      <ul className="gcl-global-property-list">
        {errorMessages
        ->Array.mapWithIndex((i, (header, body)) => <Item header body key={string_of_int(i)} />)
        ->array}
      </ul>
    </div>
  }

  let warningMessagesBlock = if Array.length(warnings) == 0 {
    <> </>
  } else {
    <div className="gcl-global-props">
      <h2> {string("Warnings")} </h2>
      <ul className="gcl-global-property-list">
        {warnings
        ->Array.mapWithIndex((i, warning) =>
          switch warning {
          | MissingBound(_loc) =>
            <Item
              header={"Bound Missing"}
              body={"Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""}
              key={string_of_int(i)}
            />
          | ExcessBound(_loc) =>
            <Item
              header={"Excess Bound"}
              body={"The bound annotation at this assertion is unnecessary"}
              key={string_of_int(i)}
            />
          }
        )
        ->array}
      </ul>
    </div>
  }

  <Subst.Provider value=onSubstitute.current>
    <Link.Provider value=onClickLink.current>
      <section className tabIndex={-1}>
        <DevPanel method=connection />
        errorMessagesBlock
        warningMessagesBlock
        <ProofObligations id pos onExport />
        <GlobalProps id props />
      </section>
    </Link.Provider>
  </Subst.Provider>
}
