open Belt
open React

@react.component
let make = (~onRequest: Chan.t<ViewType.Request.t>, ~onResponse: Chan.t<ViewType.Response.t>) => {
  let (connection, setConnection) = React.useState(_ => None)
  let ((id, blocks), setDisplay) = React.useState(() => (0, []))
  // let (errorMessages, setErrorMessages) = React.useState(_ => [])
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
      | Display(id, blocks) => setDisplay(_ => (id, blocks))
      // | SetErrorMessages(msgs) => setErrorMessages(_ => msgs)
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

  // let onExport = () => onResponse->Chan.emit(ExportProofObligations)

  let className = "gcl-panel native-key-bindings"

  let blocks = if Array.length(blocks) == 0 {
    <> </>
  } else {
    <ul>
      {blocks
      ->Array.mapWithIndex((i, value) => {
        <Element.Block value key={string_of_int(i)} />
      })
      ->array}
    </ul>
  }

  <Subst.Provider value=onSubstitute.current>
    <Link.Provider value=onClickLink.current>
      <ReqID.Provider value=Some(id)>
        <section className tabIndex={-1}>
          <DevPanel method=connection />
          blocks
        </section>
      </ReqID.Provider>
    </Link.Provider>
  </Subst.Provider>
}
