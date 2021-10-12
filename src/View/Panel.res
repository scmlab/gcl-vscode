open Belt
open React

@react.component
let make = (~onRequest: Chan.t<ViewType.Request.t>, ~onResponse: Chan.t<ViewType.Response.t>) => {
  let (connectionStatus, setConnectionStatus) = React.useState(_ => "Disconnected")
  let ((id, sections), setDisplay) = React.useState(() => (0, []))
  let onClickLink = React.useRef(Chan.make())

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
      | ViewType.Request.UpdateConnectionStatus(status) => setConnectionStatus(_ => status)
      | Display(id, blocks) => setDisplay(_ => (id, blocks))
      }
    )
    Some(destructor)
  }, [])

  // relay <Link> events to "onResponse"
  React.useEffect1(
    () => Some(onClickLink.current->Chan.on(ev => onResponse->Chan.emit(Link(ev)))),
    [],
  )

  // onInsertAnchor
  let onInsertAnchor = hash => onResponse->Chan.emit(InsertAnchor(hash))

  // onSubst
  let onSubst = id => onResponse->Chan.emit(Substitute(id))

  let className = "gcl-panel native-key-bindings"

  let sections = if Array.length(sections) == 0 {
    <> </>
  } else {
    <ul>
      {sections
      ->Array.mapWithIndex((i, value) => {
        <Element.Section value key={string_of_int(i)} onInsertAnchor />
      })
      ->array}
    </ul>
  }

  <Link.Provider value=onClickLink.current>
    <Substitution.Provider value=Some(onSubst)>
      <ReqID.Provider value=Some(id)>
        <section className tabIndex={-1}> <Status status=connectionStatus /> sections </section>
      </ReqID.Provider>
    </Substitution.Provider>
  </Link.Provider>
}
