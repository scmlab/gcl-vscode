open Belt
open React

@react.component
let make = (~onRequest: Chan.t<ViewType.Request.t>, ~onResponse: Chan.t<ViewType.Response.t>) => {
  let (connection, setConnection) = React.useState(_ => None)
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
      | ViewType.Request.UpdateConnection(method) => setConnection(_ => method)
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

  let className = "gcl-panel native-key-bindings"

  let sections = if Array.length(sections) == 0 {
    <> </>
  } else {
    <ul>
      {sections
      ->Array.mapWithIndex((i, value) => {
        <Element.Section value key={string_of_int(i)} />
      })
      ->array}
    </ul>
  }

  <Link.Provider value=onClickLink.current>
    <ReqID.Provider value=Some(id)>
      <section className tabIndex={-1}> <DevPanel method=connection /> sections </section>
    </ReqID.Provider>
  </Link.Provider>
}
