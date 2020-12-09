@react.component
let make = (~onRequest: Chan.t<ViewType.Request.t>, ~onResponse: Chan.t<ViewType.Response.t>) => {
  // let (reqID, setReqID) = React.useState(() => None);
  // let (header, setHeader) = React.useState(() => ViewType.Request.Header.Loading)
  let ((id, pos, props), setDisplay) = React.useState(() => (0, [], []))
  let (hidden, setHidden) = React.useState(_ => false)
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
      | ViewType.Request.Display(id, pos, props) => setDisplay(_ => (id, pos, props))
      | Substitute(i, expr) => onSubstitute.current->Chan.emit(Subst.Response(i, expr))
      | Hide => setHidden(_ => true)
      | Show => setHidden(_ => false)
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
  React.useEffect1(() => Some(onSubstitute.current->Chan.on(x =>
      switch x {
      | Subst.Request(i, expr, subst) => onResponse->Chan.emit(Substitute(i, expr, subst))
      | Response(_, _) => ()
      }
    )), [])

  let onExport = () => onResponse->Chan.emit(ExportProofObligations)

  let className = "gcl-panel native-key-bindings" ++ (hidden ? " hidden" : "")

  // <ReqID.Provider value=reqID>
  <Subst.Provider value=onSubstitute.current>
    <Link.Provider value=onClickLink.current>
      <section className tabIndex={-1}>
        <ProofObligations id pos onExport /> <GlobalProps id props />
      </section>
    </Link.Provider>
  </Subst.Provider>
  // </ReqID.Provider>;
}
