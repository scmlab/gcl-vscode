// reliable duplex Chan
module Module: {
  type t<'a, 'b>
  let make: unit => t<'a, 'b>
  let send: (t<'a, 'b>, 'a) => Promise.t<'b>
  let handle: (t<'a, 'b>, 'a => Promise.t<'b>, unit) => unit
  let destroy: t<'a, 'b> => unit
} = {
  type t<'a, 'b> = {
    req: Chan.t<'a>,
    res: Chan.t<'b>,
  }

  let make = () => {req: Chan.make(), res: Chan.make()}
  let send = (self, req) => {
    let promise = self.res->Chan.once
    self.req->Chan.emit(req)
    promise
  }
  let handle = (self, handler) =>
    self.req->Chan.on(res => handler(res)->Promise.get(self.res->Chan.emit))

  let destroy = self => {
    Chan.destroy(self.req)
    Chan.destroy(self.res)
  }
}

include Module
