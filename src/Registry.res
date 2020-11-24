open Belt

// a dictionary of FileName-State entries
let dict: Js.Dict.t<State.t> = Js.Dict.empty()

let get = fileName => dict->Js.Dict.get(fileName)

// do nothing if the state already exists
let add = (fileName, state) =>
  switch get(fileName) {
  | Some(_) => ()
  | None => dict->Js.Dict.set(fileName, state)
  }

let rename = (oldName, newName) => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  get(oldName)->Option.forEach(state => {
    delete_(dict, oldName)
    add(newName, state)
  })
}

// remove the entry (but without triggering .destroy() )
let remove = fileName => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  delete_(dict, fileName)
}
let destroy = fileName => {
  get(fileName)->Option.forEach(State.destroy)
  remove(fileName)
}

let contains = fileName => get(fileName)->Option.isSome

let size = () => Js.Dict.keys(dict)->Array.length
