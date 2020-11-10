open Belt;

// a dictionary of FileName-State entries
let dict: Js.Dict.t((State.t, TaskRunner.t)) = Js.Dict.empty();

let get = fileName => dict->Js.Dict.get(fileName);

// do nothing if the state already exists
let add = (fileName, dispatcher) => {
  switch (get(fileName)) {
  | Some(_) => ()
  | None => dict->Js.Dict.set(fileName, dispatcher)
  };
};

let rename = (oldName, newName) => {
  let delete_: (Js.Dict.t('a), string) => unit = [%raw
    "function (dict, key) {delete dict[key]}"
  ];
  get(oldName)
  ->Option.forEach(state => {
      Js.log3("[ states ][ rename ]", oldName, newName);
      delete_(dict, oldName);
      add(newName, state);
    });
};

// remove the entry (but without triggering .destroy() )
let remove = fileName => {
  let delete_: (Js.Dict.t('a), string) => unit = [%raw
    "function (dict, key) {delete dict[key]}"
  ];
  delete_(dict, fileName);
};
let destroy = fileName => {
  get(fileName)
  ->Option.mapWithDefault(
      Promise.resolved(),
      ((state, taskRunner)) => {
        Js.log("[ states ][ destroy ]" ++ fileName);
        State.destroy(state);
        TaskRunner.destroy(taskRunner);
        // ->Promise.flatMap(() => TaskRunner.destroy(taskRunner));
      },
    )
  ->Promise.get(() => remove(fileName));
};

let contains = fileName => get(fileName)->Option.isSome;

// let destroyAll = () => {
//   dict
//   ->Js.Dict.entries
//   ->Array.map(((_, (state, taskRunner))) => {
//       Js.log("[ states ][ destroy all ]");
//       State.destroy(state) |> ignore;
//       TaskRunner.destroy(taskRunner);
//     })
//   ->List.fromArray
//   ->Promise.all
//   ->Promise.get(_ => ());
// };
