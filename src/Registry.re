open Belt;

// a dictionary of FileName-State entries
module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module TaskRunner = TaskRunner.Impl(Editor);
  let dict: Js.Dict.t((State.t, TaskRunner.t)) = Js.Dict.empty();

  let get = fileName => dict->Js.Dict.get(fileName);

  let getByEditor = (editor: Editor.editor) =>
    editor->Editor.getFileName->Option.flatMap(get);

  // do nothing if the state already exists
  let add = (fileName, state) => {
    // let fileName = editor->Interface.getFileName';
    switch (get(fileName)) {
    | Some(_) => ()
    | None =>
      Js.log("[ states ][ add ]");
      dict->Js.Dict.set(fileName, state);
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
          Js.log("[ states ][ destroy ]");
          State.destroy(state) |> ignore;
          TaskRunner.destroy(taskRunner);
        },
      )
    ->Promise.get(() => remove(fileName));
  };

  let contains = fileName => get(fileName)->Option.isSome;

  let destroyAll = () => {
    dict
    ->Js.Dict.entries
    ->Array.map(((_, (state, taskRunner))) => {
        Js.log("[ states ][ destroy ]");
        State.destroy(state) |> ignore;
        TaskRunner.destroy(taskRunner);
      })
    ->List.fromArray
    ->Promise.all
    ->Promise.get(_ => ());
  };
};
