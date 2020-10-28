open React;

[@react.component]
let make =
    (
      ~header: View.Request.Header.t,
      ~editorType: API.editorType,
      ~mode: GCL.mode,
      ~onChangeMode: GCL.mode => unit,
    ) => {
  open! View.Request.Header;

  let onChange = _ => {
    open GCL;
    let newMode =
      switch (mode) {
      | WP1 => WP2
      | WP2 => WP1
      };
    onChangeMode(newMode);
  };

  // display different type fo mode toggle base on the editor type
  let modeToggle =
    switch (editorType) {
    | API.VsCode =>
      <button className="gcl-mode-button gcl-mode-vscode" onClick=onChange>
        {string(
           switch (mode) {
           | WP1 => "WP"
           | WP2 => "WP2"
           },
         )}
      </button>
    | Atom =>
      <div className="gcl-mode-button gcl-mode-atom">
        <label className="input-label">
          <input
            className="input-toggle"
            type_="checkbox"
            checked={
              switch (mode) {
              | WP1 => false
              | WP2 => true
              }
            }
            onChange
          />
          {string("WP2")}
        </label>
      </div>
    };

  <div className="gcl-header">
    {switch (header) {
     | Loading => <h2 className="text-plain"> {string("Loading ...")} </h2>
     | Plain(s) => <h2> {string(s)} </h2>
     | Error(s) => <h2 className="text-error"> {string(s)} </h2>
     }}
    modeToggle
  </div>;
};