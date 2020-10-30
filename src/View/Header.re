open React;

[@react.component]
let make =
    (
      ~header: View.Request.Header.t,
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

  <div className="gcl-header">
    {switch (header) {
     | Loading => <h2 className="text-plain"> {string("Loading ...")} </h2>
     | Plain(s) => <h2> {string(s)} </h2>
     | Error(s) => <h2 className="text-error"> {string(s)} </h2>
     }}
    <button className="gcl-mode-button gcl-mode-vscode" onClick=onChange>
      {string(
         switch (mode) {
         | WP1 => "WP"
         | WP2 => "WP2"
         },
       )}
    </button>
  </div>;
};
