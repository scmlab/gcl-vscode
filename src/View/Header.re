open React;

[@react.component]
let make =
    (
      ~header: View.Request.Header.t,
      ~mode: View.Response.mode,
      ~onChangeMode: View.Response.mode => unit,
    ) => {
  open! View.Request.Header;
  //   let (header, setHeader) = React.useState(_ => Loading);
  //   let (activated, setActivation) = React.useState(_ => false);

  let onChange = _ => {
    open View.Response;
    let newMode =
      switch (mode) {
      | WP1 => WP2
      | WP2 => WP1
      };
    // events.onSetMode.emit(newMode);
    onChangeMode(newMode);
  };

  <h2 className="gcl-header">
    {switch (header) {
     | Loading => <div className="text-plain"> {string("Loading ...")} </div>
     | Plain(s) => <div> {string(s)} </div>
     | Error(s) => <div className="text-error"> {string(s)} </div>
     }}
    <div className="gcl-mode">
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
  </h2>;
};