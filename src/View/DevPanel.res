// open Belt
open React

@react.component
let make = (~devMode: bool) => {
    let className = {"gcl-dev-panel " ++ (devMode ? "" : "hidden")}
    <div className>
        {string("dev")}
    </div>
}
