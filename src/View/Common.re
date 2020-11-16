open React;

module Space = {
  [@react.component]
  let make = () => <div className="expr-space"> {string(" ")} </div>;
};

module Paren = {
  [@react.component]
  let make = (~activate, ~children) => {
    let (hovered, setHover) = React.useState(_ => false);
    let (collapsed, setCollapse) = React.useState(_ => false);
    let onMouseOver = _ => setHover(_ => true);
    let onMouseLeave = _ => setHover(_ => false);
    let onClick = _ => setCollapse(_ => collapsed ? false : true);
    let className =
      "expr-paren"
      ++ (hovered ? " hovered" : "")
      ++ (collapsed ? " collapsed" : "");

    if (activate) {
      if (collapsed) {
        <div className onMouseOver onMouseLeave onClick>
          {string("(...)")}
        </div>;
      } else {
        <>
          <div className onMouseOver onMouseLeave onClick>
            {string("(")}
          </div>
          children
          <div className onMouseOver onMouseLeave onClick>
            {string(")")}
          </div>
        </>;
      };
    } else {
      children;
    };
  };
};