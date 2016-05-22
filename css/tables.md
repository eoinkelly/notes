
Good responsive table example: http://codepen.io/AllThingsSmitty/pen/MyqmdM/
    * uses `content: attr(data-label);` to pull a data attribute into the visible DOM when certain media queries are active
    * attr()
        * https://developer.mozilla.org/en/docs/Web/CSS/attr
        * lets you use the _value_ of a selected element as the value of the content property
        * the spec lets you pull values of many types but it seems only string type has been implemented yet
        * has great browser support but only for use as the value of the `content` property and only with `string` data types
