Good responsive table example: http://codepen.io/AllThingsSmitty/pen/MyqmdM/ _
uses `content: attr(data-label);` to pull a data attribute into the visible DOM
when certain media queries are active _ attr() _
https://developer.mozilla.org/en/docs/Web/CSS/attr _ lets you use the _value_ of
a selected element as the value of the content property _ the spec lets you pull
values of many types but it seems only string type has been implemented yet _
has great browser support but only for use as the value of the `content`
property and only with `string` data types
