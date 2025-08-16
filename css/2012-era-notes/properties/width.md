# width

Sources

https://drafts.csswg.org/css-sizing-3/

Overview

- sets width of `content` area by default
- sets width of `content + padding + border` area if you use
  `box-sizing: border-box;`
- Values
    1. Any length value
        - percentage = a percertnage of **the containing block**
        - if element is absolutely positioned **and** containing block is a
          "block container element" then
            - a percentage of the padding box of the parent
    2. keywords:
        - min-content:
            - the smallest width that would fit around its content if **all**
              the soft-wrap opportunities within the box are taken
        - max-content:
            - the smallest width that would fit around its content if **none**
              of the soft-wrap opportunities within the box are taken
        - fit-content:
            - roughly equiv to `margin-left: auto; margin-right: auto;` but it
              "works for unknown widths"
            - needs prefixes but has pretty good modern browser support
- Overridden by:
    - `max-width`
    - `min-width`
- Inherited: no
- Elements it doesn't apply to:
    - non-replaced elements
    - inline elements
    - table rows
    - table row groups (`thead`, `tbody`, `tfoot`)
