# Viewport units

- http://caniuse.com/#feat=viewport-units
    - supported by everything except IE's don't support vmax

https://web.dev/learn/css/sizing/

- px
- em
- ex (0.5 \* em, the x-height of the font)
- ch (width of character, works best in monospaced fonts)
- rem
- % (varies depending on the property block dims are percentage of the parent
  block's width, fonts are % of parent font size)
- Length units representing a percentage of the current viewport dimensions:
    - width (vw)
    - height (vh)
    - the smaller of the two (vmin)
    - or the larger of the two (vmax)

- Container Query **Units** (not the same spec as container queries specify a
  length relative to the dimensions of a query container.
    - The units include:
        - Query Width (cqw): 1% of the container width
        - Query Height (cqh): 1% of the container height
        - Query Minimum (cqmin): the smaller value of Query Width cqw or Query
          Height cqh.
        - Query Maximum (cqmax): the larger value of cqw or cqh.
        - We also have two other units: cqi and cqb and they mean Query Inline
          and Query Block, respectively.
            - They can be used for cases where we have different writing modes.
    - As of 2022-Nov, good browser support except firefox
