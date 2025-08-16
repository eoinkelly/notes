# Flexbox

Sources

- https://www.w3.org/TR/css-flexbox-1/
- https://css-tricks.com/snippets/css/a-guide-to-flexbox/
- https://egghead.io/series/flexbox-fundamentals
- https://hackernoon.com/11-things-i-learned-reading-the-flexbox-spec-5f0c799c776b

- lets you decouple source order from layout order

- 3 syntaxes
    - most recent uses `flex-*` prefix

"flex containers" contain "flex items" arranged on "flex lines"

> A flex container expands items to fill available free space, or shrinks them
> to prevent overflow Flexbox is (aside from optional wrapping) a
> single-direction layout concept.

    * so each flex-container has a single "direction" (up, down, left, right)

> Note that float, clear and vertical-align have no effect on a flex item.

- flex line
    - 1 flex-line per flex-container by default
    - can be LTR or RTL

6 significant properties on a flex container

1. display
1. align-content
1. flex-wrap
1. align-items
1. flex-direction
1. justify-content

5 significant properties on a flex item

1. order
1. margin
1. align-self
1. flex
1. visibility

- Don't use flexbox for overall page layout because
    - browser renders content as it gets it
    - if user is on slow connection then using flexbox will cause content to
      jump around when new content arrives
    - "content dictates layout" with flexbox whereas "container dictates layout"
      in the grid specification
    - More details:
      https://jakearchibald.com/2014/dont-use-flexbox-for-page-layout/

## Flexbox spec

CSS 2.1 defined 4 "layout modes" or algorithms for deciding the position and
layout of boxes

1. block layout
1. inline layout
1. table layout
1. positioned layout

The flexbox spec adds a new _layout mode_

- flexbox has no concept of floats or columns like block layout does

boxes laid out in flex layout mode can flex their size to take up **available
space**

you can `order: -1` to "move the element one up from its source order

the `auto` margin value seems special in flexbox

a flex container establishes a new "flex formatting context" for its contents
similar to block formatting context: _ floats do not intrude into the flex
container _ the margins of the flex container do not collapse with the margins
of its contents

- flex containers form the "containing block" for their children
- flex containers are not block containers so
    - float, clear does nothing
    - vertical-align does nothing
    - `column-*` properties do nothing
- absolutely positioned children of flex-containers do not participate in flex
  because they are not in flow

- flex-item
    - establishes a new formatting context for its contents
    - the margins between flex-items **do not collapse**
    - % margin and padding on flex-items should be avoided because the spec
      doesn't say what the % is resolved against
    - z-index value other than auto will create a new stacking context even if
      position is static

> Auto margins expand to absorb extra space in the corresponding dimension. They
> can be used for alignment, or to push adjacent flex items apart.

## Flexbox gap

My codepen: https://codepen.io/eoinkelly/pen/LYxaBdm?editors=1100

- support: modern browsers except Safari but it is in the TP
    - https://caniuse.com/flexbox-gap
- these properties are also supported on `grid` and `multi-column` layouts but
  browser support differs so check before using.
- sets the size between rows and columns of items in the flex container
- lets you avoid using margin and padding for spacing elements within a flexbox
  container

```scss
.flexy-thing {
    display: flex;
    gap:  100px // shorthand for
    column-gap: 100px;
    row-gap: 100px;
}
```

https://css-tricks.com/almanac/properties/g/gap/
