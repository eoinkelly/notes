# Flexbox

https://css-tricks.com/snippets/css/a-guide-to-flexbox/

* lets you decouple source order from layout order
* 3 syntaxes
    * most recent uses `flex-*` prefix


"flex containers" contain "flex items" arranged on "flex lines"
 > A flex container expands items to fill available free space, or shrinks them to prevent overflow
 > Flexbox is (aside from optional wrapping) a single-direction layout concept.
    * so each flex-container has a single "direction" (up, down, left, right)
> Note that float, clear and vertical-align have no effect on a flex item.

* flex line
    * 1 flex-line per flex-container by default
    * can be LTR or RTL

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
