# Flexbox

Sources

* https://css-tricks.com/snippets/css/a-guide-to-flexbox/
* https://egghead.io/series/flexbox-fundamentals


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


* Don't use flexbox for overall page layout
    * https://jakearchibald.com/2014/dont-use-flexbox-for-page-layout/
        * browser renders content as it gets it
        * if user is on slow connection then using flexbox will cause content to jump around when new content arrives
        * "content dictates layout" with flexbox whereas "container dictates layout" in the grid specification
* overall
    * Flexbox is mostly here now and is great so use it if you can but don't use it for overall page layout

