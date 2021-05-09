## :where pseudo-class

Sources

* https://developer.mozilla.org/en-US/docs/Web/CSS/:where

Summary

* `:is()`  but easier to override
    * maybe more suited to setting default styles or in design systems like bootstrap kinda stuff?

Overview

* Support: 2021-04-27: all modern browsers support it but Safari doesn't support the forgiving selector list bit yet
* functions very much like `:is()`
* The difference between :where() and :is() is that :where() always has 0 specificity, whereas :is() takes on the specificity of the most specific selector in its arguments.
    * This means that you can more easily override the styles set by a `:where` selector
* if any selector in the list is not valid then that selector will be ignored but the whole list will not be
    * this "forgiving" selector behaviour is different to what selectors normally do
    * makes it useful for supporting features that might not yet be in all browsers I guess?

