# CSS Rounding

http://meyerweb.com/eric/thoughts/2010/02/10/rounding-off/
http://www.positioniseverything.net/round-error.html

https://bugzilla.mozilla.org/show_bug.cgi?id=63336
http://robert.ocallahan.org/2007/02/units-patch-landed_07.html
http://robert.ocallahan.org/2007/10/tale-of-two-zooms_19.html

http://css-tricks.com/percentage-bugs-in-webkit/ Like some others explained at
the top, there are 3 types of calculation for sub-pixels.

    IE-Method:

    – classic round for subpixels
    – round-down<0.535 ///// 32.3=>32
    – leads to the problem that floated divs break the layout when IE rounds-Up too often.

    Opera/Safari-Method:

    – always round-down subpixels
    – e.g. 34.6=>34 ///// 32.3=>32
    – leads to some blank pixels at the left when divs are floated to the left and at the right when floated right

    Firefox Method:

    – complex algorithm for rounding
    –>http://elasticss.com/determination-of-algorithms-used-for-percentage-based-rounding-divs-on-browsers-and-css-frameworks/
    – leads to the best result for layouts, but its not transparent which div gets which exact size.

aspen site full site width 100% = 1015px

    .primary
        chrome 804px
        opera 801px
        IE 9 804.99px
        FF 804.983px
        safari: 804px (presumably cos it's webkit like chrome)

maybe can make a bigger box that divides evenly and has overflow: hidden set?
