
Andy Hume - CSS For grown ups
==============================
audeio: http://schedule.sxsw.com/2012/events/event_IAP9410
slides: http://speakerdeck.com/u/andyhume/p/css-for-grown-ups-maturing-best-practises


We need to add constraints to CSS to help us manage complexity by keeping things separate

AH feels that we should optimise code for change aka maintainablility
    "how easy and safe is it to make changes/maintain the codebase"
    if not maintainable
        - harder to fix bugs
        - harder to optimise

think of CSS in layers and your selectors should not cross layers!

layer 0: the html document
layer 1: SMACSS base rules = my element defaults

layer 2: modules (indivisula large-ish compontents)
    in my current code, i guess my wordpress templates are considered "modules"

layer 3: layout
    the grid lives in this layer on it's own and selectors should not cross layers

layer 4: skins
    like the various skins that simplicity theme has


.module-name {}
.module-name--extension-name {}

a module should know it's own width and be able to change itself accordingly but
it breaks layers to have a module know the browser width

media query in the module
    - breaks layers, module is now less flexible as it depends on browser width

group all media queries together
    - styles for .module is now in two places

AH argues that it's better to extend modules with these helper classes on a case
by case basis rather than trying to come up iwth a system that defines "if module
X is below Y then have 1em of space, otherwise don't but if module Z is above X they do etc."

.breathing-room-top {
    margin-top: 1em;
}
.breathing-room-bottom {
    margin-bottom: 1em;
}
.gutter-left {
   padding-left: 1em;
}
.gutter-right {
   padding-right: 1em;
}


There can be other layers on top of this

we want to keep the layers separate

.promo-box h3 {} couples layer 1 and 3
.promo-box .promo-box-h {} is better as it's all level 3

#sidebar .promo-box {} is bad
    couples layer 3 and 2
    targets an ID
    a better alternative is to extend .promo-box wiht a variant
    can name it .promo-box--light (double dash )

headings 1-6 alternatives when styling
.h-headline
.hsubheadline
.h-byline
.h-standfirst
.h-related
.h-promo

in responisve can use JS to apply another class to a given element when it reaches a particular size

he is ok iwth some presentational only classes e.g.
.margin-top { margint-top: 1em; }

his best practice for sprited icons
<p><i class="icon icon-comment"></i>23 Comments</p>
<a href=""><i class="icon icon-comment"></i>23 Comments</a>

keep away from pages as long as you can - craete a design language and library that can be re-used


create styleguides for each layer in code - let this be the documentation of the design vocabulary of the site
