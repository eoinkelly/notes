# My CSS architecture

create as good an architecture document as I can following the code complete pre-requesites process

##The Problem


this architecture should say _why_ i chose things and _why_ i didn't choose others
should give pros/cons

## Requirements

*   Quick for future devs to learn and understand
*   Maintainable. Can be extended without breaking
Flexible. Must not be so rigid that I can't extend it later
*   Fast
*   Semantic. The bits that matter about semantics are
*   Reusable. I want to reuse code within the project and across projects. must allow me to create well tested reusable modules
*   Small code. CSS should be as small as possible



## Architecture


the grid takes care of horizontal, the module's intrinsic height takes care of vertical
a grid box can contain many modules but a module should not know or care where it is

Understanding the grid

I'm a module and...
	I take up 100% of the available horizontal width
	I take up as much height as I and my children require

I'm a grid-unit and...
	I know how my horiziontal width as a % of my parent's width
	I am floated left.
	I clear my floats (or establish new formatting context depending on code)
	I don't care about  my height - I let the things I contain tell me what they need
	I have 0 margins and 0 padding.
	My parent must be a grid-line box
	All my siblings are also grid-unit boxes
	I only touch modules or another grid-line box on my inside
	? can i contain a grid-line box and a module?
	I can contain more than one module but they must stack vertically (following normal document flow) within me. Modules must not know how to lay themselves out relative to each other.

I'm a grid container and ...
	I shouldn't have any sibling elements
	My parent should be the HTML element
	My children are all grid-line boxes
	I know my width relative to the width of the viewport

I'm a grid-line box and ...
	My parent must be either a grid-container box or a grid-unit box.
	My siblings are all grid-line boxes
	My children are always grid-unit boxes

which type of block does the spacer class get applied to?



add .icon as a new root
.icon-important
.icon-alert
and mark it up like this
<i class="icon icon-important"></i>
apparently twitter bootstrap does this

.promo-box h3 assumes structure of the html doc
.promo-box .title allows you to use any element in the markup and to change it later wihtout editing the CSS

when faced with wanting to tweak a module in a certain place on the page consider how I want it to look and WHY I am changing it
and create the new variant to reflect that

My first instinct is
	#sidebar .widget_search {}
then i realise i want it to be the full width of the sidebar so create
	.widget-search-full-width
etc.

classes that extend others must appear after the parent style in the CSS file so the cascade causes it to override the parent
based on the 4 layers in andy humes talk

media queries are stored with the selectors that they apply to.
    - you can't see all the changes that will apply in one place
    + you can easily find where the styles are


Variables
    all defined in one place so you can find them easily
    no indirection - the variables defined in _variables.scss are used as-is
    when I need a new color or to fundementally change colors I have to copy+paste everywhere that references that color. This takes a bit longer but
    makes the resulting code easier to understand.
    small variations on the color can be made without changing its name
    i have decided to have color value have color names, not logical names


/*
+ easier to type and remember real color names
- if we change mind about color i either have to make the color not match it's name or do global search+replace

i want out of this
    not to repeat myself
    have a color defined in one place and used throughout the site
    so having many color variables might be better
    a file that a designer can understand - not too codey!

$black: #000;

$color-paragraph-text: $black; // or #000 if not too many colors



// I don't use semantic color names like $contact-details-heading
/*
semantic color names e.g. $contact-details-heading
+ all colors for the site are in one place
- you need to come up iwth descriptive names for each color location e.g.
    h1-color, h2-color, h3-color - this leads to a lot of typing

would it make more sense for color and font to be set where the selector is defined

color and font are a site-wide things
how often do we want to change them completely?

it is a bonuse to define typeography all in one place as it makes it easier to avoide "one-off" tweaks to it

 I am making a conscious decision to keep all color and typography in one place in the CSS. The pros/cons are
 + easier to enforce rules on size, scale, palettes etc.
 + easier to see the "big picture" patterns in your design, easier to answer quesitons like "how many weights have i used", how many font faces, how many colors, are all colors on palette
 + easier to change colors, fonts after
 - all the CSS that styles a particular element or class will not be one place, it will be in 2 places (not a big deal)

 I could put a palette at the start of each "section":
 + keeps the palette with the stuff it applies to
 - harder to get an overview of the whole site palette

? should i have logical colors or just use the names though-out my scss files?

// variables make you want to have a "complete set" which leads to needless css code bloat
// only have variables for what I actually use - css inheritance is a *feature*

should SCSS variables be defined in the "scope" that they are used or all in one file
    one file approach
        + easy to find them and understand where variables are
        - they are defined farther away from where they are used
        + all the vars are global so this makes it explicit
        ? it makes lots of sense for stuff like colors and fonts but maybe less for variables that are just used in one module?
        - the variables do not travel with the file - there is a dependency created

*/

all color variables set in _variales.scss - all other files reference these names directly
we could have another level of indirection where we create logical color names and assign them to real colors
    - a lot of color names
    - another level of redirection to understand
    = not much win
    could be good if modules are very large or lots of colors or need themes


it seems important that the grid doesn't have any margins or padding so that you can next it sanely

can subsitute divs with approprriate more semantic elements in the grid. since all css tied to clea


.g-
.mod-
.btn-

The grid

Lastly, grids are invisible. You don’t give them backgrounds, borders, or change them in any way beyond their function as a structural element.


should the semantic elements be usedon the grid or direclty on the module?
    module could appear in diff part of page?
        widgets might?

    they aren't in the CSS anyway
can add roles to html too, css will ignore it
CSS ignores IDs so add them only if JS needs access to them

    m
Modules are laid out on the page using the grid
is it best practice to use separate divs for grid and module elements?

Advanced reading
    http://chriseppstein.github.com/blog/2011/08/21/responsive-layouts-with-sass/
    cool living styleguide http://style.caring.com/ from compass guy

there should be no crossover between grid and module

### Modules

modules that extend .mod start with mod-
.mod
.mod-menu
.mod-wysiwyg
.mod-searchbox

### Buttons

.btn
.btn-large

are wordpress templates a module?
    they are not necessairly singletons
    a template is a layout
    we want out CSS to be more reusable
where do i put style changes that relate to just one template?

We do not use wordpress templates to scope styles.

what about singletons - do i create a new module extension?

TODO grade my arch and reqs based on chap 2 of code complete

