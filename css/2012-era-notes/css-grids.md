# Grids

Questions About Grids

should media queries be named based on their

- width
- intended actions on the page
    - brittle if one media query does different things to different modules
- device they target?
    - doesn't really respect the content of each module

How many grid containers should be on a page

- option: just one that envcompasses the whole page
- option: just create a grid whenever you need one - there can be multiple grids
  on the page but the whole page doesn't have to be a grid. Grids and responsive
  design ? all grid systems seem to include _device specific_ breakpoints that
  trigger layout change (usually "eveything into one column") ? are there any
  grid systems that allow you to break at different widths?

when using a grid on the page as a whole it's hard to scope styles for things
that are made up of more than one grid unit e.g. a porfolio piece has multiple
grid lines but its hard to add a .portfolio class to the container because there
is no one container

- a grid forces you to rearrange all areas on the page at the same time when
  doing responsive design

Explore the idea "Growing a Grid guided by X and Y etc."

- we know the least about the design right now so the grid should allow you to
  put off as many design decisions as possible until we (hopefully) have better
  knowledge.
- grids should be designed for flexibility above all else?
- a grid is a "contract/interface" provided by CSS classes.

When is the right time to use a grid? ? always ? never ? when the design has
regular no. of columns and rows and regular spacing between them implications of
this: _ the design needs to be done **before** you can create a grid for it. _
as a developer i feel I have come at this backwards (grid before design) more
often than not. _ we have assempbled a collection of the things we need in the
app with a pre-cut grid and put off "design" until later _ in order to make good
decisions about a grid, the visual designer needs to be "on board" and adhering
to the same grid (otherwise we will end up breaking grid rules more often than
we keep them)

One grid per-page + grid is supposed to provide order to the design as a whole
so changing grids is not as useful - a design might require more than one grid ?
waht are the cases for this

grid elements should not have other styles applied to them

- this considers the grid as an invisible skeleton that holds the page together

* it makes for extra markup divs

- it is easy to understand - "grid divs" do one job only.
- there is less chance that a style class applied to a "grid div" will
  accidently overwrite a property required by the grid (ie. accidently break the
  grid)
- it is easier when including partial templates (the included partial doesn't
  have to know about the box it is being put in) ? what do you do when the area
  covered by a grid div needs styling?
    1. add another div inside/outside with styling?


        - extra markup
        - results in the grid containers not just touching other grid containers - is this difficult to understand?

    2. apply the styling to the grid div

3. grids should have gutters defined in them

- handy if the site has a very "regular" layout

* I sometimes found myself overriding the gutters more often than I used them.

- the answer is easy if we have a completed visual design to look at - we often
  do not.

3a. a grid should have spacing helpers + the helpers create a sort of lego for
flexible guttering + you put all the spacing requiremetns for your app into a
few classes which prevents repeating spacing manually on each unit - keeps
things DRY. - you have to add the classes to everything that needs space - this
is only good if 50%+ of your modules do not require any space. ? should spacing
helpers be something you refactor into? \* if the visual design was finished
before we created the grid AND we require multiple spacing between grid modules
then helpers would be a great solution

4. should grid divs outside edges just touch other grid divs (where grid divs =
   containers, rows, units)

# Inbox

http://framelessgrid.com/ http://stacklayout.com/key_benefits.html
https://github.com/vladocar/Box-CSS-Framework#readme http://www.ez-css.org/
https://github.com/necolas/griddle

Reading
http://www.stubbornella.org/content/2009/07/23/overflow-a-secret-benefit/
http://www.stubbornella.org/content/2010/12/09/the-hacktastic-zoom-fix/
http://stackoverflow.com/questions/6196725/how-does-the-css-block-formatting-context-work

http://www.vcarrer.com/2009/06/1-line-css-grid-framework.html

Criteria for judging grid systems Nestable Flexible Gutters or not how does it
handle creating empty spaces?

1. http://960.gs/ floats each box (includes clearfix) fixed width columns each
   column has a margin (gutter) defined width of each box set in pixels, all
   those pixel dimensions set to work with 960px wide layout only boxes display
   set to 'inline' lots of CSS code

2. https://github.com/Montoya/blueprint-plugin---liquidgrid liquid version of
   blueprint

3. http://www.blueprintcss.org/ 24 cols, 30px fixed width + 10px margin between
   each col quite complete - has classes for adding padding to inner boxes etc.
   it is nestable fixed widht grid each row is ended by a .last .container
   .span-4 .span-16 .span-4.last

    does push pull calsses to change column order from that in the source can
    add empty columns before/after a block using class

4. yui grid box width set in % (can work out exact dimensions based on body
   widht in px) class names fractional 1/24 --> 23/24 boxes display set to
   'inline-block' terse CSS code

5. OOCSS grid. very terse code line display set to 'table' % based grid fully
   nestable boxes floated

6. Less Framework http://lessframework.com/ responsive (4 layouts)

Golden grid
http://goldengridsystem.com/?utm_source=twitterfeed&utm_medium=twitter
responsive - look sinteresting

http://nickcowie.com/other/golden_section.html interesting ideas of making grid
proportions based on golden circle http://adapt.960.gs/ a javascript file which
determines which css file to load before a browser renders a apge

Semantic Grid http://semantic.gs/

things I want: flexible % or em based grid no gutters - have separate classes
for ventilation follows oocss principles they should be nestable ideally be able
to push/pull blocks to lay them out in diff order
