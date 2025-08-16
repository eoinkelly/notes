# Inbox

================ http://www.sitepoint.com/books/cssant4/

- http://cssdesignpatterns.com/
- http://24ways.org/2011/unicode-range
- http://net.tutsplus.com/tutorials/html-css-techniques/the-30-css-selectors-you-must-memorize/
  css3 sleector reference
  http://vogtjosh.com/selectors/?utm_source=html5weekly&utm_medium=email

ensure half-leading is an integer for consistent browser rendering of fonts
http://stackoverflow.com/questions/5880459/vertical-alignment-text-in-container-webkit-vs-firefox
how does line-height and height interact in CSS?

We group selectors by layer and module - not by output e.g. print, media query,
browser version this allows for isolated testing potentially loading assets when
required

KEEP ALL MODULE STYLES IN ONE FILE FOR THAT MODULE. _ module styles should
default to it's initial state _ this includes state selectors. Consider
.is-{action} selectors as they can be flexible e.g. JS could apply .is-pressed
to all buttons and the module selector could then decide how to handle it vs
jquery applying a unique class to buttons in each module _ this also includes
any themes that can be applied to the module _ global state rules don't make
much sense e.g. .is-pressed, .is-disabled - can use mixins if there is common
code \* .is-hidden might make sense???

I use hacks to target browser versions and mark them with @ie6-hack, @ie7-hack
in comments. TODO document all these

I organize declarations in the following order: Box Border Background Text Other

Percent is wacky in CSS: Top/bottom padding = % of width. Height = % height of
container. Line-height = % of font-size. Too prescriptive

<!-- http://yaccessibilityblog.com/library/aria-fix-non-standard-images.html -->
<div id="meowmeow" role="img" aria-label="Kitty Cats"></div>

<style>
  #meowmeow {
        background: url(http://placekitten.com/720/405/) no-repeat center center;
        background-size: cover;
        height: 0;
        padding-bottom: 56.25%; /* 16:9 */
    }
</style>G

# Comments

CSS Mastery recommends: /_ @group Structure _/ as CSS Edit on OS X can parse it.

    @todo
    @workaround
    @bugfix
    these are all part of CSS doc - I think these are worth doing

eaningful markup is important becuase:

- it is easier for others to work with
- it can be understood by other programs & devices
- you can style elements directly without having to add other identifiers
- classes & ids can be used to add extra layers of meaning (but it is not wrong
  if they don't - I think presentational classes are OK)
- non presentational class & id names are preferred but not required (other
  design constraints may trump them)

- class & id names are not case sensitive so use all lowercase and sepearate
  with hyphens
- the _div_ element stands for "division" and provides a way of splitting up the
  document into meaningful areas - it is not as specific as _aside_, _section_
  etc.

# CSS Nomenclature

selector { /_ A rule _/ property: value; /_ A declaration _/ property: value; }

property + value = declaration rule = selector + a block of one or more
declarations ruleset = one or more rules

# Property Values

A CSS property has different values depending on where in the computation
process it is

1.      Specified Value
        set in 3 ways:
        	1.	If a stylesheet sets a value for this property
        	2.	If no stylesheet mentions it, it is inherited from the parent element (if possible)
        	3. 	Otherwise the initial value of the property as per the CSS spec is applied.
    Computed Value It is "computed" from the specified value by 1. handling the
    special values 'inherit' and 'initial'. The 'inherit' value can be used to
    enforce inheritance of values, and it can also be used on properties that
    are not normally inherited. 2. doing the computation necessary to reach the
    value specified in the "computed value" line in the spec It is also used for
    inheritance i.e. child elements will inherit the computed value, not the
    specified value When inheritance occurs, elements inherit computed values.
    The computed value from the parent element becomes both the specified value
    and the computed value on the child. Used Value The final value of the
    property after all calculations have been performed _ Dimensions are all in
    pixels _ It is calculated after layout has been calculated (properties like
    'width' that have relative units need layout before we know what their final
    value is) _ The used value is what we see in Firebug/Web inspector etc. _
    CSS properties that do not depend on layout, the computed value and used
    value is the same. _ These properties do depend on layout so have different
    computed and used values: _ background-position _ bottom, left, right, top _
    height, width _ margin-bottom, margin-left, margin-right, margin-top, _
    min-height, min-width _ padding-bottom, padding-left, padding-right,
    padding-top _ text-indent Actual Value \* The actual value is the used value
    after any approximations have been applied e.g. the UA might only be able to
    render integer pixel widths.

This means that if an element as a 3% margin, then it's children will inherit
3%, not whatever final pixel value was used.

# Glyph Placement

CSS Spec defn of an 'em': "is equal to the computed value of the 'font-size'
property of the element on which it is used. The exception is when 'em' occurs
in the value of the 'font-size' property itself, in which case it refers to the
font size of the parent element."

The "em" is a fundemental qproperty of a truetype font. TT font glyphs are
designed i relation to a square grid - each unit is called an FUnit. There are
ususally 2048x2048 FUnits in the em square. When the size of the font changes,
the size of the FUnit changes - it is **still** 2048x2048 FUnits in the grid.
http://www.emdpi.com/emsquare.html

Typically, in text fonts, the height of flat uppercase Latin letters such as E,
H or I is 70% of the UPM size.

Browser alwasys assumes 96dpi

em is a property of truetype fonts. The em square is the block that each glyph
lives in 1 em = 2048 FUnits (usually, can be diff) 1 em = the "font size" of the
font some glyphs may bleed outside the em square

If I pick 10pt font then the em square will be 10pt - this means that most of
the glyphs will be smaller than that ascent = the amount the glyphs go above the
baseline font-height = the actual height of rendered glyphs descent = how much
the glyphs go below the baseline internal leading = what is added to the ascent
for that glyph (the glyph box has to be square) font-height rendered on screen =
ascent + descent + internal leading (which varies between fonts)

# CSS Selectors

Using descendant selectors says that "these selectors will always appear
together" - it implies a relationship so don't use them where none exists. .a .b
implies that .b will _always_ be wrapped in .a on the site

You never want to write CSS in a way that requires changes higher in the DOM
tree in order to make child elements behave properly. let the grid define the
width and the content define the height

# CSS Code Presentation

1. blank line between blocks
2. tabularize
3. remove

css learning plan

1. basic
1. concepts
1. box model 2. specifiicty 3. cascade 4. layout & positioning
1. typography
1. intermediate 2. advanced

another css grouping to consider: valid css3 vs not - should they be in diff
files or diff places in file?

CSS Learning Plan CSS Final Checklist Calculating the height of an Inline Level
Box The height of an inline box is the smallest that will enclose all glyphs L =
leading vertical-align Media Types Inheritance Inheritance and the cascade: The
nearest ancestor takes precedence Selectors X > Y Specificity the order of
classes makes no difference

- most specific CSS Cascade CSS Typography Font Face Pixels vs. Ems Font Size
  font-size best practice: font-weight Measure Leading Alignment Vertical Rhythm
  Web Font Rendering Box Model Default width Default height Containing Block
  static or relative aboslute fixed There are 5 CSS positioning keywords
  http://www.alistapart.com/articles/css-positioning-101/ Relative Inherit
  Static absolute fixed Stacking Context (z-index) Other layout properties
  Overflow Clip Normal Flow RGBa best practice Safe Initial Values Grids
  interesting ideas of making grid proportions based on golden circle Layout
  techniques Anchor a block to the bottom right of it’s containing block and not
  have any other elements overlap it. Make a collection of inline elements take
  up 100% of the available horizontal space

    if using an elastic M based layout or even ﬁxed width then try to only use
    widths on the overall container and percentages inside, ! ! this will save
    time if needs change later down the line (watch your padding here though)

* set a max width of 100% on elastic designs to make sure your layout doesnt
  scale outside your viewport and cause a craul bar
* you should be upping and downing your font size all the time in every browser
  to see if behaves as expected ! ! !even if you are not doing an elastic
  design.
* vertigo is healthy, height on the web is diﬁcult to get right, one
  supprisingly common thing people do wrong is specify a height in pixcels on an
  element that contains text, ! ! this is either going to bleed outside in the
  case of overﬂow visible, in which case why are you setting the height or if
  you are using overﬂow hidden it vanish which is worse.

Some schools of thought think that writing all of your CSS on one line with the
selector and... rules is better practice because it focuses the author’s
concentration on the selectors. Whilst I agree that the selectors are indeed
more important, I disagree strongly with this method for maintainability. not
least because it introduces horizontal crawling. Multiple lines also work much
better with version control tools such as Subversion so you can identify the
actual rule that has changed. This also causes issues with debugging tools that
refer to speciﬁc line numbers, such as Firebug. Also, tools such as CSS Edit
alleviate the need for this solution by showing all of the selectors down the
left hand side. ...

this concept focuses on splitting your CSS in to separate ﬁles based on the
nature of the ... rules. so to colour, layout and typography (point at ducks)
and surprise ... I don’t like this method either. Splitting colour can make
sense for some sites (those that support themes) but I’m sure most pedantic
typographers would agree that typography is very strongly linked to layout.
Changing font size a"ects dimensions speciﬁed in ems, for example. By splitting
a rule block for a component it also adds extra mental overhead in deciding
which ﬁle a rule should go in.

when you ﬂoat something, specify it explicitly as display inline ... sounds odd
but this will ﬁx the double margin ﬂoat bug, where if you ﬂoat something and
have margins, IE will render twice the intended margins. The ﬁx wont a"ect any
other browsers at all. Even if you are not specifying margins on your ﬂoated
element, someone might want to in the future, that someone might not even be
testing in Internet Explorer (god forbid) or hopefully more likely they might
not know about the ﬁx

If you add these semantics into your HTML output, designers can later customize
the look of web sites and/or apps using pure CSS, which is a great advantage and
time-saver.

1. If possible, give every page's body a unique class:
   <body class='contactpage'> this makes it very easy to add page-specific
   tweaks to the style sheet:
2. body.contactpage div.container ul.mainmenu li { color: green }
3. When building menus automatically, add as much CSS context as possible to
   allow extensive styling later. For example:
4. <ul class="mainmenu">
5. <li class="item_first item_active item_1"> First item </li>
6. <li class="item_2"> Second item </li>
7. <li class="item_3"> Third item </li>
8. <li class="item_last item_4"> Fourth item </li>
9. </ul>
10. This way, every menu item can be accessed for styling according to its
    semantic context: Whether it's the first or last item in the list; Whether
    it's the currently active item; and by number.

http://www.nczonline.net/blog/2011/03/22/using-html5-semantic-elements-today/

presentational classes vs sass extends/mixins

1. -pc: presentational classes add presentation stuff the markup - bad
2. +pc are reusable - ish
3. pc _might_ means less text in css file if they are applied a lot e.g.
   clearfix could be applied a lot but the same codes might have to be re-typed
   multiple times if we use mixin. however the markup is bigger as it has to
   mention the new classname every time & messier too. also it’s not possible to
   see from the css what styles are applied to an element without using the
   markup
4. p.c. are unavoidable when changing stuff with JS???
5. are sass mixins, extend and import a complete replacement for semantic helper
   classes? or do they just include way more css code?
6. i like sass because it’s nesting lets me show css dependencies that exist
   anyway e.g. a nested div’s environment is setup by the contianing div (at
   least for inherited properties and stuff like width) so it’s shit if that’s
   not reflected in the CSS

your styles in the order "link-visited-hover-active", or "LVHA" for short. - all
the psuedo selectors have the same specificity so the order determins which one
will win (e.g. you mouse over a visited link and then :hover and :visited will
both be applied

On the other hand, IDs are great for linking and JS hooks. Put them in the HTML,
just don’t use them for styles.

The absence of assuredly-repeatable layout is a core design principle, but it’s
also the kind of thing that tends to get engineered away, particularly when
designers and the public both get involved. Its persistence hints that it’s
something valuable and even necessary. If I had to nominate one thing about the
Web for the title of “Most Under-appreciated”, I think this would be it.

Selectors X > Y the > in selectors only selects direct children whereas x y { }
doesn’t require that y be directly in x (it can be in a child of x)

use efficient selectors

given an existing css file how do i make it more efficent?

1. remove un-used selectors
2. optimise selectors for effiency
3. automated at end:
4. concatenate
5. minify
6. serve gzipped Specificity the order of classes makes no difference
 <div class=”foo bar”> === <div class=”bar foo”>
 most specific

7. inline style ! important
8. id ! important
9. classes (incl. psuedo classes & attributes) ! important
10. elements (incl. pseudo-elements) ! important
11. inline style
12. id
13. classes (incl. psuedo classes & attributes)
14. elements (incl. pseudo-elements)

least specific Setting image size in CSS The browser should be told wxh of each
image so it can layout page without repaints( which happen when the image turns
up at a diff size than the browser was expecting) should this happen in CSS? yes
-it’s a presentaitonal issue what kind of selector should I use to target the
images

# CSS Cascade

http://reference.sitepoint.com/css/cascade
https://www.readability.com/articles/mwtbmdss/

browser decides which rule should apply by combining: importance origin
specificity source order

inheritance

a selector is a combination of

1. elements: <div>, <p> etc.
2. psuedo-elements
3. classes .content
4. pseudo-classes :hover, :active
5. attributres
6. ids: #sidebar
