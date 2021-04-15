Inbox
=====
http://designshack.net/articles/css/whats-the-deal-with-display-inline-block/

From CSS Systems Talk:
----------------------
setting display: inline on a float is considered a good practice because it fixes IE's double margin bug
likewise she recommends setting overflow: visible on buttons to avoid IE problems - normalize does this for me

you can consider a set of hTML files as "unit tests" for CSS - they show how everything should look before you hand-over the design
as well as road-testing the CSS they show how the markup should be laid out.



# CSS 2.1 Spec
## 8. Box Model
The CSS Box model says how rectangular elements are generated for elements in the document tree. The "Visual Formatting Model" says how those rectangular boxes are laid out.
boxes are always rectangular

The dimensions of the content area of a box depend on what it contains (other text, table, other boxes). The box model does not specify how content-width and content-height are calculated.


If the border area has transparent bits (e.g. a dashed border) the the background" of the content area will show through.
the "background" of a box covers content, padding and border. The margin area's background is **always** transparent.



Default width
for static and relatively positioned elements: the default width of a box are
width of containing block - padding,border,margin, scrollbars
boxes are positioned relative to their containing box but they can overflow it
a box will always fill up the full width of its containing box in static & relative formatting


Default height
no matter how content is positioned the height of the box will always be the height of  its contents unless the ‘height’, min-height or max-height property is specified. If an element has no content or contains just floated or absolutely positioned elements then it will have a height of 0


for float, absolutely and fixed position boxes the widht of ‘auto’ will mak the generated box shink to the intrinsic dimensions of its contents
IE has float bugs so always specify the width of a float explicitly


if you set content-width to 100% they you should have 0 padding, border, margin. otherwise it will overflow it’s containing box and disrupt the layout. for fluid layouts the


margin-top: <lenght>/%/auto
margin: top right bottom left;
margin top-bottom left-right;

 default margins for body element is 8 px in FF3.6
the vertical margins of the body element do not collapse (seem to in FF?






### The "containing block" (position:)
Elements are placed on the canvas in a particular ‘formatting context’ i.e. we specify dimensions and positioning relative to some containing block.
We can control what a box thinks is it’s containing block using the CSS "position:" property
	postion: static | fixed | relative | absolute;
A “positioned” element is one that has a non "static" positioning i.e. I have explicitly set it’s positioning rather than the default (which is "static")

* We need to identify what the browser thinks is the ‘containing block’ for each box so we can understand the layout.
* All relative dimensions we use for the box will be rendered relative to the continaing block.
* A child block can still overflow it’s parent.
* The containing block of the root element is called the ‘initial containing block’ and has the same dimensions as the viewport.(for continious media) and the page for paged media (print)

static or relative
	the containing block is formed by the edge of the content box of the nearest ancestor box whose display CSS property is one of:
1. block
2. inline-block
3. list-item
4. run-in
5. table
6. table-cell

aboslute
	containing block is the padding edge of the nearest ancestor who has position values of absolute, fixed, relative (i.e. a “positioned” element)

fixed
	the contianing block is the viewport


## 9. Visual Formatting Model

Block level element = elements of the source document formatted visually as blocks
display: block|list-item|table makes an element a block-level element
Block level box = a box that participates in a "block formatting context"
Block container box
	contains only block level boxes OR contains only inline-level boxes (hence sets up an inline-level formatting context)
	anonymous block-boxes are generated to make sure this rule is kept (e.g. if you mix inline and block-level elements in a container)

All block-level boxes are block container boxes except for table-boxes and replaced-elements

Block box := the box is a block-level box **and** a block-container box

Anonymous block boxes
	generated if inline-level and block-level elements appear in same container
	they inherit their properties from the enclosing non-anonymous box

Inline-level element = an element of the source document that does not form a new block of content. Instead the content is distributed in lines
display: inline|inline-block|inline-table will make an element inline-level
Inline-level elements generate inline-level boxes which are boxes that participate in an inline formatting context
Inline box = a box that is both inline-level and whose **contents** participate in its inline formatting context.

Some inline-level boxes are not inline boxes e.g. replaced inline-level elements, inline-block elements, inline-table elements. Thes are called "atomic inline-level boxes" because they participate in their inline level formatting context as a single opaque box

Anonymous inline boxes
	Any text that is directly contained inside a block container element must be treated as an anonymous inline element

### Block formatting context
Any box in a ‘block formatting context’ has the following i.e.
1. any element with display: block|list-item|table|run-in (sometimes) are laid out in block formatting context
2. a block level element with a display property other than ‘table’ generates a ‘principal box’ block.
1. a principal box will contain either block boxes OR inbline boxes buth never both.
2. if an element contains both anonymous block boexe are generated by the browser. an anonymous block inherits its properties from the enclosing non-anonymous box. any non-inherited values are set to their default valoues
1. the principal box becomes the containing block for non-positioned descendant boxes. it’s alos the box that’s affected if postion is set to anything other than ‘static’
2. in a ltr environment the left outer edge of each block box touches the left edge of the containing block
1. boxes are laid out vertically stargint at the top


### Inline formatting context
1. boxes are laid out horizontally starting at the top of the containing block
2. horizontal marings, padding and borders can exist between boxes but vertical margins are ignored for inline boxes
3. inline formatting will create anonymous boxes
4. the inline boxes that for a single line are enclosed by a rectangle that’s called a line box
5. boxes within the ‘line box’ are alligned vertically according to their vertical-align properties.
6. A line box is tall enough to accomodate all its inline boxes
7. when several inline boxes can’t fit into a single line box they are distributed over two or more stacked line boxes - you can control this stacking with ‘line-height’. if the computed value of line-height is greater than the vertical distance occupied by the inline boxes in a line box, the difference is added equally to the top and bottom. This means that if the words in your text are 14px and line height is 18px then the browser will add 2px to top and bottom to honour the line height. - this corresponds to the typographical concept of ‘half-leading’
8. when a single inline box can’t fit into a line box it is split and distrubuted over as many line boxes as required - margins, borders, padding are not applied when such splits occur\
9. if the total widht of the inline boxes is less than the width of the line box, the direction and text-align properties control how the boxes are distributed inside the line box
10. the left and right edges of a line box normall touch the edges of it’s containing block but when floats are in the way, the line boxes adjacent to floats are shortened


list formatting


an element with display: list-item generates a principal box just like any other block box but it also generates an additional box for the list marker. the box is generated outside the principal box. it cannot be styled independantly in css 2.1 (what about 3.0?)
there are 3 properties tha only apply to elements with a display of ‘list-tiem”
1. list-style-typpe
2. list-style-position
3. list-style-image
4. list-style


table formatting
tables contain
1. captions
2. row groups
1. rows
1. cells
1. column goups
1. columns
1. cells
tables are rendered as layers in a specific order from the bottom up:
1. table
2. column group
3. columns
4. row groups
5. rows
6. cells


the table model in html is row-centric.
a table can be included in a formatting context as a block-level or inline-level box. it can have padding, borders, margins
a table element generates an anonymous box that encompasses the table and it’s caption box
the internal elements of tables generate boxes that can have borders. cells can have padding but internal table objects don’t hav emargins.


replaced elements - a replaced element is one whose appearance and dimensions are defined by an external resource e.g. img, button, textarea, input, select, object
replaced elements can have intrinsic dimensions (e.g. the width and height of an image)


### Float & Clear
http://www.alistapart.com/articles/css-floats-101/
http://www.w3.org/TR/CSS2/visuren.html###floats
http://complexspiral.com/publications/containing-floats/


clearfix:
http://www.positioniseverything.net/easyclearing.html


A float is a box that is shifted to the left or right on the current line.


 When you clear an element, the browser adds enough margin to the top of the element to push the element’s top border edge vertically down, past the float


In the normal flow, each block element (div,p, h1, etc.) stacks on top of each other vertically, from the top of the viewport down.


Floated elements are first laid out according to the normal flow, then taken out of the normal flow and sent as far to the right or left (depending on which value is applied) of the parent element. In other words, they go from stacking on top of each other to sitting next to each other, given that there is enough room in the parent element for each floated element to sit.


clear says “do not allow the content of this element to flow around any element that has been floated to the <right|left>


clear says “do not allow the content of this element to flow down the  {opposite side} of any floated elements - if element A is floated left and element B has ‘clear: left’ set then the content of element B cannot flow down the right side of A


from w3c:
A float is a box that is shifted to the left or right on the current line. The most interesting characteristic of a float (or "floated" or "floating" box) is that content may flow along its side (or be prohibited from doing so by the 'clear' property). Content flows down the right side of a left-floated box and down the left side of a right-floated box. The following is an introduction to float positioning and content flow; the exact rules governing float behavior are given in the description of the 'float' property.
A floated box is shifted to the left or right until its outer edge touches the containing block edge or the outer edge of another float. If there is a line box, the outer top of the floated box is aligned with the top of the current line box.
If there is not enough horizontal room for the float, it is shifted downward until either it fits or there are no more floats present.
Since a float is not in the flow, non-positioned block boxes created before and after the float box flow vertically as if the float did not exist. However, line boxes created next to the float are shortened to make room for the margin box of the float.
A line box is next to a float when there exists a vertical position that satisfies all of these four conditions: (a) at or below the top of the line box, (b) at or above the bottom of the line box, (c) below the top margin edge of the float, and (d) above the bottom margin edge of the float.
Note: this means that floats with zero height or negative height do not move line boxes.
If a shortened line box is too small to contain any further content, then it is shifted downward until either it fits or there are no more floats present. Any content in the current line before a floated box is reflowed in the first available line on the other side of the float. In other words, if inline-level boxes are placed on the line before a left float is encountered that fits in the remaining line box space, the left float is placed on that line, aligned with the top of the line box, and then the inline-level boxes already on the line are moved accordingly to the right of the float (the right being the other side of the left float) and vice versa for rtl and right floats.
The border box of a table, a block-level replaced element, or an element in the normal flow that establishes a new block formatting context(such as an element with 'overflow' other than 'visible') must not overlap any floats in the same block formatting context as the element itself. If necessary, implementations should clear the said element by placing it below any preceding floats, but may place it adjacent to such floats if there is sufficient space. They may even make the border box of said element narrower than defined by section 10.3.3. CSS2 does not define when a UA may put said element next to the float or by how much said element may become narrower.






clear: left = “the top edge of this element must sit below any element that has “float: left” applied to it.
clear: right = “the top edge of this element must sit below any element that has “float: right” applied to it
clear: both = “the top edge of this element mus sit below any element that has “float: left” or “float: right” applied to it

so the clear property tells the browser how to position an element relative to the floats around it.




### CSS Positioning
There are 5 CSS positioning keywords
1. Static (default)
2. Relative
3. Absolute
1. Fixed (a particular instance of absolute)
1. Inherit
http://www.alistapart.com/articles/css-positioning-101/
Relative
1. Tell browser to use relative positioning using CSS
1. position: relative;
1. Browser calculates the postion of the box would be in normal flow.
2. Then offset it relative to this position based on the following CSS properties
1. top: 0px;
2. bottom: 0px;
3. left:
4. right: auto/pixels/%/ems/inherit
5. the default values of these properties is ‘auto’


1. When the box moves it leaves an empty gap in normal flow where it would have been.
2. If left and right conflict,  the browser will use the ‘direction’ of the containing block to decide which wins. If direction=ltr then left wins and right becomes -left.
3. If top and bottom conflict then top always wins.
4. Relative positioned boxes will appear in front of normal flow and floated boxes.
5. Relative positioning can be applied to boxes that have already been floated
6. Changing the position of a box with ‘relative’ has no affect on any of the boxes that follow it.
If you specify relative positioning but don’t actually move the block you get the handy side effect of the box becoming ‘positioned’ so it becomes the containing block for any absolutely positioned descendants


Inherit
Typically, position property elements do not naturally inherit their parent’s values—the static value is assigned if no position value is given. Ultimately, you can type inherit or the parent element’s value and get the same result.
Static
default positioning
absolute
completely removed from the document flow
does not affect subsequent elements at all
positioned wrt it’s containing block.
sets itself up as the containing block for it’s normal-flow children and for descendants whose position is ‘absolute’
overlapping elemnts will be stacked according to a ‘stacking context’


fixed
a subcategory of ‘absolute’ positioning
always has the viewport as it’s containing block
won’t move on screen even if document is scrolled
repeated on every page for paged media
not supported by IE6 and older


### Stacking Context (z-index)


Value:
	auto | <integer> | inherit
	Initial:
	auto
	Applies to:
	positioned elements
	Inherited:
	no
	Percentages:
	N/A
	Media:
	visual
	Computed value:
	as specified


z-index only works on positioned elements (position: relative|absolute|fixed)!
The default stack level is 0 unless you override it uzing z-index


z-index: default is ‘auto’ which means that the a box will have the same stack level as it’s parent and implies that the box does not start a new stacking context.


z-index: <integer> does 2 things when specified on a positioned element
1. specifies an explicit stack level for this box
2. causes the box to establish a new local stacking context. the box itself has the stack level of 0 in the new context


good link - suggests the dotted notation for understanding it e.g. 10.20 - but z-index can only be an integer so we can’t actually use these values


http://timkadlec.com/2008/01/detailed-look-at-stacking-in-css/


notes from this:
floats are stacked above other blocks by default
floats are stacked below inlines by default
Each box belongs to one stacking context. Each positioned box in a given stacking context has an integer stack level, which is its position on the z-axis relative other stack levels within the same stacking context. Boxes with greater stack levels are always formatted in front of boxes with lower stack levels. Boxes may have negative stack levels. Boxes with the same stack level in a stacking context are stacked back-to-front according to document tree order.


The root element forms the root stacking context. Other stacking contexts are generated by any positioned element (including relatively positioned elements) having a computed value of 'z-index' other than 'auto'. Stacking contexts are not necessarily related to containing blocks. In future levels of CSS, other properties may introduce stacking contexts, for example 'opacity'


Within each stacking context, the following layers are painted in back-to-front order.
1. the background and border of the element that establishes the stacking context
2. the stacking context of descentants with negative stack levels
3. block-level descentants in the normal flow
4. floated descendants and their contents
5. inline-level descendants in the normal flow
6. positioned descendants whose z-index is auto or 0
7. the stacking contexts of descentants with positive stack levels


Going from closest to user to farthest away from user the stacking order is:
1. Positioned elements with z-index of greater than 0, first in order of z-index from lowest to highest, then in order of appearance in source code
2. Positioned elements, in order of appearance in source code
3. Inline elements
4. Non-positioned floating elements, in order of appearance in source code
5. All non-positioned, non-floating, block elements in order of source code
6. Positioned elements with z-index of less than 0, first in order of z-index from highest to lowest, then in order of appearance in source code.
7. Root element backgrounds and borders


### Other layout properties
#### Overflow
Generally, the content of a block box is confined to the content edges (inside edge of padding) of the box In certain cases, a box may overflow, meaning its content lies partly or entirely outside of the box


overflow: { auto | hidden | scroll | visible | inherit } ; (default is ‘visible’)
auto = not specified in detail in CSS 2.1
hidden = content that overflows the elements content box is clipped
scorll = clips overflowing content but asks browser to provide scrollbars. the scrolling mechanism is visible even if the content doesn’t overflow
visible: allows overflowing content to be visible and may overlap other content


More info at:
http://www.w3.org/TR/CSS21/visufx.html




#### Clip


normally content is clipped at the inside edge of it’s padding - you can control this a bit with ‘overflow’ but ‘clip’ can allow you to have content bigger or smaller than the containing box


clip: { shape | auto | inherit } ;
auto (default) => no clipping
shape: only ‘rect(top, right, bottom, left)’ is allowed in css 2.1
top, bottom: relative to the top border edge
left, right relative to the left border edge in ltr environment (and right border edge in rtl)


sets the clipping region for any absolutely positioned elemnent
Only applies to absoutely positioned elements!
any part of an element which would render outside the clipping area will be invisible


clip: rect(50px 50px 30px 40px);
In CSS 2.1, the only valid <shape> value is: rect(<top>, <right>, <bottom>, <left>) where <top> and <bottom> specify offsets from the top border edge of the box, and <right>, and <left> specify offsets from the left border edge of the box.
Normal Flow
1. Boxes in normal flow are either
1. block
2. Inline


In block layout
1. boxes are laid out vertically starting at the top of a containing block
2. the vertical distance between two blocks is determined by their margin
1. vertical margins between adjacent blocks in normal flow always collapse. the resulting margin is the maximum of the adjoining margin widths
2. horizontal margins never collapse
3. vertical margins between a floated box and any other box do not collapse
4. vertical margins of absolutely positioned boxes do not collapse


In inline layout
1. boxes are laid out horizontally beginning at the top of the containing block
2. horizontal margins, borders, paddings are respected


### CSS Layout Categories
1. Liquid layout
2. Elastic layout
3. Fixed layout
1. try to just fix the outside container and then use % inside
2. set max-width: 100% to stop the container overflowing their screen if it happens to be smaller (media queries might be better at this
3. never use height in px on anything with text inside


a possible css code layout scheme
1. rule blocks ordered by specificity
2. classes grouped by the *effect* they create
3. ids grouped by the *component* they affect
4. elements grouped by the *type of element*

### Calculating the height of an Inline Level Box

Browsers flow inline-level boxes into a vertical stack of line boxes.

We get the height of a line box by
1. get the height of each inline-level box in the line box:
	replaced element, inline-block element, inline-table element => use height of the margin box
	inline box => use line-height
2. Align the inline boxes vertically according to 'vertical-align'
3. The height of the line-box is the distance between the uppermost box top and the lowermost box bottom
The height of an inline box is the smallest that will enclose all glyphs

If you set padding, border, margin on an inline level box, the horizontal stuff will be used but the vertical will not.
similarly setting an explicit width on an inline block will do nothing either
setting display: inline-block; allows you to set height, vertical padding, border, margin and have it work as expected.

display: inline-block isn't supported in IE 6/7

A strut = an invisible glyph of 0 width

L = leading
A = Ascender Height above baseline
D = Descender Height below baseline
Line-height = the CSS value

Leading = (Line-height -  (A+D) ) / 2


1. CSS adds equal space above and below the font to achieve the desired line-height.
2. Although margins, padding, borders of ‘inline level’ boxes are not used in the height calculation, they are still rendered by the UA

Note that ‘vertical-align’ applies to the ‘inline level’ box, not the ‘line box’

An inline-level element in the markup (e.g. <span>) generates 1 or more inline-level boxes. These boxes are grouped together into line-boxes using the vertical-align property to say how their heights align. The line-boxes are then stacked vertically.

vertical-align
	affects the vertical positioning inside a line box of the boxes generated by an inline-level element.
	Values of this property have different meanings in the context of tables


some values are relative to either
1. a parent inline element or
2. the strut of a parent block element


1. baseline
2. middle
3. sub
4. super
5. text-top
6. text-bottom
7. % = raise/lower the box by this amount (negative values OK)
8. lenght = raise/lower the box by this amount (negative values OK)

and some values are relative to the ‘line box’ itself
1. top
2. bottom

