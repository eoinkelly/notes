## Block formatting contexts

- the rules for float and clear only apply within the same block formatting
  context
- floats do not affect the layout of things in other block formatting contexts
- clear only clears past floats in the same block formatting contxt
- a block formatting context is responisble for the layout of it's children - it
  will completely contain it's children
- vertical margins will collapse ONLY if both boxes are in the same block
  formatting context

http://www.stubbornella.org/content/2009/07/23/overflow-a-secret-benefit/
http://www.stubbornella.org/content/2010/12/09/the-hacktastic-zoom-fix/
http://stackoverflow.com/questions/6196725/how-does-the-css-block-formatting-context-work
http://www.yuiblog.com/blog/2010/05/19/css-101-block-formatting-contexts/

There are 11 declarations in CSS to make a block establish a new block
formatting context: _ floats: float: left; float: right; _ absolutely positioned
elements, position: absolute; position: fixed; _ inline-blocks display:
inline-block; _ table-cells, display: table-cell; _ table-captions, display:
table-caption; _ elements styled with “overflow” (any value other than
“visible”) overflow: hidden; overflow: scroll; overflow: auto; _ tables display:
table; does not establish a new BFC itself but the anonymous boxes it creates
with 'display: table-cell' will so this establishes a new BFC indirectly _
fieldset element (in most but possibly not all browsers) not fully documented in
CSS 2.1 - the spec says not to rely on it

CSS that establishes a new block formatting context: float: left; float: right;
position: absolute; position: fixed; display: inline-block; display: table-cell;
display: table-caption; overflow: hidden; overflow: scroll; overflow: auto;
display: table;

If any of the above CSS declarations are applied to a block it establishes a new
block formatting context meaning it:

- is responsible for the layout of and will completely shrink-wrap all it's
  children
- it will not overlap the margin-box of floats that have already been positioned
  i.e. the flats margins will be respected implicit margins will be magically
  created for the elment establishing the BFC to make this happen because of
  this, negative margins should have no effect when applied to BFC estbablishing
  elments positioned beside floats

In normal flow, the left edge of the child box must touch the left edge of it's
containing block (left becomes right in rtl)

If you are establishing a new BFC you probably also want to trigger hasLayout in
IE overflow does not trigger hasLayout in IE 6 so we use zoom: 1 Good article on
hasLayout
http://www.yuiblog.com/blog/2010/05/19/css-101-block-formatting-contexts/
