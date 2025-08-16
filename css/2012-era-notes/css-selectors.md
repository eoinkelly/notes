#### CSS 2.1 Selectors

- [CSS 2 Summary Table](http://www.w3.org/TR/CSS2/selector.html#pattern-matching)
- [CSS 3 Summary Table](http://www.w3.org/TR/css3-selectors/#selectors)

A selector is a combination of

- elements: _div_, _p_ etc.
- psuedo-elements
- classes .content
- pseudo-classes :hover, :active
- attributres
- ids: #sidebar

Selectors are parsed right to left by the web browser. The first selector on
right is the "key selector"

- browser support for these: http://www.quirksmode.org/css/contents.html
- javascript polyfill for IE: http://selectivizr.com/

The 3 types of "combinator" in CSS 2.1 are

- whitespace
- >
-   -

A selector is a chain of simple selectors separated by a combinator

Simple selectors

elements classes ids pseudo-classes (7 types) allow you to style on things not
related to the structure of the document

    	pseudo-classes only for links
    		:link (matches if the link has not yet been visited)
    		:visited (matches if the link has been visited)

    	pseudo-classes theoretically for all elements (crappy IE6/7 support)
    		:active
    		:hover (The :hover pseudo-class applies while the user designates an element (with some pointing device), but does not activate it.))
    		:focus (The :focus pseudo-class applies while an element has the focus (accepts keyboard events or other forms of text input)
    		:lang
    		:first-child

    they can be strung together e.g. a:visited:hover

pseudo-elements (all in CSS 2.1) :first-line :first-letter :before :after

Universal selector used to style all elements on a page crappy performance -
avoid if possible \* { }

Combinators descendant indicated by a space (becuase of this, you need to be
carefully putting spaces around other selectors) div p { == all <p> elments that
have a <div> ancestor

    Child selectors (No IE6 support)
    	targets immediate descendants only
    	doesn't matter how much space either side of the > operator
    	div > p { }

    Adjacent Sibling selector (No IE6, mostly IE7-8 ok but problems with updating dynamic JS created pages)
    	p + p {
    	these elements must be adjacent in source order
    	doesn't matter how much space either side of the + operator
    	each element must share the same parent and one immediately preceeds the other in source order (ignoring non-element nodes like text and comments)

##### Attribute Selectors (no IE 6 support)

    div[title] = any div where the title attribute exists
    div[title="hello"] = any div where the title is exactly equal to 'hello'
    div[class~="foo"] = any div which has a classname "foo" (separated by spaces on either side)
    div[id="header"] = div#header
    div[class="warning"] = div.warning. It will not match <div class="warning foo"> - for that you need ~=

There cannot be a space between the element and the attribute selector e.g. _div
[id="header"]_ will not work - The space is interpreted as a descendant
selector.

##### Multiple classes (Buggy IE6 Support)

    p.cat.purring { } = a paragraph element that has both the 'cat' and 'purring' classes i.e. <p class="cat purring">

- The elements of the document tree that match a selector are called
  **subjects** of the selector.
- A selector consisting of a single simple selector matches any element
  satisfying its requirements.
- Prepending a simple selector and combinator to a chain imposes additional
  matching constraints, so the subjects of a selector are always a subset of the
  elements matching the last simple selector.
- One (and only one) pseudo-element may be appended to the last simple selector
  in a chain, in which case the style information applies to a subpart of each
  subject.

### CSS Outlines

- They are drawn **over** a box so they do not change layout
- They do not cause a reflow if they appear/disappear
- Outlines are drawn just outside the border edge of a box so they cover the
  margin area (if any)

When a page loads, the system focus (caret) is on the OS control that the page
lives in. When you hit tab,the first link on the page gets focus.
