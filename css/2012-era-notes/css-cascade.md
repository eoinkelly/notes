## CSS Cascade

Sources http://reference.sitepoint.com/css/cascade

The cascade = how CSS decides which style to apply. The cascade is the algorithm
that calculates a "weight" for a particular declaration e.g. font-size: 20px;
The declaration with the highest weight wins if there is a conflict.

The cascade is calculated individually for every possible declaration what would
happen if hte default UA stylesheet was blank? are there some hard-coded rules?

Nomenclature selector rule = declaration

For a given property, find all declarations that apply to a specific element.
Sort the declarations according to their levels of importance, and origins. Sort
declarations with the same level of importance and origin by selector
specificity. Finally, if declarations have the same level of importance, origin,
and specificity, sort them by the order in which they’re specified; the last
declaration wins.

The CSS Spec says what the default values of each declaration are but UA
stylesheets often override this e.g. CSS Spec: text-decoration: none; UA
Stylesheets: text-decoration: underline;

See the downloaded default stylesheets for various browsers and the CSS 2.1
spec. Good post with links to default UA stylesheets for many browsers and the
suggested W3C spec defaults.

Chrome User Stylesheet:
`%userprofile%\appdata\local\google\chrome\user data\default\user stylesheets\Custom.css`
Firefox User Stylesheet: IE User Stylesheet:

Declarations in a `<style>` tag in a HTML file do not automatically override
declarations in a linked CSS file - only the "creator" of the declaration
(user|author|UA) and the source order matters.

There are 4 elements to the cascade:

1. importance (any declarations marked !important come first. !important
   user-styles > !important author-styles). note that this gives ultimate
   control to the users - none of my styles can override a declaration in a user
   stylesheet marked !important
2. origin (author style > user style > UA style)
3. specifity (more specific wins) - only used if styles have same importance &
   origin
4. source order (latest defined wins) - only used if declarations have same
   importance, origin & specificity.

Comparison table of UA stylesheet defaults:
http://css-class.com/test/css/defaults/UA-style-sheet-defaults.htm

## Specificity

The order of classes in the class attribute makes no difference e.g. class="foo
bar" = class="bar foo"

Specificity is not calculated in base 10 but a high, unspecified, base number.
This is to ensure that a highly specific selector, such as an ID selector, is
never overridden by lots of less specific selectors, such as type selectors.
However, if you have fewer than 10 selectors in a specific selector, you can
calculate specificity in base 10 for simplicity’s sake.

<img src="./CSS/images/specificity.jpg" />

? find nicole sullivan !important specificity slide

    <div class=”foo bar”> === <div class=”bar foo”>

    *most specific*
    	inline style
    	id
    	classes (incl. psuedo classes & attributes)
    	elements (incl. pseudo-elements e.g. :before|:after)
    *least specific*

### Pseudo elements

http://www.w3.org/TR/CSS21/selector.html#pseudo-elements
http://css-tricks.com/9516-pseudo-element-roundup/

CSS introduces the concepts of pseudo-elements and pseudo-classes to permit
formatting based on information that lies outside the document tree. Neither
pseudo-elements nor pseudo-classes appear in the document source or document
tree. Act like you have injected new HTML into the page

Pseudo-classes are allowed anywhere in selectors while pseudo-elements may only
be appended after the last simple selector of the selector. Pseudo-elements can
only be applied to the last "simple selector in a chain" ? dig deeper

- Pseudo-element and pseudo-class names are case-insensitive.
- Some pseudo-classes are mutually exclusive, while others can be applied
  simultaneously to the same element. In case of conflicting rules, the normal
  cascading order determines the outcome.

CSS 2.1 Pseudo Elements :before (No IE 6/7 Support) :after (No IE 6/7 Support)
:first-line (All broswers) :first-letter (All Browsers)

CSS 2.1 Pseudo Classes :link :visited :lang :hover :active :focus :first-child

relying on the order of selectors can make my stylesheets brittle - what if
somebody adds a new one later?
