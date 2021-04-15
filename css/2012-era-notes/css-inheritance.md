## CSS Inheritance

Inheritance are the rules for how styles apply to their children

? how does inheritance intersect with the cascade?
The cascade decides how selectors are sorted - inheritance decides what happens in the absence of selectors
Inherited styles have null specificity -  Any style applied directly to an element will always
override an inherited style so inheritance is consulted by the browser when it doesn't find any mathcing style declaration - if it doesn't find erither a stylesheet declaration or a value to inherit from it uses the spec Initial value.

Sources
* http://www.smashingmagazine.com/2010/04/07/css-specificity-and-inheritance/
* full list of what does/not inherit is at: http://www.w3.org/TR/CSS21/propidx.html
* http://www.w3.org/TR/css3-cascade/
* http://www.w3.org/TR/CSS21/propidx.html (full list of properties)

You can force a property to inherit using
	<property>: inherit
This doesnt' work at all in IE 6/7.
It works in IE 8 except for
	direction
	visibility

The final value of a property is dependant on a 4 step calculation:
1. Specified value
	The user agent determines whether the value of the property comes from a style sheet, is inherited or should take its initial value (from the CSS spec)
2. Computed value
	The specified value is resolved to a computed value and exists even when a property doesn’t apply. The document doesn’t have to be laid out for the computed value to be determined.
3. Used value
	The used value takes the computed value and resolves any dependencies that can only be calculated after the document has been laid out (like percentages).
4. Actual value
	This is the value used for the final rendering, after any approximations have been applied (for example, converting a decimal to an integer).

WHen an element inherits a value from it's parent, it inherits the "computed" value - this allows inheritance to work even for stuff not specified in any stylesheet

In general text related properties inherit and box related properties do not

Full list: http://www.w3.org/TR/CSS21/propidx.html

Properties that inherit:
	color
	font- (and related properties)
	letter-spacing
	line-height
	list-style (and related properties)
	text-align
	text-indent
	text-transform
	visibility (note this)
	white-space
	word-spacing

Properties that don't inherit:
	background (and related properties)
	border (and related properties)
	display
	float and clear
	height and width
	margin (and related properties)
	min- and max-height and -width
	outline
	overflow
	padding (and related properties)
	position (and related properties)
	text-decoration (note this)
	vertical-align
	z-index


Inheritance and the cascade: The nearest ancestor takes precedence (specificity rules do not apply to inheritance)

If you have two ancestors of an element trying to apply different values for the same property to their descendant using inheritance, the one that is closest to the descendant **in the document tree** wins. For example, if you set a different font-family on both the body and p elements, and you had a strong element nested inside a paragraph, that strong element would inherit the font-family from the p element over the one from the body element, because it is closer to the p element than the body element in the source.
]
At first glance this behavior may seem very obvious, but note that it holds true regardless of the order or specificity of the rules. So, continuing the same example, if the body rule's selector was amended to body#home so that it was more specific than the p rule, the strong text would still inherit the color from the p rule despite its weaker specificity.

When inheritance occurs, elements inherit computed values. The computed value from the parent element becomes both the specified value and the computed value on the child.

If a property isn’t set via the CSS cascade, and it’s not inherited, it’s up to the user agent to supply a default value for the property.

Every element in an HTML document will inherit all inheritable properties from its parent except the root element (html), which doesn’t have a parent.

? go through the w3c table and pick out all the elements for the list above

OLD NOTES
*************************************
Inheritance
In general text related properties inherit and box related properties do not

http://www.impressivewebs.com/inherit-value-css/

Properties that inherit:
1.         color
2.         font- (and related properties)
3.         letter-spacing
4.         line-height
5.         list-style (and related properties)
6.         text-align
7.         text-indent
8.         text-transform
9.         visibility
10.         white-space
11.         word-spacing
Properties that don't inherit:
1.         background (and related properties)
2.         border (and related properties)
3.         display
4.         float and clear
5.         height and width
6.         margin (and related properties)
7.         min- and max-height and -width
8.         outline
9.         overflow
10.         padding (and related properties)
11.         position (and related properties)
12.         text-decoration
13.         vertical-align
14.         z-index


Inheritance and the cascade: The nearest ancestor takes precedence
If you have two ancestors of an element trying to apply different values for the same property to their descendant using inheritance, the one that is closest to the descendant in the document tree wins. For example, if you set a different font-family on both the body and p elements, and you had a strong element nested inside a paragraph, that strong element would inherit the font-family from the p element over the one from the body element, because it is closer to the p element than the body element in the source.
At first glance this behavior may seem very obvious, but note that it holds true regardless of the order or specificity of the rules. So, continuing the same example, if the body rule's selector was amended to body#home so that it was more specific than the p rule, the strong text would still inherit the color from the p rule despite its weaker specificity.


When inheritance occurs, elements inherit computed values. The computed value from the parent element becomes both the specified value and the computed value on the child.






full list of what does/not inherit is at:
http://www.w3.org/TR/CSS21/propidx.html