Box Sizing
==========

Inbox
-----
http://paulirish.com/2012/box-sizing-border-box-ftw/
https://developer.mozilla.org/En/CSS/Box-sizing



box-sizing:  content-box | padding-box | border-box

	content-box
		default according to CSS spec
		width and height properties do not contain padding, border, margin
	padding-box
		width and height values include the padding box but not the border or margin box
		only properly supported in gecko, will possibly be removed from spec
	border-box
		width and height values include the padding and border box
		this box model is used by IE in quirks mode
		used by "legacy user agents" for replaced elements and input elements
			which ones?

min-height, max-height do not work if you set -moz-box-sizing: border-box
-moz-box-sizing does not apply to table cells
box-sizing doesn't work in IE6/7

Paul Irish recommendation:
/* apply a natural box layout model to all elements */

* {
	-moz-box-sizing: border-box;
	-webkit-box-sizing: border-box;
	box-sizing: border-box;
}

Even if I don't do a global reset, it might be useful for fluid compontents