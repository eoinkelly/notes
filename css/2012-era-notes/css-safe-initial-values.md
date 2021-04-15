Lea Verou alerted me to the CSS3 keyword initial which will set the CSS3
property to the initial value as defined in the spec. Sadly, the only browsers
that supports this seems to be Firefox (with a -moz- prefix) and Webkit’s Safari
and Chrome (yay!).

So until full support lands on all browsers for this amazing keyword and we
don’t have to support browsers that do not recognize that keyword, here are
some of the ways you can reset some CSS properties to their initial values:

Property Value
	background transparent (transparent stands for rgba(0, 0, 0, 0)) or none or 0 0
	border none or 0
	border-image none
	border-radius 0
	box-shadow none
	clear none
	color No value, so best option is to use inherit to cascade from a parent element’s color value.
	content normal
	display inline
	float none
	font normal
	height auto
	letter-spacing normal
	line-height normal
	max-width none
	max-height none
	min-width 0
	min-height 0
	opacity 1
	overflow visible
	page-break-inside auto
	position static (not relative)
	text-shadow none
	text-transform none
	transform none
	transition none
	vertical-align baseline 
	visibility visible
	white-space normal
	width auto
	word-spacing normal
	z-index auto (not none or 0)

