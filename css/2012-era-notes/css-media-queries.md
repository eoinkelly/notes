Media Queries
=============

*	http://www.w3.org/TR/css3-mediaqueries/

media query format:
	@media [only] {media-type} and ({feature}: {value}) and ...
	only keyword is optional

10 media-types: all, braille, embossed, handheld, print, projection, screen, speech, tty, tv
they can also be used in:
	@import url("blah.css") screen and (min-width: 100px);
	<link rel="stylesheet" href="blah.css" media="{media-query}" />

Display Area = the browsers viewport (sans chrome etc.)

Rendering Surface = the entire display area (including OS chrome etc.) of the screen.

13 media query features
	has min- and max- prefixes:
		width
		height
		aspect-ratio
		device-width
		device-height
		device-aspect-ratio
		color = num bits of color per component on the device
		color-index = num color lookup table entries of the output device
		monochrome
		resolution = density of the pixels in the device e.g. resolution: 300dpi;

	does not have min-, max- prefix:
		orientation = portrait|landscape
		scan = progressive|scan
		grid = is it a grid based display like feature phones w. one fixed font.

You can chain multiple queries with an 'and' operation
@media screen and (min-device-width: 400px) and (orientation: landscape) {  }

Not all devices support all queries!

Mobile safari actually by default renders your page at 980px wide and then shrinks down to 320px
You can control this behaviour with the viewport meta tag.
<meta name="viewport" content="initial-scale=1.0, width=device-width" />
	tells safari to make the default zoom level 100% and to make the width of the viewport in pixels to be the width of the device rendering survace in pixels

media queries should come last in the code to override any earlier declarations