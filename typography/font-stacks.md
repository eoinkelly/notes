# font stacks

* http://www.awayback.com/revised-font-stack/
* http://awesome-fontstacks.com/
* http://georgebutler.com/blog/typography/10-definitive-web-font-stacks-for-designers/

## suggested by http://www.sitepoint.com/eight-definitive-font-stacks/

```
The Times New Roman-based serif stack:
	font-family: Cambria, "Hoefler Text", Utopia, "Liberation Serif", "Nimbus Roman No9 L Regular", Times, "Times New Roman", serif;

A modern Georgia-based serif stack:
	font-family: Constantia, "Lucida Bright", Lucidabright, "Lucida Serif", Lucida, "DejaVu Serif", "Bitstream Vera Serif", "Liberation Serif", Georgia, serif;

A more traditional Garamond-based serif stack:
	font-family: "Palatino Linotype", Palatino, Palladio, "URW Palladio L", "Book Antiqua", Baskerville, "Bookman Old Style", "Bitstream Charter", "Nimbus Roman No9 L", Garamond, "Apple Garamond", "ITC Garamond Narrow", "New Century Schoolbook", "Century Schoolbook", "Century Schoolbook L", Georgia, serif;

The Helvetica/Arial-based sans serif stack:
	font-family: Frutiger, "Frutiger Linotype", Univers, Calibri, "Gill Sans", "Gill Sans MT", "Myriad Pro", Myriad, "DejaVu Sans Condensed", "Liberation Sans", "Nimbus Sans L", Tahoma, Geneva, "Helvetica Neue", Helvetica, Arial, sans-serif;

The Verdana-based sans serif stack:
	font-family: Corbel, "Lucida Grande", "Lucida Sans Unicode", "Lucida Sans", "DejaVu Sans", "Bitstream Vera Sans", "Liberation Sans", Verdana, "Verdana Ref", sans-serif;

The Trebuchet-based sans serif stack:
	font-family: "Segoe UI", Candara, "Bitstream Vera Sans", "DejaVu Sans", "Bitstream Vera Sans", "Trebuchet MS", Verdana, "Verdana Ref", sans-serif;

The heavier “Impact” sans serif stack:
	font-family: Impact, Haettenschweiler, "Franklin Gothic Bold", Charcoal, "Helvetica Inserat", "Bitstream Vera Sans Bold", "Arial Black", sans-serif;

The monospace stack:
	font-family: Consolas, "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", Monaco, "Courier New", Courier, monospace;
```

A lot of fonts undergo name changes over the different operating systems, which is why I’ve listed them all; for example, Palatino, Palatino Linotype, Palladio, URW Palladio L, and Book Antiqua are all (more or less) the same font.

Additionally, some thought has been put into which variants have a larger Unicode character family, and which then precede its cousin in its stack (kudos to my friend, Tommy Olsson for reminding me of this important font element).
Klein states that this stack combines humanist typefaces – Frutiger, Calibri, Gill Sans, and Tahoma – and realist/classical typefaces such as Helvetica. He would separate the two, but it’s important for you to decide what’s best for your purpose; combining humanist and realist/classical typefaces might be suitable.



## http://css-tricks.com/snippets/css/font-stacks/
```css
/* Times New Roman-based stack */
font-family: Cambria, "Hoefler Text", Utopia, "Liberation Serif", "Nimbus Roman No9 L Regular", Times, "Times New Roman", serif;

/* Modern Georgia-based serif stack */
font-family: Constantia, "Lucida Bright", Lucidabright, "Lucida Serif", Lucida, "DejaVu Serif", "Bitstream Vera Serif", "Liberation Serif", Georgia, serif;

/* Traditional Garamond-based serif stack */
font-family: "Palatino Linotype", Palatino, Palladio, "URW Palladio L", "Book Antiqua", Baskerville, "Bookman Old Style", "Bitstream Charter", "Nimbus Roman No9 L", Garamond, "Apple Garamond", "ITC Garamond Narrow", "New Century Schoolbook", "Century Schoolbook", "Century Schoolbook L", Georgia, serif;

/* Helvetica/Arial-based sans serif stack */
font-family: Frutiger, "Frutiger Linotype", Univers, Calibri, "Gill Sans", "Gill Sans MT", "Myriad Pro", Myriad, "DejaVu Sans Condensed", "Liberation Sans", "Nimbus Sans L", Tahoma, Geneva, "Helvetica Neue", Helvetica, Arial, sans serif;

/* Verdana-based sans serif stack */
font-family: Corbel, "Lucida Grande", "Lucida Sans Unicode", "Lucida Sans", "DejaVu Sans", "Bitstream Vera Sans", "Liberation Sans", Verdana, "Verdana Ref", sans serif;

/* Trebuchet-based sans serif stack */
font-family: "Segoe UI", Candara, "Bitstream Vera Sans", "DejaVu Sans", "Bitstream Vera Sans", "Trebuchet MS", Verdana, "Verdana Ref", sans serif;
/* Impact-based sans serif stack */
font-family: Impact, Haettenschweiler, "Franklin Gothic Bold", Charcoal, "Helvetica Inserat", "Bitstream Vera Sans Bold", "Arial Black", sans serif;

/* Monospace stack */
font-family: Consolas, "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", Monaco, "Courier New", Courier, monospace;
```

## suggested by unit interactive

	http://unitinteractive.com/blog/2008/06/26/better-css-font-stacks/
	p – balanced for paragraphs or body copy
	t – balanced for headlines or titles

```
Arial, “Helvetica Neue”, Helvetica, sans-serif - p, t
Baskerville, “Times New Roman”, Times, serif - p
Baskerville, Times, “Times New Roman”, serif - t
Cambria, Georgia, Times, “Times New Roman”, serif - p, t
“Century Gothic”, “Apple Gothic”, sans-serif - p, t
Consolas, “Lucida Console”, Monaco, monospace - p, t
“Copperplate Light”, “Copperplate Gothic Light”, serif - p, t
“Courier New”, Courier, monospace - p, t
“Franklin Gothic Medium”, “Arial Narrow Bold”, Arial, sans-serif - p, t
Futura, “Century Gothic”, AppleGothic, sans-serif - p, t
Garamond, “Hoefler Text”, Times New Roman, Times, serif - p
Garamond, “Hoefler Text”, Palatino, “Palatino Linotype”, serif - t
Geneva, “Lucida Sans”, “Lucida Grande”, “Lucida Sans Unicode”, Verdana, sans-serif - p
Geneva, Verdana, “Lucida Sans”, “Lucida Grande”, “Lucida Sans Unicode”, sans-serif - t
Georgia, Palatino,” Palatino Linotype”, Times, “Times New Roman”, serif - p
Georgia, Times, “Times New Roman”, serif - t
“Gill Sans”, Calibri, “Trebuchet MS”, sans-serif - p
“Gill Sans”, “Trebuchet MS”, Calibri, sans-serif - t
“Helvetica Neue”, Arial, Helvetica, sans-serif - p
Helvetica, “Helvetica Neue”, Arial, sans-serif - t
Impact, Haettenschweiler, “Arial Narrow Bold”, sans-serif - p, t
“Lucida Sans”, “Lucida Grande”, “Lucida Sans Unicode”, sans-serif - p, t
Palatino, “Palatino Linotype”, Georgia, Times, “Times New Roman”, serif - p
Palatino, “Palatino Linotype”, “Hoefler Text”, Times, “Times New Roman”, serif - t
Tahoma, Geneva, Verdana - p
Tahoma, Verdana, Geneva - t
Times, “Times New Roman”, Georgia, serif - p, t
“Trebuchet MS”, “Lucida Sans Unicode”, “Lucida Grande”,” Lucida Sans”, Arial, sans-serif - p
“Trebuchet MS”, Tahoma, Arial, sans-serif - t
Verdana, Geneva, Tahoma, sans-serif - p
Verdana, Tahoma, Geneva, sans-serif - t
```
