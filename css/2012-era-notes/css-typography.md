# Inbox

- http://coding.smashingmagazine.com/2011/03/14/technical-web-typography-guidelines-and-techniques/
  http://blog.kenneth.io/blog/2012/03/04/word-wrapping-hypernation-using-css/

# CSS Typography

CSS typography properties:

1. font-family
2. font-size
3. font-weight
4. color
5. width
6. line-height
7. text-align
8. text-indent
9. word-spacing
10. letter-spacing[a]

Typekit notes always use fallback fonts so

These classes get added to the html tag by typekit javascript

1. .wf-loading
2. .wf-active
3. .wf-inactive

it’s important to PLAN my typographical scale from the start!

when you choose font faces & sizes you need to be influenced by how the faces
look at certain sizes - there can be big differences between single pixel sizes!

Choosing typography for a site

1. pick the font faces you want to use
2. decide on what sizes & variants to use them at
3. be influenced by how the font looks at various sizes e.g. 15px might be bad
   and 16px good in one face but could be the opposite in another
4. prob best to use a known scale to relate sizes together
5. ? a typography test sheet of lorem?
6. then decide on spacing
7. line height
8. margins, borders, padding
9. then decide on decoration
10. colours
11. graphical do-das

fonts on linux:
http://sixrevisions.com/web_design/a-web-designers-guide-to-linux-fonts/

Sizing type on the web is tricky because of the limited resolution involved. One
pixel of font-size up or down can completely change how a typeface—and thus a
whole text—looks.
http://blog.typekit.com/2011/01/26/css-properties-that-affect-type-rendering/
http://snook.ca/archives/html_and_css/font-size-with-rem

what dims should i use for web fonts? %, ems, where does line height help

Gutter = left margin Font Face Choose a readable font for blocks of text. CSS:
font-face

font-kerning All quality fonts have kerning tables which nudge letter
combinations closer together or further apart.

Pixels vs. Ems We use the px unit of measurement to define font size, because it
offers absolute control over text. We realize that using the em unit for font
sizing used to be popular, to accommodate for Internet Explorer 6 not resizing
pixel based text. However, all major browsers (including IE7 and IE8) now
support text resizing of pixel units and/or full-page zooming. Since IE6 is
largely considered deprecated, pixels sizing is preferred. Additionally,
unit-less line-height is preferred because it does not inherit a percentage
value of its parent element, but instead is based on a multiplier of the
font-size. Font Size CSS: font-size

use a constant vertical grid for “related” type. sidebars etc. do not have to
align to the same grid (unless their content refers to conent in the main grid)

http://typekit.assistly.com/customer/portal/articles/6855-Using-multiple-weights-and-styles -
how ie doesn’t honour all sizes

1 pt = 1/72 inch 1 pica = 12 pt 1 inch = 6 picas 1 em = the distance that’s
horizontally equivalent to the typesize in points e.g. for 12pt text 1 em = 12
pt. 1 em is approximately equal the height of an uppercase M in the font at that
size but this is only an approximation. the em is a sliding measure

from the spec: The 'em' unit is equal to the computed value of the 'font-size'
property of the element on which it is used. The exception is when 'em' occurs
in the value of the 'font-size' property itself, in which case it refers to the
font size of the parent element. It may be used for vertical or horizontal
measurement. (This unit is also sometimes called the quad-width in typographic
texts.)

% values on the font-size property are a % of inherited font size.

so be careful what elements you set font-size & line-height on if you are using
em/% - it should only be set on <body> and elements that do not contain other
block level elements e.g. <p> but not <div> or <form> font-size best practice:

Specify size in ems as it scales well A good minimum size is 14px - you should
only set the <body> font in pixels, all other font sizes should be in ems so
they are relative to the body size. will browser resize text if font set in
pixels?

and as such is the best choice for font-sizes on the web as it will change
properly if the user resizes their font.

since fonts are inherited from parent in CSS, if you have nested blocks and use
% font sizes then each block will get progressively smaller

best to think in pixels but specify size in ems - here’s how to convert (1 /
parent font size in pixels ) \* desired font-size in pixels = size in ems

when coming up with relative text sizes you should use a scale e.g fibonnaci
e.g. the size ratio of H1 > H2 > H3 should follow this sequence

The 62% trick

1. Browsers have a common default size of 16px for text.[b]
2. Set the body to a font- size of 62.5%, resetting everything to 10px. From
   this point, the calculations are similar for _ direct descendants_ of the
   body, for example, 12px = 1.2em; 8px = 0.8em; and so forth. Deeper nested
   elements are (still) relative, of course.

font-weight 'font-weight' Value: normal | bold | bolder | lighter | 100 | 200 |
300 | 400 | 500 | 600 | 700 | 800 | 900 | inherit Initial: normal Applies to:
all elements Inherited: yes Percentages: N/A Media: visual Computed value: see
text

keywords normal === 400 bold === 700 bolder, lighter = relative to the weight
inherited from parent

“normal” => the ‘normal’ blackness of that font family - the actual blackness
can differ across families

opentype fonts use the nine value scale directly - non-open type fonts are
assigned weights based on a guess by css

There is no guarantee on how a UA will map font faces within a family to weight
values. The only guarantee is that a face of a given value will be no less dark
than the faces of lighter values.

the bold, normal keywords are NOT related to the font name e.g. ‘Adelle Bold’
these numbers “frequently” correspond to these keywoards for font size.

1. 100
2. 200
3. 300
4. 400 = normal
5. 500
6. 600
7. 700 = bold
8. 800
9. 900

IE can only display a maximum of 4 fonts per font family (font-weight or
font-style)

oblique = slanted roman letter, not a true italic

some adobe fonts have ‘optical’ variations that are adjusted to look best
rendered in a particular size range Although any of the fonts may be used at any
size, the intended point sizes for the designs of this family are:

1. Caption: from 6.0 to 8.0 points
2. Body Text: from 8.0 to 14.0 points
3. Subhead: from 14.0 to 24.0 points
4. Display: from 24.0 to 72.0 points

text-rendering: optimizeLegibility an svg property that turns on common
ligatures not sure of support

CS3, FF4 font-feature-settings: “a bunch of fairly raw opentype properties” -
this actually works today in ff4 font-size-adjust: <num> - think it works in ff4
allows you to account for differences in font metrics as you move down the
font-stack

css3 properties font-stretch font-variant-ligatures font-variant-alternates
font-variant-numeric Measure measure is the lenght of the line of text 40
characters is a good lenght, any longer gets hard to read CSS: width property
Ideally set total page width in ems and then set columns in %

Leading Leading is the space between lines CSS: line-height can specify it as a
dimensionless number in which case it becomes a multiple of font-size which is
quite flexible

Alignment CSS: text-align = left, center, justified, right Only justify if the
measure is sufficiently long and ideally you have a good hyphenation algorithm

http://webtypography.net/toc#Horizontal_Motion

/_ browser default is 16px, this sets it to 12px _/ body { font-size: 75%; } /_
this is ignored by IE 6 _/ html>body { font-size: 12px; }

/_ line height a multiplier of font-szie _/ p { line-height 1.5em; }

in order for typographic integrity to be maintained we need to specify all our
vertical measurements in ems including margin-top and margin-bottom and the
width of the border at top and bottom

Vertical Rhythm http://24ways.org/2006/compose-to-a-vertical-rhythm

by default browser uses spacing of 1em between paragraphs but this will break
our typographical rhythm so change it[c] p { font-size:1em; margin-top: 1.5em;
margin-bottom: 1.5em; }

When there is a change in text size, perhaps with a heading or sidenotes, the
differing text should also take up a multiple of the basic leading. If p
font-size is 12px and line-height (leading) is 18px. then the h1 above it should
take up a multiple of 18px in vertical space (including margin-top and margin
bottom

if we set h1 font-size = 20px then then the line-height must be 36px and
margin-top and margin-bottom must also be 36px

font sizes set in pixels will not resize in IE 6/7

The key implication is that line-height should be the same regardless of the
size of the text (so that line-height, or the vertical grid, remains consistent,
regardless of font size).

the list apart article presents this as the best practice - everything except
<body> uses ems for vertical measurements. body set to 100% uses default
font-size of 16px and because it’s not specified in pixels will scale correctly
in IE 6/7. line height specified in ems so it scales correctly. if line-height
was specified as a dimensionless number it would scale with font-size which
would break the vertical rhythm of page[d]

supposed best practice: body { font-size:100%; line-height:1.125em; /_
16×1.125=18 _/ }

.bodytext p { font-size:0.875em; }

.sidenote { font-size:0.75em; }

The main principle of the baseline grid is that the bottom of every line of text
(the baseline) falls on a vertical grid set in even increments all the way down
the page. Imagine those oldBig Chief ruled writing pads they gave you in grade
school to practice penmanship and you’ve got the basic idea. The magical end
result is that all the text on your page lines up across all the columns,
creating a harmonious vertical rhythm.

# Web Font Rendering

go through all the typekit blog posts on it why do some fonts render so badly
online?

http://blog.typekit.com/2010/12/17/type-rendering-review-and-fonts-that-render-well/
typekit hint: All of the fonts tagged Paragraph have been manually hinted to
look great on screen at text sizes in every major browser/OS mix. Use these
recommendations to make informed decisions about how well a font will render,
knowing that they are backed up by Typekit’s font diagnostic tools.

ook closely at the detail images and you’ll notice that some pixels are tinted
with color. This is called sub-pixel antialiasing, which uses the
vertically-oriented thirds of red, green, and blue light within each pixel of an
LCD screen to effectively triple the horizontal resolution of text. At reading
distance from the screen, our eyes cease to notice colored sub-pixels and we
instead see sharper text than we otherwise would. Or at least, that’s the idea.
For some folks, the sub-pixels are always noticeable and this amounts to an
annoying color fringe effect on the edges of letters.
