# Typography

## What is a pt

* Historical typographical unit
* There have been many versions of it over time
* Most software uses pt as the font size e.g.
    * MS Word
    * Adobe products

```
1 Fourneri point = ???
1 Didot point = 0.375mm # a historical pt unit
1 Postscript (or DPT) = 0.353mm (the modern pt)

Using modern Postscript points:
1 pt = 1/72 in (is this exact or approximate?)
    it is exact in CSS but not sure about other contexts

=> 72pt  = 1.00in
=> 10pt =  3.53mm
```

## em, em box, em dash, en dash

* https://fonts.google.com/knowledge/glossary/emvv
* Historically the em unit was named after with width of capital M but that doesn't apply anymore
* The pt size of a font matches it's "em square" which is a rectangle within which digital fonts are drawn
* The em square is a box that each glyph is sized relative to.
* The boundary typically sits just above cap-height and just below descender
* The "em dash" is supposed to be exactly the width of the em box but not all fonts do this.
* The en-dash takes up half the width of the em-box


## Very old notes

>*Note*
> The notes below are super, adorably,  old

Font sources

* http://www.smashingmagazine.com/2010/12/17/25-new-free-high-quality-fonts-typography/
* http://www.google.com/webfonts/v2#ChoosePlace:select

> A good rule of thumb is that the closer to one another are the font sizes of
> the various levels, the more elegant the overall impression will be. If you're
> after a disjointed or modern feel, try using font sizes that are further apart
> on the scale.
> - Andy Hume

### Reading List

* good type book by jon tan http://www.amazon.com/Detail-Typography-Jost-Hochuli/dp/0907259340


* https://designfestival.com/what-characterizes-a-good-screen%C2%A0font/
* https://designfestival.com/the-anatomy-of-a-letterform/
* https://designfestival.com/how-to-make-your-typefaces-play-nicely-together/
* https://designfestival.com/common-web-fonts-demystified/

Things that make a good web font:

1. large x-height
1. clear letter spacing
1. large punch width

Categories of font

1. serifs
1. old style - humanist -1400's
1. transitional 1700's
1. modern late 1700's
1. slab
1. sans serif (older -> newer)
1. grotesque
1. neo grotesque
1. humanist
1. geometric
1. scripts
1. formal
1. casual
1. display (unsuitable for body copy)
1. formal
1. informal
1. blackletter
1. proportional vs monospaced


Font examples:

* Who hasn't heard of Helvetica? It's probably the most recognised classic typeface. Originally designed 1957 by Swiss designer Max Miedinger and Eduard Hoffmann Bodoni
* Bodoni is a serif typeface designed by Giambattista Bodoni in 1798. Iconically used for the Goodfellas gangster movie poster.
* Clarendon is a fantastically fat slab serif, created by Robert Besley in 1845.
* Akzidenz Grotesk was designed in 1896 by the H. Berthold AG type foundry and was used as inspiration in 1957 for the Helvetica typeface.
* Avenir is a geometric sans-serif typeface designed by Adrian Frutiger (recall the name? He's also famous for another classic font, I'll let you guess which one).
* FF Din is a relatively new typeface compared to the veterans mentioned so far with it being created in 1995 by Albert-Jan Pool. One of my personal all time favs.
* Futura is another widely used font that can be seen in countless logos. It was originally created in the 1920′s by Paul Renner.
* News Gothic was designed by Morris Fuller Benton in 1908, and has the most amazing fact of being the typeface used during the Star Wars opening credits.
* Remember Adrian Frutiger? Needless to say he was also the designer behind the classic Frutiger typeface.
* FF Meta is another member of the modern classic collection, designed by Erik Spiekermann in 1986. Meta is another of my personal favourites.
* Designed by Eric Gill in 1926, Gill Sans is another widely used font in graphic
design. Famous uses include the London Underground signage.
* Probably one of the most famous names for serif fonts, Garamond can be found in
a number of variations. Overall it's commonly used for body text in books.
* Mrs Eaves is a recent design of a traditional serif typeface style by Zuzana Licko in 1996.
* Dax, now famously used for the branding of UPS, was originally created by Hans Reichel.
* Yes, that one that appears as default in your Adobe apps. Myriad was designed specifically for Adobe by Rober Slimback and Carol Twombly.
* VAG Rounded, aka VAG Rundschrift makes an appearance in countless web2.0 logos, but was originally designed in 1979 as a corporate identity for Volkswagen.
* Optima is a German typeface designed by Hermann Zapf. It's a sans-serif font on a low calorie diet with it's thinning lines around the letterforms.
* Originally created for the Avant Garde Magazine, the Avant Garde font is now seen in plenty of printed headlines.
* Univers is another classic by Adrian Frutiger. It has typical swiss styling and is often confused with Helvetica, or Akzidenz Grotesk.
* Rockwell is probably the most iconic slab-serif font. Designed by Monotype in 1934 it's now used in all kinds of designs for an eye grabbing impact.
* Minion is a popular serif font designed by Robert Slimback in 1990 for Adobe.  Cleverly named after the traditional naming system for type sizes.
* Sabon is another old style serif, this one however was designed by Jan Tschichold in
Germany and released by Linotype, Monotype and Stempel in 1967.
* Cocon Cocon is
the most recent of this collection, designed in 1998 by Evert Bloemsma, Cocon
features some cool letterforms with sleek points.
* Rotis Rotis was built with
exceptionally high legibility in mind. Designed by Otl Aicher in 1988.
* Bembo Bembo is one of the most popular typefaces used in books, first printed in 1496
and brought to life for the modern age in 1929.




Some of the design classes I took in college got pretty deep into the anatomy and terminology of type. Many people can already identify serifs, ascenders, and descenders, but for one class, we had about 100 terms to memorize.  While web designers probably don't need to get into that much detail, it is important that you know some basic terminology if you're going to be talking about, and working with, type. The image below identifies the most important aspects of type. Each is explained below.

1. Baseline The baseline is the imaginary horizontal line on which most
characters sit. The only character that hangs below the baseline in Figure 4.5
is the lowercase "q."
2. Cap height The cap height or capline is another imaginary line. This one
marks the height of all capital letters in a typeface.  Notice that the cap
height is below the maximum height of the typeface.
3. Crossbar A stroke that connects two lines in the capital letterforms of "A"
and "H" is called a crossbar. A horizontal stroke that does not connect two
lines, like the one in the lower case "f" or "t," is known as a cross stroke.
4. Serif Serif is the name given to the finishing strokes at the bottoms and
tops of certain typefaces. I'll talk more about serifs when we get into typeface
distinctions.
5. Meanline Another imaginary horizontal line that marks the top
edge of the lowercase letters is the meanline. Contrary to the way it sounds,
the meanline isn't always exactly centered between the baseline and the cap
height.
6. Bowl The bowl of a letter is the rounded curve that encloses
negative space in a letterform. Examples of bowls can be seen in the letters
"D," "o," and "g."
7. Descender The lower portion of the lowercase letters "g,"
"j," "p," "q," and "y" that extend below the baseline of a typeface is known as
the descender. The only other characters that typically extend below the
baseline are the old-  style numerals in some typefaces. These types of
numerals, examples of which from the Georgia typeface can be seen in Figure 4.6,
were thought to blend better with lowercase roman numerals, and they look
particularly good when used within a body of text.
8. Counter The negative space within a letter is called the counter. In some
letters, like "A," "o," and "P," the counter is fully enclosed. The non-  closed
negative spaces in letters like "G," "u," and "c" are also known as counters.
9. Stem A stem is the main vertical or diagonal stroke in a letterform. These
include the vertical portions of the letters "I" and "H," as well as all of the
stokes in the letter "W."
10. Tittle This is probably my favorite typeface term.
Tittle is the name given to the dot above the lowercase "j" and "i."
11.  Terminal The end of a stem or stroke that has no serif is known as a terminal.
Even the ends of some serif typefaces have terminals, as you can see in the
letter "c" in Figure 4.6.
12. Ascender The tops of most lowercase letters form
an imaginary line that's known as the meanline. Some lowercase letters have an
ascender, which is an extension that rises above the meanline. Those letters are
"b," "d," "f," "h," "k," "l," and "t."
13. Leg The lower, angled strokes seen in
the letters "K," "R," and "Q" are known as legs. These are also sometimes
referred to as tails.
14. Ligature You may not have noticed in Figure 4.5, but
the "f" and "i" of the word "fix" are actually combined into one character. This
combination of characters is known as a ligature. Ligatures exist to give the
spacing between certain characters a greater aesthetic balance, as Figure 4.7
illustrates.
15. x-  height The x-  height is exactly what you would expect it to be: the
height of the lowercase x in a typeface. Essentially the x-  height is the
distance between the baseline and the meanline of a typeface. Although it's not
very practical, you can actually use x-  height as a relative unit of
measurement in CSS (ex).


The features that define a good screen font include:

1. low contrast and simple strokes with a consistent weight and thickness
1. generous x-  height
1.  generous width and letter spacing
1. generous punch width (space within letters).

With those points in mind, you should be in a position to begin looking beyond
the common Web fonts. So, what are the design principles you should bear in mind
when thinking about typeface choice?  Size Type size is one of the easiest
factors to control on-  screen, and as such it is often not given the importance
that it deserves in the field of Web typography. Size is an important device for
giving your content a hierarchy, and the relative sizes of your headings, body
text, and footers can have a big influence on the overall feel of a page. Type
size is also very closely linked to other characteristics of your page, such as
column width, line-  height, and so on.  As early as the sixteenth century,
typographers had begun to use a common scale for type size, and there is no
reason not to replicate this approach on the Web, particularly if you're looking
for a traditional and highly legible result. Like a musical scale, the series
below has a natural elegance to it, and is considered by typographers to be a
standard for relative type sizes.

The traditional type size series developed in the sixteenth century A great
example of a site that uses this type size scale on the Web is Jaredigital .com
It's clear that traditional and stylish typography was required for this site,
and a little discreet poking around in the CSS finds the rules that make it all
possible: h3 { font-size: 16px; } h4 { font-size: 12px; } p { font-size: 11px; }
A good rule of thumb is that the closer to one another are the font sizes of the
various levels, the more elegant the overall impression will be.  If you're
after a disjointed or modern feel, try using font sizes that are further apart
on the scale. A good example of this can be seen at Coudal .com. The large
headers used here are fairly unusual, and in my opinion, give a much more
modern, less traditional feel to the design. Note, however, that the headings
are displayed in a much lighter color than the main body text. Another good rule
of thumb states that, if you want to use larger sizes for headings, use a
lighter color to keep the overall feel of the page balanced. It's also worth
pointing out those headings as a particularly successful use of a serif font (in
this case, Times) on the screen.  Emphasis Emphasizing text is a relatively
simple way to bring your words to life on the Web. There is a great fashion at
the moment — particularly in online promotional or advertising text — to scatter
bold font weights liberally around the page. Although traditional typographic
convention would warn against the over-  use of related faces such as bold or
italics, on the Web, this technique can be used to encourage the eye through the
text when there are so many other distractions on screen. There are various ways
we can introduce emphasis into text: Bold On the Web, the most common and
effective method is the use of a bold face from the current font family. Of
course, as with all these techniques, ensure that you do not over-  use the bold
face. The key is to use bold faces sparingly, or not at all.  Italics Italic
text can suffer on low-  resolution monitors because of the slanted and more
curved shapes of italicized letters. These are likely to look too pixilated when
aliased, and too blurred when anti-  aliased, and will spoil legibility when
used in lengthy body text. However, this should not stop you from using an
italic face when convention approves it, for example, quoting foreign words and
phrases, or listing books and periodical titles.  Underlining It's commonly
understood that underlining text on the Web is not a good idea. Web conventions
tell us that any underlined text is a link, and can be clicked on. It's a pretty
fair bet that if you go around underlining text that's not clickable, you're
going to confuse users.  Color Using color for emphasis is also a rather tricky
business. Like underlined words, colored words could be mistaken for a link
within body text. In the past it was common to use a distinct background color
to give emphasis to a passage of text, but even this has now become a convention
for identifying links. Furthermore, there are the accessibility issues
associated with using color to display important meaning on the Web, which
further add to the conclusion that using color for emphasis is not an ideal
method.  Capital Letters Never set passages of text in full or small caps. Not
only is doing this considered rather rude and inelegant, but typographically, it
is a very poor choice. CONSIDER THE LEGIBILITY OF THIS SENTENCE COMPARED TO THE
ONES THAT PRECEDE AND FOLLOW IT. There is no doubt that, without the distinct
shape of the words created by the lower case letters, readability is severely
inhibited. The example below highlights this clearly. There is no doubt that it
is easier to make out the words in the upper half of the sentence due to the
more obvious shape of the words.

Note how the upper half of the sentence is easier to read than the lower
However, nice touches can be achieved using full or small caps in the right
situation. Try using small caps for abbreviations and acronyms within text, and
of course, full caps for acronyms that stand for personal names or place names.
This is a detail that is rarely considered on the Web, but that should be simple
to achieve with site-  wide CSS files.  Line Spacing A crucial detail when
setting type on the Web is the issue of line spacing, or leading. The vertical
distance between lines of body text can make a huge difference to the legibility
and overall style of your message. A comparison of the two blocks of text below
should demonstrate the point clearly. A general rule of thumb when selecting a
line-  height value is that the longer the line length (measure) of your text,
the more leading is required for legibility.

Notice the clarity that the increased line height brings to the text The default
line-  height for most browsers is around 1.2 (where 1 is where the tops of one
row of characters would touch the base of the row above). This is not sufficient
for text on screen; I'd recommend increasing your line-  height value to between
1.4 and 1.6.  Letter Spacing & Kerning In my opinion, letter spacing (or
tracking) as controlled by the letter-  spacing property in CSS should never be
used for body text. It's not very cross-  browser friendly, and what you may see
in Firefox may be drastically different in Internet Explorer or Safari.
Furthermore, it seems wrong to mess about with the tracking that each font has
built into it. The type designer carefully calculates these spaces, and most of
the time, they're going to provide good results without any extra tampering.
However, letter spacing can be a very effective tool in short headings. A
classic example of this can be seen at Zeldman .com. The large letter spacing of
the titles in the right hand column allows for a bolder, uppercase typeface, but
ensures that the overall balance of the page is consistent.

Letter spacing of titles at Zeldman .com Kerning refers to improving the
appearance of a word by adjusting spacing between certain pairs of letters. On
the Web, it's not realistic to attempt this with body text. However, if you're
creating headers or logos in graphics software, a little kerning can make a
dramatic difference to the overall result.  Word Spacing As with letter spacing,
word spacing should be considered carefully before it's applied to the screen. I
am of the belief that if you think your text needs spacing and tracking
manually, then you have probably chosen the wrong typeface for the job. As with
all the text properties that CSS allows us to control, consistency is the key.
Keep readers focused on the text, and make it easy for their eyes to flow
through the words. If you're breaking up the letters and words, you'll only
serve to distract them, and disrupt the flow of the text.  Alignment Margins can
have a surprisingly large influence on the look and feel of your pages. They
help keep your text apart from other elements of your site, and from the edges
of the browser window. Larger margins produce an open, free feeling, while
smaller, tighter margins give a constricted and more intimate aesthetic.
Assuming your content is in a language that flows left-  to-  right (ltr), you
should nearly always left-  align your text on the Web. Right-  aligning should
only be used for very specific purposes, and certainly not for long passages of
text. The ragged left edge of right-  aligned text makes it very difficult for
the eye to move from one line to the next and reduces legibility considerably.
Text that is set flush with both left and right margins is known as justified
text. Justification is extremely popular in books and newspapers, and gives a
uniform and controlled feel to a page. Justification on the Web would be far
more popular if it weren't for the fact that no browser or operating system has
a hyphenation dictionary built in. This means that the only way the browser has
to justify text is to space out and/ or squash up words to fit into the specific
line length. This is fine to an extent, but there comes a point — particularly
with shorter line lengths — when a bit more flexibility is required.  The
designer Todd Dominey provides an option on his site to justify all text, and
the two screen shots below shows the good and bad results of doing that.

Good justified text at Whatdoiknow .org

Bad justified text at Whatdoiknow .org The text in the wider column looks great,
and the browser has dealt well with the justification. However, in the smaller
left-  hand column you can see that the word spacing is very mixed and
inconsistent, and this drastically affects the legibility of the text.  What
else do you feel helps to characterize a good screen font? I'd love to hear your
tips in the comments.  What else do you feel helps to characterize a good screen
font? Id love to hear your tips in the comments.


Spacing Acronyms Acronyms being chiefly a string of uppercase characters are
harder to discern than their lowercase equivalents due to the uniformity in
height and the baseline the uppercase letters sit on (see my previous post, Top
10 Dos and Don'ts of Web Typography: #6).  A common technique to avoid causing
acronyms in a block of text to spring out and act as eyesore is to set them in
small-  capitals (font-variant: small-caps;). By doing so the height of the now
"small" capitals aligns to the x-  heightas sits nested better in the text.
However as now smaller, the small-  capitals can become somewhat illegible
sitting as close as they are to one another by default.  Here we can call upon
tracking by adding a few additional units of spacing between the glyphs. For
example:

```
acronym, .caps, abbr {
   text-transform: lowercase;
   font-variant: small-caps;
   font-style: normal;
   letter-spacing: 1px;
}
```