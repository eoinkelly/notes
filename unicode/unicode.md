# Unicode

* maps each glyph to a number (called a code point)
* 0x10FFFF (1,114,111) code points
* 0x10 (17) planes, each plane with 0xFFFF (65,535) codepoints
* the first plane (plane 0, aka 0x000000 -> 0x00FFFF) is called the _Basic Multilingual Plane_ (BMP)
    * contains the most commonly used symbols
    * all chars on BMP can be encoded in two bytes
* all other planes (planes 1 to 16) are called supplemenatry planes or **astral planes**
* code points on the astral planes need more than 4 hex digits to represent them
* UTF-16 can be used for astral plane glyphs becasue is **not** a fixed width encoding! (it will use 4 bytes for astral plane characters)
* The **only** fixed width encoding is UTF-32

## The test case

@mbynens suggests using `Iñtërnâtiônàlizætiøn☃💩` as a test string in unit tests to check for unicode problems in strings because

* The first 20 chars are from 0x0000 -> 0x00FF range
* The next char is in the 0x0100 -> 0xFF00 range
* The final char is from the astral plane (

Working with strings

Humans generally think in terms of Unicode symbols or graphemes but programming
langs don't always follow that intuition.

String edge cases

1. characters from the astral planes
1. characters made from multiple combining marks e.g. family emojii
1. extended grapheme clusters

For any language

1. How does it calculate the length of a string? (bytes?, glyphs?, something else?)
    * how does it calculate the length of a string including characters in the astral planes
2. How does it handle astral plane chars when iterating?
3. How does it do normalization?
4. How does reversing a string work with the edge cases?
5. How does it work with edge cases in regular expressions?

> Internally, JavaScript represents astral symbols as surrogate pairs, and it
> exposes the separate surrogate halves as separate "characters".

## Precomposed and decomposed forms

Excellent article: https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/Strings/Articles/stringsClusters.html

* Many writing systems allow adding an accent or other decoration to a glyph to alter its meaning
* Other writing systems allow combining many decorations together to form a single character e.g. Hangul (Korea)
    * don't think of "accents" as always being a "base character which has meaning on its own" with an "optional decoration" - that may be true in french but not in Hangul - the Hangul example demonstrates the more general problem.
* There are too many of these combinations for unicode to specify each combination of parts separately so Unicode specifies a separate code-points for each "decoration" or "part of character"
* If the final glyph is formed using the code-points for each individual part it is called a "decomposed form"
* For compatibility reasons, Unicode also has **some** code-points for characters + decoration e.g. _e with acute_ - these are called "precomposed forms"
    * Only some character + decorations exist as pre-composed form so while a particular string may be fully precomposed, you cannot create a function which would precompose all strings
* Unicode has normalizations to convert between decomposed and precomposed forms
* You have to be careful when splitting, reversing or iterating over a string to make sure you don't split up a depcomposed character

Edge-cases where the mapping from code-point to character is not 1:1

1. Surrogate pairs
1. base character plus combining mark
1. Hangul Jamo
1. Indic consonant clusters

Collectively these edge cases are called "grapheme clusters"

> In many writing systems, a single character may be composed of a base letter plus an accent or other decoration. The number of possible letters and accents precludes Unicode from representing each combination as a single code point, so in general such combinations are represented by a base character followed by one or more combining marks. For compatibility reasons, Unicode does have single code points for a number of the most common combinations; these are referred to as precomposed forms, and Unicode normalization transformations can be used to convert between precomposed and decomposed representations. However, even if a string is fully precomposed, there are still many combinations that must be represented using a base character and combining marks. For most text processing, substring ranges should be arranged so that their boundaries do not separate a base character from its associated combining marks.
>
> In addition, there are writing systems in which characters represent a combination of parts that are more complicated than accent marks. In Korean, for example, a single Hangul syllable can be composed of two or three subparts known as jamo. In the Indic and Indic-influenced writing systems common throughout South and Southeast Asia, single written characters often represent combinations of consonants, vowels, and marks such as viramas, and the Unicode representations of these writing systems often use code points for these individual parts, so that a single character may be composed of multiple code points. For most text processing, substring ranges should also be arranged so that their boundaries do not separate the jamo in a single Hangul syllable, or the components of an Indic consonant cluster.
>
> In general, these combinations—surrogate pairs, base characters plus combining marks, Hangul jamo, and Indic consonant clusters—are referred to as grapheme clusters.

More edge cases:

1. converting between upper and lowercase can change the number of code-points required
1. some collation algorithms consider multiple character sequences as a single unit when sorting
1. users may expect to be able to put their cursor in the middle of a glyph e.g. a single code-point "fi" ligature

> Characters and glyphs do not have a one-to-one correspondence. In some cases a character may be represented by multiple glyphs, such as an “é” which may be an “e” glyph combined with an acute accent glyph “´”. In other cases, a single glyph may represent multiple characters, as in the case of a ligature, or joined letter.

> In some cases, Unicode algorithms deal with multiple characters in ways that
> go beyond even grapheme cluster boundaries. Unicode casing algorithms may
> convert a single character into multiple characters when going from lowercase
> to uppercase; for example, the standard uppercase equivalent of the German
> character “ß” is the two-letter sequence “SS”. Localized collation algorithms
> in many languages consider multiple-character sequences as single units; for
> example, the sequence “ch” is treated as a single letter for sorting purposes
> in some European languages.

> insertion point boundaries are not identical to glyph boundaries; a ligature
> glyph in some cases, such as an “fi” ligature in Latin script, may require an
> internal insertion point on a user-perceived character boundary
## UTF-16 Surrogate pairs

From apple docs

> The vast majority of Unicode code points used for writing living languages
> are represented by single UTF-16 units. However, some less common Unicode
> code points are represented in UTF-16 by surrogate pairs. A surrogate pair is
> a sequence of two UTF-16 units, taken from specific reserved ranges, that
> together represent a single Unicode code point.

* even if all your code-poitns are in the BMP you need to be aware that some of them (if they are surrogate pairs) should never be split up - this is important for reversing and iterating over a string

* they exist as a hack to allow you to encode astral plain characters by combining some special characters from the BMP plain.
* surrogates are code-point from a range of 2048 characters
* these code-points are not valid on their own. they must appear with a high-surrogate and low-surrogate paired together
* the algorithm for converting to/from surrogate pairs is documented as C in
  http://unicodebook.readthedocs.io/unicode_encodings.html#surrogates
* is a UTF-16 thing

```
0xDC80 -> 0xDBFF (1024 code-points) - high surrogates
0xDC00 -> 0xDFFF (1024 code-points) - low surrogates
```

## Unicode normalization

TODO - need to dig deeper into this

* applies one of the _unicode normalization forms_
* normalization forms
    1. NFC
        * Normalization form canonical composition
    1. NFD
        * Normalization form canonical decomposition
    1. NFKC
        * Normalization form compatibility composition
    1. NFKD
        * Normalization form compatibility decomposition

## Grapheme clusters

 > grapheme clusters such as நி (ந + ி), Hangul made of conjoining Jamo such as 깍 (ᄁ + ᅡ + ᆨ), emoji sequences such as 👨‍👩‍👧‍👦 (👨 + U+200D ZERO WIDTH JOINER + 👩 + U+200D ZERO WIDTH JOINER + 👧 + U+200D ZERO WIDTH JOINER + 👦), or other similar symbols.

Aside: Vim doesn't seem to render the family emojii - it renders each one separately
## Implementations


### JS and Unicode

The TL;DR is that ES6 seems have improved Unicode support greatly

```js
// hex escapes \x##
console.log("\x41")
console.log("\x01\xF4\xA9") // interpreted as three separate characters

// Unicode escape \u####
console.log("\u2661")

// "Unicode code point escape \u{} (ES6 only)
console.log("\u{1F4A9}") // pile of poo
console.log("\ud83d\udca9") // pile of poo via surrogate pair
```

* adds the `\u{####}` unicode escape
* String@@iterator
    * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/@@iterator
* ??? other stuff TODO ???

Normalization

* `String.prototype.normalize`
* > If there is a single code point that represents the same glyph as
    > another code point followed by a combining mark, it will normalize it to
    > the single code point form.

length

* all strings in JS are UTF-16
* `length()` returns the number of "code units" in the string
* In UTF-16, a "code unit" is 16 bits
* UTF-16 uses one "code unit" (16 bits) for characters in the BMP
* UTF-16 uses two "code unit" (32 bits) for characters on the astral planes
* Implications for `length()`
    * **never** returns the number of **bytes** in the string
    * does return the number of glyphs in a string if all the characters are in the BMP
    * does NOT return the number of glyphs if any characters are in the astral planes (it will count astral plane characters as two _code units_.

iteration

    * ???

* To use characters from the astral planes in ES5 you have to use _surrogate pairs_
* surrogate pairs
    * a pair of code-points which combine to show one glyph
    * the code-point values don't resemble the equivalent single-code point value
    * are only required for astral symobls (BMP symbols can be represented by a single `\u####` escape sequence
* If you have as tring containing an astral symbol (e.g. pile of poo): `"\ud83d\udca9"` then JS will calculate that string's length as 2
    * this happens even if you use the ES6 unicode code point escape
* String.length in JS counts ???
    * the number of *bytes* not glyphs in the string
    * it can handle anything in the BMP
    * if the string is just in the BMP then it counts glyphs

```js
// pile of poo as surrogate pair
console.log("\ud83d\udca9".length) // => 2
// pile of poo as ES6 unicode
console.log("\u{1F4A9}".length) // => 2
```

Normalization

```js
// U+1E9B: LATIN SMALL LETTER LONG S WITH DOT ABOVE
// U+0323: COMBINING DOT BELOW
var str = "\u1E9B\u0323";

// String.prototype.normalize() returns a copy of the string

str.normalize(); // defaults to NFC
str.normalize('NFC'); // same as line above
str.normalize('NFD');
```


### Ruby and Unicode

* seems to do the thing humans expect by having the length of a string be in "glyphs" not bytes
* does not like surrogate pair ???

```ruby
irb(main):003:0> "\u2661"
=> "♡"
irb(main):004:0> "\u2661".length
=> 1
irb(main):005:0> "\u{41}".length
=> 1
irb(main):006:0> "\u{41}"
=> "A"
irb(main):007:0> "\u{1F4A9}"
=> "💩"
irb(main):008:0> "\u{1F4A9}".length
=> 1
```

Normalization

???

