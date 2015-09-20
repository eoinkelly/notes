# Character sets

A character set defines 3 things

1. a set of glyphs
2. an integer representation of each glyph
3. a way of encoding those integers as bytes

ASCII is a simple character set

* defines a set of glyphs
* defines the integer that each glyph is represented by
* defines how to represent that integer in bytes

###  Collation

* A character set needs some rules about how to compare characters in the set
  e.g. for sorting and string comparisons e.g. less than, equal, greater than
  etc.
* This set of rules is called a collation
* A character set always has at least one collations
* A character set may have more than one collation
* => A collation is a property of a character set and has no meaning outside
  that character set.

Examples of collations

* binary
    * compare characters based on the integers they are encoded as
    * very simplistic
* case-insensitve
    * treat upper and lower case letters as equal
* case sensitive

Common encoding names have a pattern:

    <character set name>_<language_name>_<suffix>

where `<suffix>` can be:

    cs => case sensitivk
    ci => case insensitive
    bin => binary

* A collation is tied to a character set encoding
    * the same collation name may exist in multiple character sets
* There are 3 collations available on all platforms
    1. `default`
    2. `C`
    3. `POSIX`
* C and POSIX have identical behaviours - they both specify "traditional C" behaviour
    * only uppercase A to Z considered "letters"
    * sorting done strictly by character code byte value
    * postgres considers them different collations so you can't compare a a
      column with a "C" locale and a column with a "POSIX" locale.

# Unicode includes a character set, collations and encodings

* Unicode includes the "Universal character set standard"
    * includes 120k+ characters in almost all the worlds languages
* Unicode v8.0 was released July 2015
* Unicode assigns each glyph a "code point" which is a positive integer
* Each code point needs to be represented as a binary number - _how_ that
  happens is specified by the "encoding"
* Unicode has a number of possible encodings e.g. UTF-8, UTF-16
* UTF-8 is an _encoding_ i.e. a way of turning a list of codepoints integers into bytes
* UTF-8 uses 1 byte for a codepoint if it can and 2-4 bytes if necessary
    * => it is a variable length encoding: 1 to 4 bytes
    * It is a superset of ASCII

* Unicode defines a _customisable_ collation algorithm
    * assigns a tuple of weighting floats to each glyph
    * it has a table with a default collation for _all_ unicode chars
        * the Default Unicode Collation Element Table (DUCET).
        * customizable for different languages

# Examples of character sets

* US-ASCII
* ISO-8859-1 aka latin1
* Unicode

Others http://www.iana.org/assignments/character-sets/character-sets.xhtml
