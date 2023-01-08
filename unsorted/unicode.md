# Unicode
every "platonic letter" in every alphabet is assigned a number by Unicode - called that letter's "code point" and written like U+0693.
U+ = "this is unicode"
0693 = hexedecimal number
you can see the mappings in charmap.

Hello = U+0048 U+0065 U+006C U+006C U+006F

so letters map to code-points. But how are they stored in memory? Answer = encodings

UCS-2 or UTF-16
	simple 2-byte per character encoding
	needs a flag at the start of the character stream to say whether it is big/little endian (systems are fastest at working with the endianness that matches their architecture)

UTF-8 is an encoding for Unicode (not the only one)
	every code point from 0-127 is stored in a single byte
	code points 128 and above are stored using 2,3 up to 6 bytes
	has the side-effect that english in UTF-8 looks the same as english in ASCII
	does not use the same number of bytes for each character
	it can store *any* character correctly (unlike ascii, latin-1 etc.)

there are others
	UTF-7
		like UTF-8 but guarantees that the high-bit will always be zero (handy for sending through email systems)
	UCS-4
		stores each char in 4 bytes (requires more memory for this)
	you can even encode unicode code-points as ASCII, ISO-8859-1 (aka Latin-1)  or older encodings - they will just show boxes or question marks for characters they can't show.

**It does not make any sense to have a string (in memory, a file, an email) without knowing what encoding it uses.**

the content-type http header (and/or meta tag)  tells you this for a web page. luckily most encodings do the same stuff between 32 and 127 so we can get away with it.
this is why the meta tag has to be first in the HTML file.