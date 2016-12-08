# Punycode

* Internationalized domain names are stored in the Domain Name System as ASCII
  strings using Punycode transcription.
* DNS supports a _subset_ of ASCII only

From https://en.wikipedia.org/wiki/Internationalized_domain_name

 > "xn--".[17] This four-character string is called the ASCII Compatible
 > Encoding (ACE) prefix, and is used to distinguish Punycode encoded labels
 > from ordinary ASCII labels.

 Note that the `xn--` is NOT part of punycode but is part of internationalized domain names (IDNA)

 > IDNA encoding may be illustrated using the example domain Bücher.ch.
 > “Bücher” is German for “books”, and .ch is the ccTLD of Switzerland. This
 > domain name has two labels, Bücher and ch. The second label is pure ASCII,
 > and is left unchanged. The first label is processed by Nameprep to give
 > bücher, and then converted to Punycode to result in bcher-kva. It is then
 > prefixed with xn-- to produce xn--bcher-kva. The resulting label suitable
 > for use in the DNS is therefore xn--bcher-kva.ch.

> The use of Unicode in domain names makes it potentially easier to spoof web
> sites as the visual representation of an IDN string in a web browser may make
> a spoof site appear indistinguishable to the legitimate site being spoofed,
> depending on the font used. For example, Unicode character U+0430, Cyrillic
> small letter a, can look identical to Unicode character U+0061, Latin small
> letter a, used in English.


    "München" (German name for the city of Munich) would be encoded as "Mnchen-3ya"

Overview of how it works

1. Copy all plain ascii chars from input to output.
1. append `-` if any plain ascii chars were copied
1. for each non-ascii char, calculate a number representing the codepoint and its position in the string
1. encode that number as a series of ascii chars and append them to output

Better details at https://en.wikipedia.org/wiki/Punycode

