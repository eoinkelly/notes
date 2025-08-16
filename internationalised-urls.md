# Internationalised URLs

## Sources

- https://www.w3.org/International/articles/idn-and-iri/ (most useful)
- http://xahlee.info/js/url_encoding_unicode.html (quite old but useful)

## Basics

How i18n is achieved is different for the domain and path parts of a URL

1. Domain: Uses IDN (punycode)
    - Steps
        1. Browser converts whatever original encoding is in typed text to
           Unicode
        1. Browser normalises the Unicode
        1. Browser punycode encodes each label in the domain
        1. Browser makes DNS query for the punycoded encoded domain
        1. Browser gets IP address
        1. Request continues as normal
1. Path: Uses IRI (Internationalised resource identifiers)
    - Steps
        1. Browser converts whatever original encoding is to Unicode
        1. Browser normalises the Unicode
        1. Browser encodes the Unicode codepoints as UTF-8
        1. Browser percent-encodes the UTF-8 encoded bytes
        1. Browser sends the request
        1. Server percent decodes the path, interprets the UTF-8, converts it
           into the Unicode codepoints
        1. Server converts Unicode to whatever the underlying filesystem uses
        1. Server finds the resource and returns it to the browser

## Punycode encode/decode

Easiest: https://www.punycoder.com/

Ruby:

```ruby
# First:
#
# $ gem install simpleidn
#
require "simpleidn"

punycode_domain = "xn--mllerriis-l8a.com"

unicode_domain = SimpleIDN.to_unicode(punycode_domain)
=> "møllerriis.com"

SimpleIDN.to_ascii(unicode_domain)
=> "xn--mllerriis-l8a.com"
```

```bash
# Maori version
http://māori-example.rabidapp.nz
# * when pasted into browser address bar, browser will actually send the
#   punycode version to server

# Punycode version
http://xn--mori-example-7mb.rabidapp.nz
# * when pasted into browser address bar, browser will convert it to Unicode
#   for display

# "Englishized" version (you should register this to ensure that those who
# cannot easily type non-ASCII
http://maori-example.rabidapp.nz
```

## Example using IRI encoding to encode an i18n path

Consider this unusual URL

    https://arstechnica.com/science/2019/05/the-oceans-absorbed-extra-co₂-in-the-2000s/

- Note that the non-ASCII char is in the _path_ not the _domain_ (this is
  relevant)
- It has a "subscript 2" unicode character which is NOT percent encoded
    - Unicode "subscript two"
        - https://www.fileformat.info/info/unicode/char/2082/index.htm
        - is 0x2082,
        - encoded as UTF-8 as 0xE28282

### What do browsers show in the URL bar for an IRI encoded path?

- Browsers will actually submit the IRI encoded form but may show the Unicode
  form in the URL
    - Modern browsers seem to show the Unicode in the URL bar
    - Firefox and Chrome
        - when you paste the URL above into the URL bar, firefox will show the
          original URL but if you copy it from the URL bar it will copy
          `https://arstechnica.com/science/2019/05/the-oceans-absorbed-extra-co%E2%82%82-in-the-2000s/`
    - Safari
        - Copies the Unicode version not the IRI encoded version e.g the URL
          copied will be
          `https://arstechnica.com/science/2019/05/the-oceans-absorbed-extra-co₂-in-the-2000s/`
          in Safari.
- Browsers do automatic converstion of URIs with Unicode chars
- it varies somewhat between browser
- will automatically convert a URL with Unicode chars in it to a percent encoded
  form
