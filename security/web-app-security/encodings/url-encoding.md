# Percent encoding or "URL encoding" (RFC 3986 encoding)

Sources

- https://en.wikipedia.org/wiki/Percent-encoding
- RFC 3986 (dating from 2005) is the most recent standard for this

Overview

- The common name "URL encoding" is technically incorrect as this encoding is
  used for encoding data in a URI which includes both URL and URN

    URI = Uniform resource identifier URL = Uniform resource locator URN =
    Uniform resource name

    URI = URL + URN

> The sets of reserved and unreserved characters and the circumstances under
> which certain reserved characters have special meaning have changed slightly
> with each revision of specifications that govern URIs and URI schemes.

Usages

1. encode data in URI of HTTP requests
2. encode data for the `application/x-www-form-urlencoded` media type i.e. HTTP
   POST form uploads. (Some modifications are made to this encoding for that
   media type)

Characters are divided into three groups

1. reserved characters
    - have a "reserved purpose" (or special meaning) e.g. `/` has special
      meaning to be the delimiter between path segments
    - are transmitted as is
    - RFC-3986: `!*'();:@&=+$,/?#[]` (18 chars)
        - note `%` is not in this list - it is the escape character, not a
          reserved character
2. unreserved characters
    - have no special meaning
    - are transmitted as is
    - RFC-3986: `a-zA-Z0-9-_.~`
3. All other characters
    - must be encoded with "percent encoding"

How percent encoding works

1. convert the character to its UTF-8 bytes
    - because UTF-8 is a superset of ASCII you can consider that we use the
      ASCII byte value for those characters which are within the ASCII range
2. represent those bytes using hex digits prefixed by `%` (which is the escape
   character)

> URIs that differ only by whether an unreserved character is percent-encoded or
> appears literally are equivalent by definition, but URI processors, in
> practice, may not always recognize this equivalence. For example, URI
> consumers shouldn't treat %41 differently from A or %7E differently from ~,
> but some do. For maximum interoperability, URI producers are discouraged from
> percent-encoding unreserved characters.

> Because the percent character ( % ) serves as the indicator for
> percent-encoded octets, it must be percent-encoded as %25 for that octet to be
> used as data within a URI.

QUESTION: will it work to have all characters percent encoded, even those that
don't need it?
