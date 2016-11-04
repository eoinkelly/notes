# Percent encoding or "URL encoding"

Sources

* https://en.wikipedia.org/wiki/Percent-encoding

Overview

* The common name "URL encoding" is technically incorrect as this encoding is used for encoding data in a URI  which includes both URL and URN

URI = Uniform resource identifier
URL = Uniform resource locator
URN = Uniform resource name

    URI = URL + URN


Usages

1. encode data in URI
2. encode data for the `application/x-www-form-urlencoded` media type i.e. HTTP POST form uploads


Characters are divided into two groups

1. reserved
    * have special meaning e.g. `/` has special meaning to be the delimiter between path segments
    * must be encoded with "percent encoding"
2. unreserved
    * have no special meaning
    * are transmitted as is

How

1. convert the character to its UTF-8 bytes
    * because UTF-8 is a superset of ASCII you can consider that we use the ASCII byte value for those characters which are within the ASCII range
2. represent those bytes using hex digits prefixed by `%` (which is the escape character)


> URIs that differ only by whether an unreserved character is percent-encoded
> or appears literally are equivalent by definition, but URI processors, in
> practice, may not always recognize this equivalence. For example, URI
> consumers shouldn't treat %41 differently from A or %7E differently from ~,
> but some do. For maximum interoperability, URI producers are discouraged from
> percent-encoding unreserved characters.

> Because the percent character ( % ) serves as the indicator for
> percent-encoded octets, it must be percent-encoded as %25 for that octet to
> be used as data within a URI.
