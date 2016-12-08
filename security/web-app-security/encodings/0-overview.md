# Overview of encodings

Encodings in use on the web

1. URL encoding (RFC 3986)
2. %uXXXX encoding (deprecated, never standardized)
1. Base64 encoding
4. x-www-form-urlencoded encoding (basically URL encoding with some tweaks)
5. HTML entity encoding
6. Plain hex encoding of binary data (not in common use but is possible)
7. Punycode (a candidate for how i18n domain names might work)
8. form/multipart

These can be layered to potentially fool input sanitization.

Where _data_ exists in HTTP requests and responses:

1. HTTP URI
1. HTTP Header values
1. HTTP Body

Details of encodings used in various areas:

https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers

    This is WIP and needs more research - I'm still confused about what exactly can be used where

* HTTP URI
    * uses URL encoding
    * uses punycode for domain name labels
    * TODO: it's not clear to me what the support and interaction between URL-encoding and punycode in URLs is
* HTTP Header values
    * I think this uses URL-encoding ???
* HTTP Body
    * HTTP request headers relevant to HTTP body
        * Content-Type
        * Content-Encoding
        * Content-Location
        * Content-Length
        * Accept
    * HTTP response headers relevant to HTTP body
        * Content-Type
        * Content-Encoding
        * Content-Location
        * Content-Length
    * encodings commonly used
        * `x-www-form-urlencoded`
        * `form/multipart`
    * HTTP Request: body encoding indicated by the `Content-Type` header
    * HTTP Response: body encoding indicated by `Content-Type` header

[THIS SECTION NEEDS MORE RESEARCH]
