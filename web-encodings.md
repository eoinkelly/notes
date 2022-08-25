# application/x-www-form-url-encoded vs Percent-Encoding (RFC-3968: URI encoding)

* Percent-encoding
    * defined in https://www.rfc-editor.org/rfc/rfc3986#page-12
    * called "Percent-Encoding" in the RFC
* `application/x-www-form-url-encoded`
    * used to send data as part of HTTP requests e.g. in the body of a POST request
        * the encoding in use will be in the `Content-Type` request header
    * content type HTTP header: `application/x-www-form-urlencoded`
    * based on an early version of the general URI percent-encoding rules, with a number of modifications such as newline normalization and replacing spaces with + instead of %20.
    * currently defined in the HTML and XForms specifications.
        * In addition, the CGI specification contains rules for how web servers decode data of this type and make it available to applications.
    * is an alternative to `multipart/form-data` (content type: `multipart/form-data; boundary={boundary string}`)

Differences between them

> The encoding used by default is based on an early version of the general URI percent-encoding rules, with a number of modifications such as newline normalization and replacing spaces with + instead of %20.


## How to use them in various stacks

### Ruby

https://bugs.ruby-lang.org/issues/18822

```ruby

# percent encode
# ##############
ERB::Util.url_encode(" ") # => "%20"

# percent decode
# ##############
CGI.unescape("%20hello+there") # => " hello there"
# or
# seems to also work?
URI.decode_www_form_component("hi%20there") # => "hi there"


# x-www-form-url-encoded encode
# #############################
CGI.escape(" ") # => "+"
# or
URI.encode_www_form_component(" ") # => "+"

# x-www-form-url-encoded decode
# #############################
URI.decode_www_form_component("+hi%20") # => " hi "
# or
CGI.unescape("%20hello+there") # => " hello there"
```