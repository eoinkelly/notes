# x-www-form-urlencoded encoding

* uses the `application/x-www-form-urlencoded` media type
* is defined in the HTML, XForms, CGI specs (specs are defined in an "outdated manner"0
* is _mostly_ the same as URL encoding but has some modifications
    * replace space with `+` instead of `%20`
    * "newline normalization"
* uses
    * body of HTTP POST message
    * technically HTML forms can be submitted via email too and it can be used in this
