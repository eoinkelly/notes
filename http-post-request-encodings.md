# Encoding data for HTTP POST requests

When you send data with a POST request there are four common options for
packaging it up:

1. multipart/form-data
    * emulates waht the browser would send when submitting a form
    * follows RFC 2388
    * you can send binary files from disk as the value via curl
    * example with curl
        ```
        curl -X POST
            -H "Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW"
            -F "grant_type=client_credentials"
            -F "client_id=eba574524069f02b9102a13ff609a2d41ec49647456f11da59b6e7f06186d4ed"
            -F "client_secret=7dfa6914499eacb75defe64ea4619801da125a02abfa11ac8ef7f3da08c07a59"
            "http://localhost:3001/oauth/token/"
        ```
2. application/x-www-form-urlencoded
    * used by default by cURL
    * example using curl
        ```
        curl -X POST
            -H "Cache-Control: no-cache"
            -H "Postman-Token: b6631134-24f2-95e3-a969-903898c42481"
            -H "Content-Type: application/x-www-form-urlencoded"
            -d 'grant_type=client_credentials&client_id=eba574524069f02b9102a13ff609a2d41ec49647456f11da59b6e7f06186d4ed&client_secret=7dfa6914499eacb75defe64ea4619801da125a02abfa11ac8ef7f3da08c07a59'
            "http://localhost:3001/oauth/token/"
        ```
3. ascii or raw
    * curl `-d` aka `--data-ascii`
    * curl `--data-raw` is same except it will not interpret the `@` character
    * just sends the given string as data in the body of the HTTP request
    * cURL `-d "GIVEN_STRING" option
4. binary
    * curl `--data-binary @somefilename`


From the curl man page:

```
-d, --data <data>

(HTTP) Sends the specified data in a POST request to the HTTP server, in the same way that a browser does when a user has filled in an  HTML  form
and presses the submit button. This will cause curl to pass the data to the server using the content-type application/x-www-form-urlencoded.  Com-
pare to -F, --form.

-d, --data is the same as --data-ascii. --data-raw is almost the same but does not have a special interpretation of the @ character. To post  data
purely binary, you should instead use the --data-binary option.  To URL-encode the value of a form field you may use --data-urlencode.

If  any  of  these  options  is  used more than once on the same command line, the data pieces specified will be merged together with a separating
&-symbol. Thus, using '-d name=daniel -d skill=lousy' would generate a post chunk that looks like 'name=daniel&skill=lousy'.

If you start the data with the letter @, the rest should be a file name to read the data from, or - if you want curl to read the data from  stdin.
Multiple  files can also be specified. Posting data from a file named 'foobar' would thus be done with --data @foobar. When --data is told to read
from a file like that, carriage returns and newlines will be stripped out. If you don't want the @ character to have a special interpretation  use
--data-raw instead.
```
