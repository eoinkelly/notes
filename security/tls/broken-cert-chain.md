# Broken certificate chains

Sources

* https://security.stackexchange.com/questions/64415/how-are-browsers-able-to-find-and-construct-an-alternate-trusted-chain-path

Browsers can handle missing intermediate certs but other software cannot. Why? What do browsers do?

1. Cache intermediate certs
2. Certificates have an _Authority Information Access (AIA)_ extension section which contains the URL of the issuing authority. Browsers will follow this if required.

## Diagnosing a broken chain

* Option 1: Use a website
    * https://www.sslshopper.com/ssl-checker.html
    * ++ easy to interpret output
* Option 2: Use OpenSSL
    ```bash
    # you must run all this on the box which will be the client

    $ curl -v https://aaa.example.com
    # curl will behave similarly to other openssl libraries in your programming language


    $ openssl s_client -showcerts -servername aaa.example.com -connect aaa.example.com:44
    # output will show the certs
    ```
