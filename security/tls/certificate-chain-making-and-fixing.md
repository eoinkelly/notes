# Certificate chains

## I have my_common_name.crt, how do I create a chain?

Good tutorial: https://medium.com/@superseb/get-your-certificate-chain-right-4b117a9c0fce

https://certificatechain.io/ seems like a useful automatic way to do it

Create a bundle of certs for your server which has the full chain so browsers
can work their way back to their trusted root certs.

NB: Always test certificates using `curl` or similiar (browsers will fill in gaps in chain with AIA and hide problems from you)

* A certificate chain is just a concatenation of multiple CRT files
* a chain is joined on Issuer-in-child-cert --> Subject-in-signing-cert

Steps

Follow https://medium.com/@superseb/get-your-certificate-chain-right-4b117a9c0fce for details

1. Fet the certificate of the CA that signed your cert
2. Get the certs for any intermediate CAs
3. Concatenate them into a single file for use on your web server

```bash
cat your_domain_name.crt DigiCertCA.crt >> bundle.crt
```

## Why browsers can tolerate a broken chain but other software cannot

https://security.stackexchange.com/questions/64415/how-are-browsers-able-to-find-and-construct-an-alternate-trusted-chain-path

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

## Task: verify a certificate chain

Given a `.cer` file containing a chain of certificates for a site, show that this machine can can verify them using it's built-in certificate store


```
# First split the file into multiple .cer files each with a single cert

# these commands must be run on the server you are verifying i.e. using the
# same openssl installation as the server software will use.

# get info about each .cer
openssl x509 -in file.cer -noout -subject
openssl x509 -in file.cer -noout -issuer


# verify from a given root cert
openssl verify -CAfile root_ca.cer -untrusted middle.cer my_server.cer

# verify using the store of root certs built into openssl
openssl verify -untrusted middle.cer my_server.cer
```
