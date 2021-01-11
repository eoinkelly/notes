# SSL/TLS Certs

    cert = public-key + identity + signature-of-a-trusted-party-binding-the-key-and-identity

* X.509
    * is the format of the certificate in TLS/SSL
    * https://tools.ietf.org/html/rfc5280

## History

* commonName field has been considered deprecated, because it’s ambiguous and untyped.
* The commonName field should be duplicated in the list of SANs

## The Certificate Signing Request (CSR)

* contains
    * some data about the requester
    * a public key
* Cert providers e.g. Digicert allow you to override the requester data in the web form
* Limitations of CSR
    * apparently doesn't handle SANs very well

Gotcha: Adding SANs to a CSR is hard according to Digicert so they want you to do it in their web form
    how true is this?

## Tools

* https://badssl.com/
    * A list of test subdomains with specific SSL problems

## Jargon

* SAN = Subject Alternate Name
    * An extension to the X.509 format that allows the certificate to protect multiple domain names
        * the standard doesn't seem to enforce a maximum number of SAN domains which may be added but it seems Windows at least sets 4kb as the limit for the SubjectALtName field.
            * badssl.com has an example with 10k altnames which fails in Chrome
        * SAN domain names can be wildcards (usual rules around wildcards apply i.e. only one level of *. )
    * A certificate can be described as a _SAN certificate_ if it has the extension
    * SAN extension is deprecating the CN (commonName) field of the X.509 format
    * every certificate issued is a SAN certificate now because issuers are required to duplicate the value in CN in a SAN field as well

## Types of certs

Certs can be categorized by the number of domains they "protect":

1. Standard cert (a single common name)
    * Digicert seems to include a `www.` SAN in the base price
2. Multi-Domain (SAN) certs
    * Single CN field but multiple SN fields
        multi fields or multi urls in same field???
    * charged per SAN
3. Wildcard certs
    * common name begins with `*`
    * the `*` matches any legal character except `.`
        * => wildcards cannot go multiple levels deep


Certs can be also categorised by the methods used by the certificate authority (CA) to validate the subject information included in the certificate:

1. Domain Validation (DV)
   * is the lowest level of validation, and verifies that whoever requests the certificate controls the domain that it protects.
2. Organization Validation (OV)
   * verifies the identity of the organization (e.g. a business, nonprofit, or government organization) of the certificate applicant.
   * how?
3. Individual Validation (IV)
   * verifies the identity of the individual person requesting the certificate.
   * how?
4. Extended Validation (EV)
    * like OV, verifies the identity of an organization.
    * However, EV represents a higher standard of trust than OV and requires
      more rigorous validation checks to meet the standard of the CA
    * Cert has been validated by humans, not just automated domain validation
    * Cert vendors still try to upsell you to this
    * In the past browsers should show the name of the entity beside the lock in the address bar
        * their research showed it didn't improve security outcomes so they stopped
    * Don't get an EV cert
        * https://www.bleepingcomputer.com/news/software/chrome-and-firefox-changes-spark-the-end-of-ev-certificates/
        * https://www.troyhunt.com/extended-validation-certificates-are-really-really-dead/

## Cert length

* Recommended bit length for 2020: 2048 bits
* 2048 bit key seems to be the standard these days.
* 4096 bit key available but not popular as option
    * I _think_ this is because it's seen as overkill and a bit computationally expensive to encrypt/decrypt data (computational cost is not linear with key size)
    * See https://stackoverflow.com/questions/589834/what-rsa-key-length-should-i-use-for-my-ssl-certificates

## How to get a cert

1. Generate a CSR
    * CSR includes the `commonName` but you can add subject alternative names later in the process usually
        * generating CSR with alt names is fiddly in openssl apparently
1. Submit CSR to a certificate authority
    * add SANs through their web form usually
1. Get your certificate from the vendor, usually in `your_common_name.crt` file
1. For SSL/TSL usage:
    1. create a bundle of certs for your server which has the full chain so browsers can work their way back to their trusted root certs
        `cat your_domain_name.crt DigiCertCA.crt >> bundle.crt`
    1. Install the certificate and the private key you generated in step 1 onto the server
        ```
        # nginx config snippet
        server {
            listen   443;
            ssl    on;
            ssl_certificate    /etc/ssl/your_common_name.pem; (or bundle.crt)
            ssl_certificate_key    /etc/ssl/your_common_name.key;
        ```


## Example

this will create both a CSR and a private key (RSA:2048 bit)

```bash
$ openssl req -new -newkey rsa:2048 -nodes -out eoin_example_com.csr -keyout eoin_example_com.key -subj "/C=NZ/ST=Wellington/L=Wellington/O=My Org/OU=Dev/CN=eoin.example.com"

$ ls -al
total 8
drwxr-xr-x 4 eoinkelly staff  128 Aug 10 11:43 .
drwxr-xr-x 6 eoinkelly staff  192 Aug 10 11:41 ..
-rw-r--r-- 1 eoinkelly staff 1017 Aug 10 11:43 eoin_example_com.csr
-rw-r--r-- 1 eoinkelly staff 1704 Aug 10 11:43 eoin_example_com.key
```

# Alternatives to openssl on the command line

* wizard which generates openssl command to create a CSR request: https://www.digicert.com/easy-csr/openssl.htm
    * doesn't add SANs (I presume it assumes you will add them in the web from you submit later)
