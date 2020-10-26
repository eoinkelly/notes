# SSL/TLS Certs

    cert = public-key + identity + signature-of-a-trusted-party-binding-the-key-and-identity

* X.509
    * is the format of the certificate in TLS/SSL
    * https://tools.ietf.org/html/rfc5280

commonName field has been considered deprecated, because it’s ambiguous and untyped.
commonName should be duplicated in the list of SANs

https://stackoverflow.com/questions/589834/what-rsa-key-length-should-i-use-for-my-ssl-certificates

Recommended bit length for 2020: 2048 bits

## Jargon

Standard DV ??

## How to get a cert

1. Generate a CSR
    * CSR includes the `commonName` but you can add subject alternative names later in the procss usually
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


# Misc

how to gen CSR for nginx: https://www.digicert.com/kb/csr-ssl-installation/nginx-openssl.htm

wizard which generates openssl command to create a CSR request: https://www.digicert.com/easy-csr/openssl.htm

you can usually add SANs during the ordering process rather than trying to make openssl put it in the CSR

