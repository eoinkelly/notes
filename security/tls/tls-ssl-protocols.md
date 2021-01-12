# TLS and SSL

## Aside fixing SSL verification

```
# changing the cert file from the default openssl one
# (/usr/local/etc/openssl/cert.pem) to the one that comes with curl seems to work

export SSL_CERT_FILE="/usr/local/Cellar/curl-ca-bundle/1.87/share/ca-bundle.crt"
```

## Sources

* TLS 1.2
    * https://tools.ietf.org/html/rfc5246
* https://en.wikipedia.org/wiki/Transport_Layer_Security

## Protocols

* There are three SSL protocols (1.0, 2.0, 3.0) and they have ALL been deprecated now
* TLS 1.0
    * 1999
    * refines previous version, upgrades keys and algorithms
    * supports downgrading to SSL 3.0 which weakens security
* TLS 1.1
    * 2006
    * refines previous version, upgrades keys and algorithms
* TLS 1.2
    * 2008
    * refines previous version, upgrades keys and algorithms
* TLS 1.3 (draft)
    * WIP

TLS Record protocol
TLS Handshake protocol

##  Host Verification and Peer Verification

 * TLS can secure any protocol over TCP
    * https is just a common one

* OpenSSL default behavior is to not verify certificates

OpenSSL has flags you can set to tell it how to verify certificates

* `SSL_VERIFY_NONE`
    * in server mode
        * no request for certificate is sent to client
        * client should not send a certificate
    * in client mode
        * any certificated sent by the server will be verified BUT failure of this verification will not terminate the connection
    * use cases
        * only makes sense when operating in server mode when clients can't provide a certificate to prove their identity
* `SSL_VERIFY_PEER`
    * in server mode
        * a request for a certificate will be sent to the client during handshake
        * the client is NOT required to send a cert
        * but if the client does send a cert the server will verify it and terminate connection if that verification fails
    * in client mode
        * if the server sends a certificate it will be verified and the connection terminated if that verification fails
        * the only time a server would not send a certificate is if an "anonymous cipher" is enabled
            * anonymous ciphers are disabled by default
* `SSL_VERIFY_FAIL_IF_NO_PEER_CERT`
    * only used in server mode
    * only used if `SSL_VERIFY_PEER` is also set
    * causes the server to terminate the connection if the client does not provide a certificate
* `SSL_VERIFY_CLIENT_ONCE`
    * only used in server mode
    * only used if `SSL_VERIFY_PEER` is also set
    * server still requests certificate from client during initial handshake
    * server does NOT request certificate from client during renegotiations

there are other flags
http://etutorials.org/Programming/secure+programming/Chapter+10.+Public+Key+Infrastructure/10.7+Verifying+an+SSL+Peer+s+Certificate/

## OpenSSL

```
# built-in OS X version
➜  fairfax-payment-service git:(adinterface-tests) ✗ /usr/bin/openssl version
OpenSSL 0.9.8zg 14 July 2015

# version from homebrew
➜  openssl git:(master) /usr/local/opt/openssl/bin/openssl version
OpenSSL 1.0.2d 9 Jul 2015
```

## curl

* docs: http://curl.haxx.se/docs/sslcerts.html
* use -k to put curl in insecure mode (will stop if failing if the server cert does not verify)
* curl verifies certs using its "installed CA cert bundle" by default
    * `/usr/local/Cellar/curl-ca-bundle/1.87/share/ca-bundle.crt`
* If you're using the curl command line tool, you can specify your own CA cert
  path by setting the environment variable CURL_CA_BUNDLE to the path of your
  choice

## CA bundles

In order for OpenSSl to verify the certificate for a server it needs to have
the certificate of the CA (certificate authority) available locally

CA certificates are usually distributed as a "bundle" e.g.
http://curl.haxx.se/ca/cacert.pem is a bundle of X.509 certificates of public
certificate authorities

* curl installs a CA bundle
    * `/usr/local/Cellar/curl-ca-bundle/1.87/share/ca-bundle.crt`
* OS X keeps certs in Keychain
* Firefox includes a certs file too
    * http://mxr.mozilla.org/mozilla-central/source/security/nss/lib/ckfw/builtins/certdata.txt
    * TODO: not sure where this lives on local filesystem
* openssl installs a cert bundle
    * `/usr/local/etc/openssl/cert.pem`
    * 5000 lines long on my system
    * just certs without comments

## Openssl: Show cert chain for a given host

```
➜  openssl s_client -host cominterface.preprod.stuff.co.nz -port 443 -showcerts
CONNECTED(00000003)
depth=2 /C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
verify error:num=20:unable to get local issuer certificate
verify return:0
---
Certificate chain
 0 s:/OU=GT00648307/OU=See www.rapidssl.com/resources/cps (c)15/OU=Domain Control Validated - RapidSSL(R)/CN=*.preprod.stuff.co.nz
   i:/C=US/O=GeoTrust Inc./CN=RapidSSL SHA256 CA - G3
-----BEGIN CERTIFICATE-----
MIIEsTCCA5mgAwIBAgIDAYCdMA0GCSqGSIb3DQEBCwUAMEcxCzAJBgNVBAYTAlVT
MRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMSAwHgYDVQQDExdSYXBpZFNTTCBTSEEy
NTYgQ0EgLSBHMzAeFw0xNTAxMDcwODQzMTJaFw0xNjAyMDkxMzI1MDRaMIGZMRMw
EQYDVQQLEwpHVDAwNjQ4MzA3MTEwLwYDVQQLEyhTZWUgd3d3LnJhcGlkc3NsLmNv
bS9yZXNvdXJjZXMvY3BzIChjKTE1MS8wLQYDVQQLEyZEb21haW4gQ29udHJvbCBW
YWxpZGF0ZWQgLSBSYXBpZFNTTChSKTEeMBwGA1UEAwwVKi5wcmVwcm9kLnN0dWZm
LmNvLm56MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2YML+gSi+KVL
VaFlVyu/cnZS1073X3uTKJDoNYGeOXtGyPtG4kh7TEP0IJ8ZCqOuV0Wlm/X2yiRv
xv/1QTRuErFbSuyGj+H0C2B+5Q04stmu8XbTk5CkMo9YrmcEvnybMXPhBIL7IAoO
7jrda/QY1cW4X3MolBriV7Pj53hXHmbu/9qAfc52LA6LwhD2PR+iAtHzSHA92jCA
bdFEl/F+feCPwHVe4hR8m0hgO8U0wfs5tybJVSNWqJQ9v8rJDOKmPSYiM+WIBkEo
yuaO4YI2hjfzWAwDGuGx7grUGRwWk/OsGkCOHdwjWIrD/4mldpHdl/PYSBNnMg8a
Xg/e758WCwIDAQABo4IBUTCCAU0wHwYDVR0jBBgwFoAUw5zz/NNGCDS7zkZ/oHxb
8+IIy1kwVwYIKwYBBQUHAQEESzBJMB8GCCsGAQUFBzABhhNodHRwOi8vZ3Yuc3lt
Y2QuY29tMCYGCCsGAQUFBzAChhpodHRwOi8vZ3Yuc3ltY2IuY29tL2d2LmNydDAO
BgNVHQ8BAf8EBAMCBaAwHQYDVR0lBBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMCAG
A1UdEQQZMBeCFSoucHJlcHJvZC5zdHVmZi5jby5uejArBgNVHR8EJDAiMCCgHqAc
hhpodHRwOi8vZ3Yuc3ltY2IuY29tL2d2LmNybDAMBgNVHRMBAf8EAjAAMEUGA1Ud
IAQ+MDwwOgYKYIZIAYb4RQEHNjAsMCoGCCsGAQUFBwIBFh5odHRwczovL3d3dy5y
YXBpZHNzbC5jb20vbGVnYWwwDQYJKoZIhvcNAQELBQADggEBABlmgK5/TVOGz9Dd
MFOEcgF3hgyycWfDWJqdrQYrUVsoIdKdkzmb360iymC0GsOwuDU5hozVHoKMh/+U
RmBAha6XvgaaSqIB8JIjen1T9bacvkeYXj0aIFZb7htrEmCFsvRFq17Ef5r8APQ1
vbAD6pWKzqmquOqxQCu3HQDyPlNh/6lYH330SKfIT2Vm832FKJGWbEtpa4x+WF3s
iL6+p2cIV9oLy2HOS48dyCvXZrSUtATS4eBhkwMyskhfPRkGlkeqE7iCwWcfYk6m
WO9SdWrZl97d+GmhPT9CCwlnfWI4ybXvfYvuy0p4b9eqMkX7+uBnTjHQ4bkUL3XA
U+wNZ6U=
-----END CERTIFICATE-----
 1 s:/C=US/O=GeoTrust Inc./CN=RapidSSL SHA256 CA - G3
   i:/C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
-----BEGIN CERTIFICATE-----
MIIEJTCCAw2gAwIBAgIDAjp3MA0GCSqGSIb3DQEBCwUAMEIxCzAJBgNVBAYTAlVT
MRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMRswGQYDVQQDExJHZW9UcnVzdCBHbG9i
YWwgQ0EwHhcNMTQwODI5MjEzOTMyWhcNMjIwNTIwMjEzOTMyWjBHMQswCQYDVQQG
EwJVUzEWMBQGA1UEChMNR2VvVHJ1c3QgSW5jLjEgMB4GA1UEAxMXUmFwaWRTU0wg
U0hBMjU2IENBIC0gRzMwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCv
VJvZWF0eLFbG1eh/9H0WA//Qi1rkjqfdVC7UBMBdmJyNkA+8EGVf2prWRHzAn7Xp
SowLBkMEu/SW4ib2YQGRZjEiwzQ0Xz8/kS9EX9zHFLYDn4ZLDqP/oIACg8PTH2lS
1p1kD8mD5xvEcKyU58Okaiy9uJ5p2L4KjxZjWmhxgHsw3hUEv8zTvz5IBVV6s9cQ
DAP8m/0Ip4yM26eO8R5j3LMBL3+vV8M8SKeDaCGnL+enP/C1DPz1hNFTvA5yT2AM
QriYrRmIV9cE7Ie/fodOoyH5U/02mEiN1vi7SPIpyGTRzFRIU4uvt2UevykzKdkp
YEj4/5G8V1jlNS67abZZAgMBAAGjggEdMIIBGTAfBgNVHSMEGDAWgBTAephojYn7
qwVkDBF9qn1luMrMTjAdBgNVHQ4EFgQUw5zz/NNGCDS7zkZ/oHxb8+IIy1kwEgYD
VR0TAQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAQYwNQYDVR0fBC4wLDAqoCig
JoYkaHR0cDovL2cuc3ltY2IuY29tL2NybHMvZ3RnbG9iYWwuY3JsMC4GCCsGAQUF
BwEBBCIwIDAeBggrBgEFBQcwAYYSaHR0cDovL2cuc3ltY2QuY29tMEwGA1UdIARF
MEMwQQYKYIZIAYb4RQEHNjAzMDEGCCsGAQUFBwIBFiVodHRwOi8vd3d3Lmdlb3Ry
dXN0LmNvbS9yZXNvdXJjZXMvY3BzMA0GCSqGSIb3DQEBCwUAA4IBAQCjWB7GQzKs
rC+TeLfqrlRARy1+eI1Q9vhmrNZPc9ZE768LzFvB9E+aj0l+YK/CJ8cW8fuTgZCp
fO9vfm5FlBaEvexJ8cQO9K8EWYOHDyw7l8NaEpt7BDV7o5UzCHuTcSJCs6nZb0+B
kvwHtnm8hEqddwnxxYny8LScVKoSew26T++TGezvfU5ho452nFnPjJSxhJf3GrkH
uLLGTxN5279PURt/aQ1RKsHWFf83UTRlUfQevjhq7A6rvz17OQV79PP7GqHQyH5O
ZI3NjGFVkP46yl0lD/gdo0p0Vk8aVUBwdSWmMy66S6VdU5oNMOGNX2Esr8zvsJmh
gP8L8mJMcCaY
-----END CERTIFICATE-----
 2 s:/C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
   i:/C=US/O=Equifax/OU=Equifax Secure Certificate Authority
-----BEGIN CERTIFICATE-----
MIIDfTCCAuagAwIBAgIDErvmMA0GCSqGSIb3DQEBBQUAME4xCzAJBgNVBAYTAlVT
MRAwDgYDVQQKEwdFcXVpZmF4MS0wKwYDVQQLEyRFcXVpZmF4IFNlY3VyZSBDZXJ0
aWZpY2F0ZSBBdXRob3JpdHkwHhcNMDIwNTIxMDQwMDAwWhcNMTgwODIxMDQwMDAw
WjBCMQswCQYDVQQGEwJVUzEWMBQGA1UEChMNR2VvVHJ1c3QgSW5jLjEbMBkGA1UE
AxMSR2VvVHJ1c3QgR2xvYmFsIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB
CgKCAQEA2swYYzD99BcjGlZ+W988bDjkcbd4kdS8odhM+KhDtgPpTSEHCIjaWC9m
OSm9BXiLnTjoBbdqfnGk5sRgprDvgOSJKA+eJdbtg/OtppHHmMlCGDUUna2YRpIu
T8rxh0PBFpVXLVDviS2Aelet8u5fa9IAjbkU+BQVNdnARqN7csiRv8lVK83Qlz6c
JmTM386DGXHKTubU1XupGc1V3sjs0l44U+VcT4wt/lAjNvxm5suOpDkZALeVAjmR
Cw7+OC7RHQWa9k0+bw8HHa8sHo9gOeL6NlMTOdReJivbPagUvTLrGAMoUgRx5asz
PeE4uwc2hGKceeoWMPRfwCvocWvk+QIDAQABo4HwMIHtMB8GA1UdIwQYMBaAFEjm
aPkr0rKV10fYIyAQTzOYkJ/UMB0GA1UdDgQWBBTAephojYn7qwVkDBF9qn1luMrM
TjAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBBjA6BgNVHR8EMzAxMC+g
LaArhilodHRwOi8vY3JsLmdlb3RydXN0LmNvbS9jcmxzL3NlY3VyZWNhLmNybDBO
BgNVHSAERzBFMEMGBFUdIAAwOzA5BggrBgEFBQcCARYtaHR0cHM6Ly93d3cuZ2Vv
dHJ1c3QuY29tL3Jlc291cmNlcy9yZXBvc2l0b3J5MA0GCSqGSIb3DQEBBQUAA4GB
AHbhEm5OSxYShjAGsoEIz/AIx8dxfmbuwu3UOx//8PDITtZDOLC5MH0Y0FWDomrL
NhGc6Ehmo21/uBPUR/6LWlxz/K7ZGzIZOKuXNBSqltLroxwUCEm2u+WR74M26x1W
b8ravHNjkOR/ez4iyz0H7V84dJzjA1BOoa+Y7mHyhD8S
-----END CERTIFICATE-----
---
Server certificate
subject=/OU=GT00648307/OU=See www.rapidssl.com/resources/cps (c)15/OU=Domain Control Validated - RapidSSL(R)/CN=*.preprod.stuff.co.nz
issuer=/C=US/O=GeoTrust Inc./CN=RapidSSL SHA256 CA - G3
---
No client certificate CA names sent
---
SSL handshake has read 3342 bytes and written 456 bytes
---
New, TLSv1/SSLv3, Cipher is AES128-SHA
Server public key is 2048 bit
Secure Renegotiation IS supported
Compression: NONE
Expansion: NONE
SSL-Session:
    Protocol  : TLSv1
    Cipher    : AES128-SHA
    Session-ID: 99265FD342627543189E57EA53282EF94A28D71E52FFF03B27173A84D5C4F77C
    Session-ID-ctx:
    Master-Key: E1684C7673B837EB58C85E54BCA889787E22C078267460F84CE1193D2DA618C811A990E487CB740B34F6ABC148727293
    Key-Arg   : None
    Start Time: 1442984592
    Timeout   : 300 (sec)
    Verify return code: 0 (ok)
---
closed
```

# TLS performance

Source: https://www.youtube.com/watch?v=0EB7zh_7UE4

* Don't use TLS comopression
    * connection is already using HTTP compression (gzip)
    * TLS compression has a significant memory allocation overhead e.g. up to
      1MB per connection on server

* Elliptic curve ephermeral Diffe-hellman can be favoured as a cipher suite
  because of TLS resumption allows reuse of the session

```
# openssl: check the TLS status of a server
openssl s_client -connect foo.herokuapp.com:443 -tls1 -tlsextdebug -status
```

Most servers out of the box don't automatically rotate keys so your "perfect forward secrecy" won't work properly

* CDNs are good for TLS performance
* TLS has an extra 2 RTTs so you want to minimuse the problem
* The closer you can terminate the TLS connection the better the latency


OCSP Online Certificate Status Program

* browser checks with CA that the server cert is still valid
* You will see it as an empty area on the waterfall graph as the browser goes
  off and does the check
* There are problems with the OCSP protocol
* Chrome doesn't block on the OCSP check, firefox does a live check

OCSP stapling makes getting the OCSP response the servers job. It "staples" the
OCSP response (which has been signed by the CA) to the response it sends the
client

You can see OCSP stapling in the output of `s_client` if it has happened e.g.

```
OCSP response: no response sent
```

* Stapled OSCP increases the size of the certificate.
* If your cert chain is large, browser will need more than 2 RTT to setup TLS
