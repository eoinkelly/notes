# Keys & certs refresher

# Sources

* http://serverfault.com/questions/9708/what-is-a-pem-file-and-how-does-it-differ-from-other-openssl-generated-key-file
* http://www.cryptosys.net/pki/rsakeyformats.html
    TODO Handbook of Applied Cryptography: http://cacr.uwaterloo.ca/hac/

# Basic atoms of a crypto system

1. Public keys
2. Private keys
3. Certificates

Keys and signatures are blobs of binary data. This binary data can be encoded
in various ways and can live in many different container file formats.

There are 2 popular ways to encode binary key data:

1. Binary DER-encoded format. This is sometimes called ASN.1 BER-encoded (there
   is a subtle difference between BER- and DER-encodings: DER is just a
   stricter subset of BER)
2. PEM or base64 format. This is the same data as the DER-encoded file but it
   is encoded in base64 with additional header and footer lines:


x509 keys being

The PEM headers try to identify what is in the file

    -----BEGIN PGP PUBLIC KEY BLOCK-----
    -----BEGIN RSA PRIVATE KEY-----
    -----BEGIN RSA PUBLIC KEY-----
    -----BEGIN PRIVATE KEY-----
    -----BEGIN ENCRYPTED PRIVATE KEY-----
    -----BEGIN PUBLIC KEY-----

## RFC4716

* A container for _public_ keys - it has nothing to say about private keys
* http://www.ietf.org/rfc/rfc4716.txt
* RFC4716 files begin with
    ---- BEGIN SSH2 PUBLIC KEY ----

# File Formats

* .p12
    * PKCS #12 - an archive format for storing _many_ cryptographic objects in a single file
    * part of the _Public Key Cryptography Standards_ (PKCS) family of
      standards published by RSA labs.
    * has file extensions .p12 or .pfx
    * the container itself can be password protected
    * commonly used to store
        * private key with its X.509 cert
        * all the members in a web of trust
* .gpg
    * default binary export from GnuPG
    * ?? what is structure of itd contents ???
* .asc
    * default ASCII armored version of a key export from GnuPG
    * presume it a radix64 encoding of the data that would be in `.gpg` file
* .pub
    * ???
    * does not seem to be a standardized name
    * usually indicates that this file contains a public key???
    * `ssh-keygen` will put the public key it creates in a `.pub` by default
* .pem, .der
    * A file in PEM format (Defined in RFC's 1421 through 1424)
    * a container format
    * keys are in x509 ASN.1 format
    * may include:
        * just public _certificate_
        * or an entire certificate chain including public key, private key, root certificates
    * .der is the binary verion, .pem is the base64 (or "ASCII") version
* .crt, .cert, .cer
    * aliases for .pem or .der (Windows explorer recognises these)
* .key
    * PEM format file containing just a private key
    * not a standardized name, just a convential one
* .pgp
    * ???
* .csr
    * a certificate signing request.
    * contains:
        * details for requested certificate e.g. subject, organisation etc.
        * the public key of the subject
    *  actual format is PKCS10 which is defined in RFC 2986.
    * the CSR is then signed by the CA and they return the _public certificate_ for that subject.
        * this certificate can be in a number of formats
* .p7b
    * a PKCS7 certificate bundle

### Open PGP

* Defined in RFC 4880 (obsoletes 1991, 2440).
* is a protocol for encrypting email using public key cryptography
* defines standard formats for encrypted messages, signatures, private keys,
  and certificates for exchanging public keys.
* deals with packets across networks, does not deal with storage
* specifies how applications should "ascii armor" their packets using a variant
  of Base64 called Radix64- described in the "Open PGP Message Format" RFC (RFC4880
  http://tools.ietf.org/html/rfc4880)

# OpenSSH export formats

OpenSSH format key pair
SSH2 format key pair

seems these are different SSH implementations

can convert between the formats using `ssh-keygen`
http://burnz.wordpress.com/2007/12/14/ssh-convert-openssh-to-ssh2-and-vise-versa/

> Specify a key format for the -i (import) or -e (export) conversion options.
> The supported key formats are: `RFC4716` (RFC 4716/SSH2 public or private
> key), `PKCS8` (PEM PKCS8 public key) or `PEM` (PEM public key).  The
> default conversion format is `RFC4716`.


public key from gnupg to keychain access ???

# gnupg export formats

1. ascii
    * .asc extension
    * ascii encoded (or "armored") version of the key

```
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG/MacGPG2 v2.0.22 (Darwin)
Comment: GPGTools - https://gpgtools.org
key-data-here
-----END PGP PUBLIC KEY BLOCK-----
```

2. binary
    * .gpg extension
    * binary dump of the key


Mac OS Keychain access
* does not import
    * .gpg
    * .asc
* will import
    * .pem
    * .p12

# Certificates

Aside: OpenSSH has its own certificate format which is much simpler than X509 format

X509 certificates are used by SSL, ???

There are 3 main file formats for certificates:

* PEM (DER = binary PEM)
    * Defined in RFC's 1421 through 1424
    * Can contain private keys
    * A container format
    * Keys are in x509 ASN.1 format
    * may include:
        * just public _certificate_
        * or an entire certificate chain including public key, private key, root certificates
    * popular with open source tools
    * files containing PEM data can have many file extensions e.g.
        * .cert
        * .pem
        * .der
        * .key (private key part only)
        * .cer
        * .crt
    * can be converted to PKCS12 using `openssl`
* PKCS7
    * Open standard, used by java, not common.
* PKCS12
    * can contain private keys
    * provides extra security over PEM
    * is a binary format (I think???)
    * can be converted to PEM using `openssl`
    * Unlike PEM files, this container is fully encrypted
    * when you export a p12 from Mac OS Keychain Access you are prompted for a
      password that will protect the whole file.
    * File extenions: .p12, .pkcs12, .pfx
    * convert to PEM:
        * `openssl pkcs12 -in file-to-convert.p12 -out converted-file.pem -nodes` TODO check!


## Certificate structure

Certificates expose the following info in UI

* MD5 fingerprint
* SHA1 fingerprint
* a public key
    * size
    * data
    * algorightm
    * signature
    * exponent
    * key usage: verify
* subject info
    * user id
    * common name
    * organisainal unit
    * organisation
    * country
* issuer info
    * serial number
    * common name
    * organisainal unit
    * organisation
    * country
    * signature algorithm
    * parameters
    * not valid before: {date}
    * not valid after: {date}
* a bunch of extensions

The _Name_ column of the UI is the _Subject Common Name_

The digital certificate provides a solution to the problem of how to find a
user's public key and know that it is valid.

What is in a digital cert:

* some identifying information of subject
* the public key of subject
* a signature of issuer that wraps the whole thing

You can verify a cert using the public key of the _issuer_

* Some certificates (aka public key + id info) are already trusted by the operating system.
* The "System Roots" part of Keychain Access shows these. e.g. the "Apple
  Certificate Authority" has a certificate signed by itself in "System Roots"
  on my laptop. This cert provides a trusted public key for "Apple CA" to my
  laptop
* You can think of "System Roots" as a collection of trusted public keys
* A cert is "certifying" that the public key and the subject data match.

# Trust policy

The issuer of a cert can decide that it is valid for some things but not others
- your software uses a "trust policy" to check that as well as being valid, the
cert is valid for _what_ it was designed for.

To parse a certificate downloaded from apple developer site:

```
openssl x509 -inform der -in ios_development.cer -noout -text
```

This implies that `.cer` files generated by Apple are X509 certificates in DER format.
