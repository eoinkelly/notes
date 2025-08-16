# OpenSSL

- On Ubuntu
    - `/usr/lib/ssl` is mostly links to under `/etc/ssl`
    - all the openssl stuff is in `/etc/ssl`
- All certs are in `/etc/ssl/certs`
    - that is mostly soft links to stuff under
        - `/usr/share/ca-certificates/mozilla`
- `/usr/share/ca-certificates/mozilla`
    - contains just a bunch of .crt files

## Symmetric ciphers via the Ruby wrapper

https://ruby.github.io/openssl/OpenSSL/Cipher.html

- OpenSSL cipher objects use the builder pattern
- choosing a cipher
    - You choose a cipher by choosing
        1. cipher name
        2. key length in bits
        3. cipher mode
    - you pass a string of the form: `<ciphername>-<keylen>-<ciphermode>`
    - e.g. `aes-256-gcm`
        - cipher = AES
        - key length = 256 bits
        - mode = GCM

- encryption and decryption are very similar operations for symmertic algorithms
  which is why the same class is used for both.
- raw passwords make for terrible symmetric cipher keys
    - you can use a _Password-Based Key Derivation Function 2_ (PBKDF2) function
      to get a good key from a password
    - choosing a good PBKDF2 is another important security choice
    - options:
        - OpenSSL::PKCS5.pbkdf2_hmac_sha1
        - OpenSSL::PKCS5.pbkdf2_hmac
    - TODO: are these options still good?

```ruby
require "openssl"

# list the symmetric ciphers available
OpenSSL::Cipher.ciphers

cipher = OpenSSL::Cip
cipher = OpenSSL::Cipher.new('aes-256-gcm')

# first task after creating the cipher is to decide whether it is encrypting or decrypting
# you must do this first
# the pattern seems to be to build up configuration within the cipher instance by setting options so this option must be set first because it can erase some other config
cipher.decrypt
cipher.encrypt
```

Authenticated Encryption with Associated Data (AEAD)

is a mode of operation of a symmetric cipher Galois Counter Mode GCM CCM

you provide "associated data" (1-16 bytes) to the encryption process which
coputes a tag at the end of the encryption so the output of encryption is tuple
(ciphertext, tag) then you send that tag along with your ciphertext

this tag is used in the decryption process

> by verifying it's validity the authenticity of a ciphertext can be established

who does the verifying?

in OpenSSL tags are 16 bytes by default
