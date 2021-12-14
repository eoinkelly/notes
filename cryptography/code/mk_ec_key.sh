#!/usr/bin/env bash

# with ssh-keygen
ssh-keygen -o -a 100 -t ed25519 -f ./ssh_keygen_ed25519 -C "foo@example.com"


# macos comes with libressl which cannot create ed25519 keys.
#   $ openssl version
#   LibreSSL 2.8.3

# so we need to install openssl from homebrew and provide full path to it
# because it's not linked into /usr/local by default (for good reasons)
#   $ brew install openssl
#
#   $ /usr/local/opt/openssl@3/bin/openssl version
#   OpenSSL 3.0.0 7 sep 2021 (Library: OpenSSL 3.0.0 7 sep 2021)

# generate the key
/usr/local/opt/openssl@3/bin/openssl genpkey -algorithm ed25519 -outform pem -out openssl_ed25519.pem

# extract the public key
/usr/local/opt/openssl@3/bin/openssl pkey -in ./openssl_ed25519.pem -pubout > openssl_ed25519.pub

# these tools create private keys with different formats. Why???