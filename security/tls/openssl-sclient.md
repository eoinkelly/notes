# openssl s_client

The s_client command implements a generic SSL/TLS client which connects to a remote host using SSL/TLS.

It drops you into an interactive environment after it connects. Use `Q` to exit.

```bash
man s_client # for help

# basic
openssl s_client -connect aaa.example.com:443

# Set the TLS Server Name Indicatin (SNI) extension in the ClientHello
# without this option, SNI is set to the to the name given to -connect if it is
# in DNS format, otherwise it is set to 'localhost'
-servername aaa.example.com

-showcerts # usually optional, servers cert is usually first (at depth 0)
# Prints all certificates in the certificate chain presented by the SSL
# service. Useful when troubleshooting missing intermediate CA certificate
# issues.
#
# Displays the server certificate list as sent by the server: it only consists of
# certificates the server has sent (in the order the server has sent them). It is
# not a verified chain.

-cipher
# force a specific cipher

# Step 2: diagnose why it failed
#
openssl s_client -showcerts -servername aaa.example.com -connect aaa.example.com:443
#
# the output here shoudl show all the certs openssl got from the server
# * Note this doesn't return quickly for some reason


# use a client-cert (default is to use none)
-cert
-cert-chain
-certform


# verify the certs ???
-verify <depth>
-verify 5
```


Examples

```bash

# test for a weak cipher
openssl s_client -connect google.com:443 -cipher EXP-RC4-MD5
```
