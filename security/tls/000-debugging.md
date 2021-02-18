# Diagnosing a broken TLS cert

## Online tools

```bash
# ++ easy
# -- you might not want to paste your stuff into a random website
https://www.sslshopper.com/ssl-checker.html
```

## Command line

```bash
# Step 1: see if it works:
#
curl -v https://aaa.example.com
#
# curl will behave similarly to other openssl libraries in your programming language
# curl will not show you **why** it failed if it does fail


# Step 2: diagnose why it failed
#
openssl s_client -showcerts -connect aaa.example.com:443
# shows the certs in the order the server sends them (usually the server's cert will be first in list at depth 0)
# Add -servername aaa.example.com  if the servername does not match domain in -connect
#
# the output here shoudl show all the certs openssl got from the server
# * Note this doesn't return quickly for some reason


# inspect a certificate on the command line using openssl (note the missing -in to make it read from stdin)
openssl x509 <<EOF -text -noout
> <paste your cert text here>
EOF
```

# Fix missing intermediate certs

## Online

```bash
# seems like a useful automatic way to do it but you have to trust a random site :-(
https://certificatechain.io/
```
