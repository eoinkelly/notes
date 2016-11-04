
```
# note the space at start of line to avoid putting it in history
 echo "somesecretthing" | gpg -e -a > output_file.gpg.asc


```

what is "locally signing" a key?
local signatures are used when I cannot verify the key is really from the person
* a local signature will not be exported off the computer it was created on


```
# create a key pair
gpg --gen-key # then follow the prompts

# list your available keys

gpg -k # list public keys on ~/.gnupg/pubring.gpg
gpg -K # list private keys on ~/.gnupg/secring.gpg

# export your public key to a file

# import a public key from a file
```
