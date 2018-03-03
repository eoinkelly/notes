# aws s3api

The full S3 Rest API is available from the command line via the `aws s3api` command.

```
$ aws s3api help # show man page

# examples
# see details of an object (including how it is encrypted (if at all))
$ aws s3api head-object --bucket eoin-test-111 --key x86-64-psABI-r252.pdf

# upload an object and store it encrypted
$ aws s3api copy-object --copy-source encrypt_me.txt --key encrypt_me.txt --bucket tester --server-side-encryption aws:kms
```
