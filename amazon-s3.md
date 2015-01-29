
S3 = Simple Storage System

buckets are top level container (bit like top level dirs

* all files uploaded to S3 belong to a bucket
* bucket names must be unique across the whole S3 system

To access S3 you need

1. Access Key ID (identifies your S3 account)
2. Secret Access Key (akin to a password)


For an app to access files on S3 it needs

1. The S3 credentials (see above)
2. The name of the bucket that the files are in


The `aws s3` commands distinguish between 3 kinds of thing:

object
prefix
bucket
