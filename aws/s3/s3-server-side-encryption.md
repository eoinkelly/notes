# S3 Encryption at rest

http://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html

* S3 encrypts your data **at the object level** as it is being written to disk and decrypts it when you read it back
* Only the object data is encrypted - **object metadata is not encrypted**
* Limitation: You can't enforce whether objects are encrypted with SSE-S3 when they are uploaded using presigned URLs.
    * maybe 'encryption by default' fixes this???
* Server side encryption
    * is transparent to pre-signed URLs
    * happens at the **object** level not the bucket
        * ?=> you can have different kinds of SSE on different objects in the same bucket?
    * does not prevent objects in a bucket from being listed

There are 3 kinds of server side encryption and they are mutually exclusive (you can only use one at a time on a particular _object_)

1. SSE with S3 managed keys (SSE-S3)
    * ++ easiest
    * uses 256-bit Advanced Encryption Standard (AES-256) to encrypt data
    * each object is encrypted with a key and then that key is itself encrypted with a master key. The master key is rotated regularly
1. SSE with AWS KMS managed keys (SSE-KMS)
    * -- has extra charges for the AMS key
    * ++ uses separate permissions for the "envelope key"
    * ++ provides an audit trail of when the key was used and by whom
    * You have the option to create and manage encryption keys yourself, or use
      a default key that is unique to you, the service you're using, and the
      region you're working in.
1. SSE with Customer provided keys (SSE-C)
    * you manage the encryption keys and Amazon S3 manages the encryption

## How to request encrypted storage for a new object

* Add the `x-amz-server-side-encryption` header to the PUT request.
* Set the value of the header to the encryption algorithm AES256 that Amazon S3 supports.
* Amazon S3 confirms that your object is stored using server-side encryption by
  returning the response header x-amz-server-side-encryption.

```
# request headers ...
x-amz-server-side-encryption: AES256

# response headers ...
x-amz-server-side-encryption: ???
```

## Forcing encryption


Enforce encryption by adding a bucket policy which prevents uploads that do not request encrytpion

# Turning on 'Encryption by default' for a bucket

It used to be that you needed to set a bucket policy to reject object PUTs that did not request encryption but now AWS recommend you trigger 'default encryption'

* Once enabled, only **new** objects will be encrypted - existing objects will not be
* To change the encryption state of an existing object, make a copy of the object and delete the source object.
* As of this writing, terraform does not support this option yet

## Triggering SSE from Paperclip

https://github.com/thoughtbot/paperclip/wiki/Encryption-on-amazon-s3

Note that 'encryption by default' means you don't have to do this anymore.
