# S3 Web Hosting

by default when files are marked public they are reset back to private when you upload a new version

1. enable static hosting on the bucket
2. Add a bucket policy to make items in the bucket public by default
    * see http://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteAccessPermissionsReqd.html
    * oddly you have to explicitly reference the bucket name in the policy that is applied to it
    * note that bucket policy only applies to objects uploaded by the bucket owner - you need to use the object's ACL to make objects owned by another user public
    * example bucket policy
        ```
        {
            "Version":"2012-10-17",
            "Statement":[
                {
                    "Sid":"PublicReadGetObject",
                    "Effect":"Allow",
                    "Principal": "*",
                    "Action": ["s3:GetObject"],
                    "Resource": ["arn:aws:s3:::example-bucket/*"]
                }
            ]
        }
    ```

