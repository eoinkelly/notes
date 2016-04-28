
This policy will open access for the Transmit app by Panic to the given S3 bucket

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Action": "s3:*",
            "Effect": "Allow",
            "Resource": [
                "arn:aws:s3:::PUT_YOUR_BUCKET_NAME_HERE",
                "arn:aws:s3:::PUT_YOUR_BUCKET_NAME_HERE/*"
            ]
        },
        {
            "Effect": "Allow",
            "Action": "s3:ListAllMyBuckets",
            "Resource": "arn:aws:s3:::*"
        }
    ]
}
```

