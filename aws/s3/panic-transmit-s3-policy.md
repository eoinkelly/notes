This policy, when applied to an IAM user, will allow the Transmit app by Panic
to let a user manage named bucket as that user.

- Transmit requires the `s3:ListAllMyBuckets` permission to let the user see the
  bucket in the Transmit UI to click on it.
    - this action is naturally scoped to an AWS account
    - and cannot be scoped to a particular bucket or object i.e. the resource
      must be '\*'

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
