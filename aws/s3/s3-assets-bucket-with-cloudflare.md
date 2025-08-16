If you are building `example.com` and want assets served from
`assets.example.com` then:

1. Create an s3 bucket named `assets.example.com` - the bucket name **must**
   match the fully qualified domain name of the assets domain. Note that **you
   do not need to enable _Website hosting_ on the S3 bucket**.
1. Add an S3 bucket policy which allows Cloudflare IP addresses to read objects
   from the bucket without authentication. The IP addresses in the example below
   are taken from https://www.cloudflare.com/ips/ . Don't forget you need to put
   your bucket name in the example below:
    ```js
    // The IP addresses here are taken from https://www.cloudflare.com/ips/
    {
        "Version": "2012-10-17",
        "Statement": [
            {
                "Sid": "PublicReadGetObject",
                "Effect": "Allow",
                "Principal": "*",
                "Action": "s3:GetObject",
                "Resource": "arn:aws:s3:::TODO_PUT_YOUR_BUCKET_NAME_HERE/*",
                "Condition": {
                    "IpAddress": {
                        "aws:SourceIp": [
                            "103.21.244.0/22",
                            "103.22.200.0/22",
                            "103.31.4.0/22",
                            "104.16.0.0/12",
                            "108.162.192.0/18",
                            "131.0.72.0/22",
                            "141.101.64.0/18",
                            "162.158.0.0/15",
                            "172.64.0.0/13",
                            "173.245.48.0/20",
                            "188.114.96.0/20",
                            "190.93.240.0/20",
                            "197.234.240.0/22",
                            "198.41.128.0/17",
                            "2400:cb00::/32",
                            "2405:8100::/32",
                            "2405:b500::/32",
                            "2606:4700::/32",
                            "2803:f800::/32",
                            "2a06:98c0::/29",
                            "2c0f:f248::/32"
                        ]
                    }
                }
            }
        ]
    }
    ```
1. In cloudflare setup a CNAME from your assets subdomain to the domain of the
   S3 bucket e.g.
    ```
    assets.example.com CNAME assets.example.com.s3.ap-southeast-2.amazonaws.com
    ```
1. Make sure that you have enabled proxying on the CNAME you just created i.e.
   the little cloud icon should be orange

You can now test your setup by running

```bash
curl -v https://assets.example.com/path/to/some/asset.jpg
```

which should work.

Now change your app to serve asset URLs of the form
`https://assets.example.com/path/to/some/asset.jpg` - in Rails you'll want to
have a look at the `asset_host` config value.
