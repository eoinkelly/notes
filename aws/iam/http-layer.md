# AWS API HTTPS Layer

Underlying the SDKs, CLIs etc. is the HTTPS API

* Every endpoint allows you to use either GET or POST
    * Note that there is a browser imposed size limit on GET requests so sometimes you have to use POST
* The response is always XML
    * The AWS CLI can take the XML and create JSON for you but the response from server is always XML
* HTTPS is always required
* Most Requests must be signed
* Some requests do not need to be e.g. anonymous requets to S3
* Can be debugged with `mitmproxy` - you have to pass `--no-verify-ssl` to the AWS CLI
* IAM has a single global endpoint: `https://iam.amazonaws.com`
* STS has a single global endpoint: `https://sts.amazonaws.com`
    * THere are also regional endpoints for STS (e.g. `https://sts.ap-southeast-2.amazonaws.com`) which are recommended instead of the global endpoint which is in US

## Signing requests

https://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html

* All requests must be signed using
    1. Access key ID
    2. Access key secret
* Steps
  1. Calculate a hash of the request
  1. Use hash + other request info + secret key to create a signature
  1. Add the signature to the HTTP `Authorization` header **OR** add it as a query string to the URL (a URL which contains a signature is called a "pre-signed URL")
* There are multiple versions of the signing algorihtm
    1. Version 4 (current, recommended)
        * Uses `AWS4-HMAC-SHA256` as signing algorithm
        * https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
    1. Version 2 (still exists for SimpleDB)