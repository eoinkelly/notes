# Security Token Service

https://docs.aws.amazon.com/STS/latest/APIReference/welcome.html

## Use-case

* Your application has an identity with long-term credentials that it uses to access AWS.
* You have to embed that in your application somehow. By using temporary security credentials (e.g. roles) your app has to change identity to do things. This means that you can turn off access in just one place (your app identity being able to assume the role)
* You package up "access to the resource" into a Role and then decide who can access it - even if that user doesn't exist yet e.g. a federated user or it's a user from a different account (maybe managed by a different team)

TODO: I haven't articulated this well.


## Overview

* STS an HTTP based web service
* lets you request temporary limited-privilege credentials for
    1. IAM users
    2. Federated users
* `https://sts.amazonaws.com` is the default (global) location of STS
* Under the hood this maps to us-east-? (north virginia)
* There are also regional endpoints available in most regions (and AWS recommends you use them instead)
    * e.g. `https://sts.ap-southeast-2.amazonaws.com` for Sydney
    * Some regions have to be manually activated
* STS supports CloudTrail so requests appear there


## AssumeRole

* `AssumeRole` actions are sent to the STS API
* You can pass a JSON policy to the `AssumeRole` API call. This policy can limit the access of the session more than the permissions on the role normally would
* You can also pass in the ARN of a managed policy object to set as session policy
* tagging a session
    * You can add tags to the request that creates the session
    * these tags can override tags already on the role
    * these tags can be seen in CloudTrail