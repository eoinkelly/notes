# S3 Permissions

## Recommendations [WIP]

* General
    1. Keep buckets single use (avoid ACL and policy heroics)
        * because:
            * It is very easy to get wrong
            * Bucket policies max 20kb, ACLs max 100 grants so you can run out of space if you need to do a lot
    2. Avoid cross-account bucket permission shennanigans if you can.
    * because:
        * Not always possible but is fairly complex to understand so no all people working on it may fully understand it
    3. Object ACLs are ok but the only recommended use case for the bucket ACL is to grant write permission to the Amazon S3 Log Delivery group to write access log objects to your bucket
        * because:
            * Object ACLs do some stuff that you can't do with a policy but Bucket ACLs can be totally replaced by policy
* Usage categories
    1. Assets bucket for an app. Uses only one of these methods:
        1. Policy only method
            * Objects ACLs are default (no public allowed)
            * Bucket ACL is default (no public allowed)
           * The PAB settings for blocking _ACLs (not policies) shoudl be ticked
            * Bucket policy is:
                ```json
                //
                // Grant permission for the public to access objects within the bucket
                {
                    "Version":"2012-10-17",
                    "Statement":[
                        {
                            "Sid":"PublicReadGetObject",
                            "Effect":"Allow",
                            "Principal": "*",
                            "Action":["s3:GetObject"],
                            "Resource":["arn:aws:s3:::example-bucket/*"]
                        }
                    ]
                }
                ```
        2. ACLs method
           * Objects are have public-read canned ACL
           * Bucket ACL is default (no public settings)
           * No bucket policy
           * The PAB settings for blocking _policies_ (not ACLs) shoudl be ticked
    2. Terraform state bucket
        * All public access block settings should be enabled
        * No bucket policy, no bucket ACL beyond the default, no object ACLs beyond the default
    3. Other
        * varies but should default to being like terraform state bucket

I can maybe thing in terms of the blended permissions from ACLs and policies. Maybe I don't need to care _how_ the thing is implemented?

## Sources

* AWS S3 developer guide
* https://aws.amazon.com/blogs/security/iam-policies-and-bucket-policies-and-acls-oh-my-controlling-access-to-s3-resources/

## The S3 UI

The list of S3 buckets you see has an _Access_ column with the following possible values:

The definition of "public" https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status is helpful to understand the differences.

1. _Public_
    * Public –
        * Everyone has access to one or more of the following: List objects, Write objects, Read and write permissions.
2. _Objects can be public_
    * Objects can be public – The bucket is not public, but anyone with the appropriate permissions can grant public access to objects.
3. _Bucket and objects not public_
    * Buckets and objects not public
        * The bucket and objects do not have any public access and the "Block public access" settings have been set
4. _Only authorized users of this account_
    * Only authorized users of this account – Access is isolated to IAM users and roles in this account and AWS service principals because there is a policy that grants public access.
5. _Error retrieving access type_

The UI has a dropdown allowing you to filter your buckets based on their _Access_ setting.


## General notes

* Buckets and objects within them are considered separate _resources_ for the purposes of access permissions
    * A single object within a bucket can be public
    * Bucket and object permissions are separate from each other
    * An object does not inherit the permissions of the bucket it is in
* All S3 resources are _private_ by default i.e. only the _resource owner_ (the AWS **account** which created the resource) can access it
* Buckets and Objects are owned by AWS **accounts** - they are NOT owned by individual IAM roles or users.

## Map of S3 resources and subresources

You can CRUD (in RESTful style) the following resources and subresources in S3:

```
bucket
    lifecycle
    website
    versioning
    policy
    acl
    cors
    logging
object
    acl
    restore
```

Full list of S3 actions: https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html#amazons3-actions-as-permissions

## Understanding requests to S3

* S3 is a RESTful service
* You send S3 requests via the REST API (or an AWS SDK lib which wraps the REST API for you)
* The S3 service has (conceptually) a single endpoint per region where you send requests to. For example, s3 in the Sydney region can use any of the following (they are conceptually aliases of each other):
    ```
    3.ap-southeast-2.amazonaws.com
    s3.dualstack.ap-southeast-2.amazonaws.com
    account-id.s3-control.ap-southeast-2.amazonaws.com
    account-id.s3-control.dualstack.ap-southeast-2.amazonaws.com
    ```
    * Aside: Dual stack endpoints support both IPv6 and IPv4
* Requests to S3 are either
    1. Authenticated
        * The request includes a signature (computed, in part, from the user's access key ID and secret) that authenticates the sender
        * The SDKs take care of computing the signature for you from whatever Access key ID and secret you give the SDK. If you are making raw HTTP requests, you will need to compute the signature yourself.
    2. Anonymous
        * No signature comes with the request

Access keys (used to create the signature) can come from the following sources:

1. AWS account access keys (associated with a root account)
2. IAM user access keys (keys associated with an IAM user within an AWS account)
3. Temporary security credentials
    * You can grant temporary security creds for a role/federated user/etc. to access your S3 resources

Each _source of access keys_ corresponds to a type of user:

1. Public users
    * users who are not authenticated to AWS at all
2. Authenticated users
    * users who are authenticated (root or IAM user) to **any** AWS account
3. Specific users (either root or IAM) within the AWS account that owns the resource

## S3 ACLs

Good docs about ACLs: https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html

S3 ACLs have 3 predefined groups

1. Authenticated users `AuthenticatedUsers` (requests must be signed)
2. All users `AllUsers` (requests can be signed or unsigned/anonymous)
3. Log delivery group `LogDelivery` (Used for writing server access logs)

ACLs **cannot** grant to an IAM user, only one of the predefined groups or an AWS account

* ACLs are deprecated:
    * > AWS recommends using S3 bucket policies or IAM policies for access control. S3 ACLs is a legacy access control mechanism that predates IAM.
* can be used to grant read/write (not very granular) permissions to the 3 predefined groups or other **AWS Accounts** (not IAM users)
* limited in some ways:
    * can only grant permissions to accounts, not users within an account
    * you cannot grant conditional permissions
    * you cannot explicitly deny permissions
* The have at least one use-case because objects have an ACL but not a policy attached: If AWS account AAA allows AWS account BBB to upload object to a bucket owned by AAA, then ACL is the only way that BBB can control permissions on the object
* Each bucket and object has an ACL attached as a subresource
* ACLs are XML documents
* ACLs can be set when you create a bucket/object or later on
* ACLs can be set via HTTP headers or request body
* an object ACL is also limited to a maximum of 100 grants


Conclusion: I think we should avoid using ACLs unless absolutely required

## S3 and policies

Access control is managed by _policies_. In this case, we use the word "policy" to refer to both the IAM JSON policy documents and XML ACL documents. Policies can be attached to

1. A bucket **or** object (collectively policies attached like this are called "resource based" policies)
    * bucket policies are limited to 20 KB in size. This matters if you need to do very granular things with the objects in a bucket
    * Within this category you can have
      1. S3 Bucket Policy
        * A normal IAM JSON policy. It must identify a `Principal` but does not include a `Resource` because that is implied by its attachment.
        * Can be attached to bucket but not object
      2. S3 ACL
        * Can be attached to buckets or objects
2. An IAM User/Group/Role (called "user based" policies)
    * These are the normal IAM policies attached to users/groups/roles.
    * They reference S3 resources (buckets and objects) via the `Resources` section of each rule

You can combine _resource based policies_ and _user based policies_ to achieve the effect you want or to completely confuse yourself, whichever is easiest.

## Questions

how do ACLs and policies interact, which one wins?


If I know the URL of a resource (bucket or object and its region) and I am one of these personas

1. Somebody logged into an AWS account (any account)
2. Sombody not logged into any AWS account
3. logged in as root in the AWS account which owns the bucket
4. logged in as an IAM user in the AWS account which owns the bucket

then what can I see/do?



User based policies make sense for locking access to your IAM users
    Maybe only use resource based policies for the "any AWS account" user and the "anonymous" user

What should a well configured S3 bucket hosting a website look like?
    A: Only default ACLs where owner has full access but nobody else does. A single bucket policy which gives `s3:GetObject` to the objects within the bucket and nothing else.

## The rails case WIP

Q: does a bucket have to be public to be an assets bucket for a rails app?
A: No, the **bucket** should never be public. The objects within **may** have to be public depending on how the app is configured to store them there.

Q: What are the pros/cons of using the canned public-read ACL on newly created objects? dragonfly and paperclip do this.

* ++ dev doesn't have to set any JSON policy on the bucket to make things work
* -- it is quite annoying to change the ACL on each object in the bucket later on.



Q: What is the optimal permission set for an assets bucket for a rails app?

Writers:
    * IAM user representing the app OR role being used by an EC2 instance
    * writing happens via the AWS Ruby API
Readers
   * Case 1:
        * The rails app acts as a proxy for objects in the bucket - it is the only reader, the general public do not see the bucket
        * uncommon
   * Case 2:
        * The rails app gives our short-term signed URLs to objects within the bucket - the general public requests objects from the bucket via HTTP
            * note HTTPS is not supported
   * Case 3:
        * The rails app gives our long-term URLs to objects within the bucket - the general public requests objects directly from the bucket via HTTP
            * note HTTPS is not supported
        * most common

Many poplular rails file management gems set the canned `public-read` ACL on objects they upload by default

* Dragonfly_s3_storage
    * Gives objects the canned `public-read` ACL by default (via  `x-amz-acl: public-read` header)
* Activestorage
    * Doesn't seem to set an ACL by default but I have not verified yet
    * I think activestorage defaults to giving access via the Rails app and pre-signed URLs and not direct acess which might explain why it doesn't set `public-read`
* Paperclip
    * Gives objects the canned `public-read` ACL by default
    * https://github.com/thoughtbot/paperclip/blob/master/lib/paperclip/storage/s3.rb#L44

So I guess any linting of permissions would have to account for which style of storage the app is using - we would need an "public-read ACL on all objects" style (where we could still assert things about the ACL on the bucket itself) or a "no public-read ACL" style.

Q: is using JSON bucket policy superior to using ACLs?

## S3 URLS

https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteEndpoints.html

There are two kinds of URL

1. S3 REST API URL
    * supports both public and private content
    * returns XML formatted error responses
    * supports **all** bucket and object operations
    * has no support for redirect
    * supports SSL
    * GET/HEAD request to the bucket returns a list of "top level" objects in the bucket
2. Website URL
    * only availble if you have turned on _website hosting_ for the bucket
    * will only return publicly readable content
    * supports redirects at both object and bucket level
    * no SSL support
    * GET/HEAD request to the bucket returns the index.html page configured

Key differences between the URLs: https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteEndpoints.html#WebsiteRestEndpointDiff


```
# REST API URL
s3-<AWS-region>.amazonaws.com/<bucket-name>/<object-name>

# Website URL (only accessible if you have turned on website hosting)
<bucket-name>.s3-website.<AWS-region>.amazonaws.com
```

## Website hosting in a bucket WIP

* HTTP only, no HTTPS - you can use Cloudfront to achieve HTTPS by terminating HTTPS in the Cloudfront distrubtion and using the S3 bucket as the upstream source.

> This bucket must have public read access. It is intentional that everyone in the world will have read access to this bucket
    * Q:Question is that true if you are using cloudfront in front of it? it seems like it wouldn't necessarily be

The HTTP URL of the bucket will be either of:

    <bucket-name>.s3-website.<AWS-region>.amazonaws.com
    <bucket-name>.s3-website-<AWS-region>.amazonaws.com

Note the `.` vs `-` in the URL

Apply this policy to the bucket resource itself to make website hosting work

* This only applies to objects owned by the bucket owner. If the bucket contains objects which are not owned by the bucket owner AWS account then each object should configure their access via their ACL subresource.

```json
//
// Grant permission for the public to access objects within the bucket
{
    "Version":"2012-10-17",
    "Statement":[
        {
            "Sid":"PublicReadGetObject",
            "Effect":"Allow",
            "Principal": "*",
            "Action":["s3:GetObject"],
            "Resource":["arn:aws:s3:::example-bucket/*"]
        }
    ]
}
```

For website hosting, the bucket itself does not have to be public but the objects within it do. That is what the policy above does. `GetObject` can only be applied to objects not buckets so it only allows the public to retrieve objects.

**You do NOT need ACLs to host a website on S3!!!**

Policies are much better than ACLs because you don't need to set a particular ACL on new objects when you create them to make them public

# ------------------------------
Q: What happens when you upload a new object to a bucket which is public?
    where does the object get its default ACL from? how does it decide to be public or not?
    is it up to the uploading requst to set the ACL? or are defaults pulled from somewhere?
A:
    * When you PUT a new object, you can set the ACL but nothing else
    * The **policy** from the bucket will apply if the policy targets this object's name in `Resource` field
    * The default ACL on an object gives the creating AWS account full control but nothing else

Q: What do popular rails asset management gems do about ACLs when they create objects?

Q: how do you best apply an ACL change to multiple objects at once?
A: Use S3 Inventory

# ------------------------------

Best practice: turn on _block public access_ for all ACLs and for new policies **after** you have set the policy above.


## S3 Inventory

A useful tool for bulk applying changes to permissions or ACLs

https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html

## Important feature: Block public access

https://docs.aws.amazon.com/AmazonS3/latest/user-guide/block-public-access.html

* This feature will block any user from adding a policy or ACL which would make the bucket/object "public"
* Can be applied to a single bucket, multiple buckets at once, or all buckets in an account

## What does "public" mean in the S3 UI?

https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status

### Public via ACL

> Amazon S3 considers a bucket or object ACL public if it grants any permissions to members of the predefined AllUsers or AuthenticatedUsers groups. For more information about predefined groups, see Amazon S3 Predefined Groups.

### Public via policy

When evaluating a bucket policy, Amazon S3 begins by assuming that the policy is public. It then evaluates the policy to determine whether it qualifies as non-public. To be considered non-public, a bucket policy must grant access only to fixed values (values that don't contain a wildcard) of one or more of the following:

* A set of Classless Inter-Domain Routings (CIDRs), using aws:SourceIp. For more information about CIDR, see RFC 4632 on the RFC Editor website.
* An AWS principal, user, role, or service principal (e.g. aws:PrincipalOrgID)
* aws:SourceArn
* aws:SourceVpc
* aws:SourceVpce
* aws:SourceOwner
* aws:SourceAccount
* s3:x-amz-server-side-encryption-aws-kms-key-id
* aws:userid, outside the pattern "AROLEID:*"
* aws:PrincipalOrgId



The _Log delivery group_ is what S3 uses to write access logs to your bucket - you should give it access in your ACL only if this bucket is to receive access logs from a website hosted in a bucket