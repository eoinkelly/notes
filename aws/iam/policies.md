## IAM policies

- [IAM policies](#iam-policies)
- [5 Policy types](#5-policy-types)
- [Evaluation](#evaluation)
- [Overview](#overview)
  - [Resource based policies](#resource-based-policies)
- [Role](#role)
- [Delegation](#delegation)
- [Policy variables](#policy-variables)
- [Wildcards in policies](#wildcards-in-policies)
- [Anatomy of a Policy (12 elements)](#anatomy-of-a-policy-12-elements)
  - [1. Version](#1-version)
  - [2. Id](#2-id)
  - [3. Statement - array of objects](#3-statement---array-of-objects)
    - [1. Sid](#1-sid)
    - [2. Effect](#2-effect)
    - [3. Principal](#3-principal)
    - [4. NotPrincipal](#4-notprincipal)
    - [5. Action](#5-action)
    - [6. NotAction](#6-notaction)
    - [7. Resource](#7-resource)
    - [8. NotResource](#8-notresource)
    - [9. Condition](#9-condition)
- [randoms](#randoms)


## 5 Policy types

1. Identity based Policy
2. Resource based Policy
3. Permissions Boundary Policy
    * can be set on a user or role
    * cannot be inline, must be a policy object
    * do not provide permissions of their own - they only limit the permissions provided by the identity policies attached to the entity
    * the use case:
        * you can give a user JohnDoe the ability to create other IAM users but add a condition that a named permissions policy must be attached
        * you have to also prevent JohnDoe from fiddling with the policy which sets this
4. Service Control Policy
    * There is **always** an SCP involved in evaluating a policy. By default it's the "allow everything" one that AWS applies for you.
    * You always need an explicit `Allow` from an SCP for **every** policy evaluation!
    * THe Allow in an SCP just means "continue evaluating policies"
    * The pattern in an SCP is to have one Allow statement which allows everything and then explicitly Deny things in granular way
    * If an SCP is considered as part of a request, it **must** have an Allow for the action
5. Session Policy
    * When you create a session via `AssumeRole` you can add a JSON policy which can make the session more restricted than the role would normally allow.

A policy can be stored in one of:

1. A dedicated policy object, either customer or AWS managed (Type: identity policy, permissions boundary policy, Service control policy)
2. Inline in a user (Type: Identity policy)
3. Inline in a group (Type: Identity policy)
4. Inline in a role  (Type: Identity policy)
5. Inline in an AssumeRole API call (Type: Session policy)
6. Inline in a resource e.g. S3 bucket (Type: resource policy)

## Evaluation

Good diagram: https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html

Note: An IAM group is not an "IAM entity" from the pov of policies - it's just a way to apply a policy to a bunch of IAM users
    => Only users and roles are "entities" from the pov of policy evaluation

Starting point:

Every principal except the root account is implicitly denied everything by default. The root account is allowed everything in their own account by default.

Steps

1. AWS evaluates the collection of policies which apply to a given request
1. An explicit Allow in a policy overrides the default implicit Deny
1. An explicit Deny in a policy overrides any Allow in another policy

Consequences

* => so the policies must have an Allow for the action in the request
* => but if another policy has an explicit Deny then the deny wins

A cloudtrail event contains most (maybe all?) the things relevant to the policy evaluation process

* separate statements in a policy document are combined with logical OR
* separate policy documents are combined from the same "storage place" are (conceptually) contatenated
    * See the diagram of the evaluation order for details

## Overview

* defines what a "trusted entity" can access
* does not define how long they can access the resource(s)
    * that is defined by what kind of entity you use: users and groups are permanent access but roles have access only for the duration of their session (default seems to be 1 hour)
* Amazon has ~148 existing "policy" objects that you can "attach" to a user, group, role
* You can create your own policy objects
* A policy joins a group of "actions" with a group of "resources" via an ALLOW or DENY
* A policy is a JSON object
* A policy is a bit like a firewall rule - you can allow/deny access to sets of actions within a certain Amazon service and optionally restrict to particular Amazon resources
* A policy object can have many users/groups/roles attached and a user/group/role can have many policy objects attached
* A "managed policy" is one what is kept in the IAM policy repository
    * A managed policy can reference up to 10 entities e.g. user/group/role.
    * Managed policies are automatically updated by Amazon when new features become available
    * Max size of customer managed policy is 5KB and up to 5 versions
    * Can attach a max of 10 managed policy to a user/group/role
    * Resource policies are still inline-only
* An "in-line policy" can be pasted in-line into a particular user/group/role
    * Managed policies are better than inline policies in most situations:
        * Inline does not have versions
        * Inline has different size limits depending on what you attach it to
* Policies are written in _IAM Policy Language_ (A dialect of JSON)
* policies have a size limits which vary between type and can be extended https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html

There are 3 kinds of policies

1. Permissions policy
    * Can be attached to a role/user/group
        * defines what actions and resources the role/user/group can access
    * Can be _inline_ or _attached_
1. Resource based policy
    * embedded directly into a resource
    * are always _inline_ (embedded) into another entity
    * are _inline policies_ which happen to be embedded in a resource rather than embedded in a role/user/group
1. Trust policy
    * Only appears attached to roles
    * Are an example of a resource based policy (trust policies are resource based policies which are embedded into roles)
    * Defines who is allowed to assume a role

### Resource based policies

Some AWS resources support **resource based policies** - you can attach
policies directly to these resources without having to go through an IAM role.

Examples of services which support resource based policies:

1. Amazon Simple Storage Service (S3) buckets
1. Amazon Glacier vaults
1. Amazon Simple Notification Service (SNS) topics
1. Amazon Simple Queue Service (SQS) queues.

Unlike a user-based policy, a resource-based policy specifies who (in the form
of a list of AWS account ID numbers) can access that resource

Scenario:

    You are `you@account-a` and you are accessing a resource in `account-b` via
    a resource-based policy

Advantage:

    You don't have to switch role so you can still access all your stuff in
    `account-a` - handy if you need to copy data to/from `account-b`

Example of resource based policy on an S3 bucket

```json
{
    "Version": "2008-10-17",
    "Id": "Policy1399437007199",
    "Statement": [
        {
            "Sid": "Stmt1399437a002721",
            "Effect": "Allow",
            "Principal": {
                "AWS": "arn:aws:iam::017211111111:user/example"
            },
            "Action": "s3:*",
            "Resource": [
                "arn:aws:s3:::example-assets-staging/*",
                "arn:aws:s3:::example-assets-staging"
            ]
        }
    ]
}
```

## Role

http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html

* Roles are similar to a user except rather than being associated with a person
  it is designed to be able to be assumed by any person/robot who needs it
* A role is a set of permissions
    * it grants access to actions and resources in AWS
* A role can be be used by any of:
    1. An AWS user in the same account
    1. An AWS user in a different account
    1. An AWS web service e.g. EC2
    1. An external user authenticated by an identity provider e.g. SAML, OpenID Connect
* Only IAM users can assume roles - the root account user cannot.
* Roles do not have standard long-term credentials
    * a role has no passwords or access keys
    * instead a temporary security credentials are created when a user assumes a role

Use cases

You can use roles to delegate access to users, applications, or services that
don't normally have access to your AWS resources

> You might want to allow a mobile app to use AWS resources, but not want to
> embed AWS keys within the app (where they can be difficult to rotate and
> where users can potentially extract them)

> You want to give AWS access to users who already have identities defined
> outside of AWS, such as in your corporate directory

> You might want to grant access to your account to third parties so that they
> can perform an audit on your resources

See _delegation_ section below

## Delegation

* grants permission to somebody else to access resources you control
* Setup trust between
    1. the account which controls the resource (the "trusting" account)
    1. the account which wants access to the resource (the "trusted" account)
* Trusted and trusting accounts can be
    1. the same account
    1. two accounts in the same organization
    1. two accounts in different organizations
* Note that trusted and trusting happens at the **account** level, not the IAM user level
    * => as the trusting account you have to trust every user in the trusted account - it is up to an admin of the trusted account to lock down which IAM users can assume the role


To setup a delegation

1. Create a role in the trusting account and attach two policies
    1. Add a permissions policy to the role in the _trusting_ account
        * grant whatever assumes the role the needed permissions to access the resource
        * This is visible in the "Permissions" tab of the AWS console
    1. Add a trust policy to the role
        * says which **accounts** are allowed to grant their users permissions to assume the role
        * Visible under the "Trust relationships" tab in AWS Console
        * Trust relationships for delegation are setup between two **accounts**
          - you cannot delegate access to a single IAM user within an account -
            you must delegate access to any IAM user from that account.
1. To complete the configuration, the administrator of the trusted account must give specific groups or users in that account permission to switch to the role. There doesn't seem to be a handy managed policy for this  so the following snippet is available in the docs:
    ```
    {
    "Version": "2012-10-17",
    "Statement": {
        "Effect": "Allow",
        "Action": "sts:AssumeRole",
        "Resource": "arn:aws:iam::ACCOUNT-ID-WITHOUT-HYPHENS:role/Test*"
    }
    }
    ```
1. After that there are two ways for a user to switch roles
    1. You can then send the users a link that takes the user to the Switch
       Role page with all the details already filled in.
    1. You can provide the user with the account ID number or account alias
       that contains the role and the role name. The user then goes to the
       Switch Role page and adds the details manually.

NB: You cannot switch roles if you sign in as the root account.


Customer managed policies are immutable - when you make changes IAM just makes a new version of the policy


## Policy variables

https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html

* Only available in 2012-10-17 and later format policy files
* They allow you to create smaller policies
* A feature that lets you specify placeholders in a policy.
* When the policy is evaluated, the policy variables are replaced with values that come from the context of the **request** itself.
* Variable syntax is `${variablename}`
* Can only be used in the Resource element and in string comparisons in the Condition element.
* Full list of available policy variables: https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#policy-vars-infotouse


Example:

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": ["s3:ListBucket"],
      "Effect": "Allow",
      "Resource": ["arn:aws:s3:::mybucket"],
      "Condition": {"StringLike": {"s3:prefix": ["${aws:username}/*"]}}
    },
    {
      "Action": [
        "s3:GetObject",
        "s3:PutObject"
      ],
      "Effect": "Allow",
      "Resource": ["arn:aws:s3:::mybucket/${aws:username}/*"]
    }
  ]
}
```

Examples of the most useful policy variables:

```
${aws:username}
${aws:userid}
${aws:SourceIp}
${aws:SecureTransport}
```

## Wildcards in policies

https://steampipe.io/blog/aws-iam-policy-wildcards-reference

IAM policies support 3 kinds of wildcard:

1. `?` single character wildcard
1. `*` all Resource wildcard
1. `*` one or more character in a segment (delimited by `/` or `::` in Resource ARNs)
    * you can use it more than once in a Resource element

You can use wildcards in

Effect = No wildcard support
Action/NotAction = all kinds of wildcard
Resource/NotResource = all kinds of wildcard
Condition = single-char or multi-char only
Principal/NotPrincipal = All-resource only

Some services don't let you specify a single resource so you have to use `*` in the Resource element
    which ones?

## Anatomy of a Policy (12 elements)

Read each policy as:

    "Policy version {Version} with id {Optional Id} says {Statements}."

Read each statement as:

    "Statement {optional ID} says to {Allow|Deny} {Principal} to perform {Actions} on {Resources} provided {Conditions} match"

Create each policy statement to follow EPARC ordering (Effect, Principal, Action, Resource, Condition)
    what about the other elements ? e.g. NotPrincipal

Policy documents are JSON and are made up of the following 12 elements

1. Version
2. Id
3. Statement
    1. Sid
    2. Effect
    3. Principal
    4. NotPrincipal
    5. Action
    6. NotAction
    7. Resource
    8. NotResource
    9. Condition

Reference: https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html

How the major statement parts work in various policy types:

* `Principal` is sometimes required, sometimes forbidden
    1. Identity based = forbidden
    2. Resource based = required
    3. Permissions boundary = ???
    4. SCP = ???
    5. ACL = ???
    6. Session policy = ???
* `Resource` is sometimes required, sometimes forbidden
    1. Identity based = required
    2. Resource based =  optional (if missing, it is set to the resource you are attaching it to
    3. Permissions boundary = ???
    4. SCP = ???
    5. ACL = ???
    6. Session policy = ???
* `Action`
    * always required
    * list of actions
    * not all actions apply to all services so sometimes if the `Principal` is set to `*` you can still constrain it to a service by choosing the right actions
* `Condition`
    * conditions which must be true for the statement to apply


### 1. Version

* Sources
    * https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_version.html
* Technically  optional but you should always include it because without it, AWS defaults to the oldest version which doesn't support features like policy variables
* Recommendation: include it or it defaults to the old version
* defines the version of the policy _language_ not the version of the policy
* two possible values
    1. `2012-10-17`
        * use this version of the policy language
        * includes features not included in the old version e.g. policy variables like `${aws:username}`
    2. `2008-10-17`
        * old, deprecated version of the policy language

### 2. Id

* Sources
    * https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_id.html
* Always optional
* Most AWS services don't care about it
* Some AWS services (for example, Amazon SQS or Amazon SNS) might require this element and have uniqueness requirements for it.
    * A UUID is recommended to ensure uniqueness in these cases

### 3. Statement - array of objects

* Always required (otherwise your policy does nothing)
* is the main element in a policy
* is an `Array<Object>` in JSON terms
* contains an array of individual statements
* contains up to 9 keys:
    1. Sid
    2. Effect
    3. Principal
    4. NotPrincipal
    5. Action
    6. NotAction
    7. Resource
    8. NotResource
    9. Condition

#### 1. Sid

* Always optional
* unique identifier for the statement
* must be unique within the **policy**

Q: do you ever **need** this?

#### 2. Effect

* valid values are `Allow` or `Deny`
* says whether this policy will allow or deny the Principal to do the given Actions to the Resources

#### 3. Principal

    Apply this policy to this principal

Q: does it have to be exactly one, do wildardswork?

* A principal is the thing which makes the _request_ that policies authorize i.e. the "actor"
* Principles can be users, federated users, roles or applications
* Every policy has a **Principal** (sometimes it is implicit and not explicitly stated in the JSON)
    * the principal is only in the JSON in resource based policies
* The Principal is the entity which is being allowed to perform actions or access resources
* Only 3 things which can be principals in a policy
    1. AWS root account
    1. IAM account
    1. Role
* In permissions policies the "Principal" is inferred by what users or roles the policy is attached to
* For services which support _resource based policies_ you identify the Principal in the JSON
* Principal is specified by its ARN

Examples

```
"Principal": {"Aws": "*.*"}

// these are the same (they both refer to the root user and any IAM user in the named account)
"Principal": {"AWS": "0123456789"}
"Principal": {"AWS": "arn:aws:iam::0123456789:root"}

// The IAM user bob is the Principal
"Principal": {"AWS": "arn:aws:iam::0123456789:user/bob"}

// Federates ARNs
"Principal": {"Federated": "graph.facebook.com"}

// Service ARNs
"Principal": {"Service": "ec2.amazonaws.com"}
```

* https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html
* says what thing is being allowed or denied access to resources where "thing" is one of
    * IAM User (AWS user, federated user, assumed role user)
    * AWS User
    * AWS service
* value is the ARN of the AWS account, IAM user, IAM role, federated user, or assumed-role user
* IAM groups cannot be used as the principal
    * used in
    * trust policies
        * a _trust policy_ is a _resource policy_ attached to a role
        * in a role this specifies which things can assume the role
    * resource based policies (i.e. policies embedded directly into a resource). Policies can be embedded into resources like S3 buckets, SNS topics, SQS queues
    * not used in permissions policies because the "principal" is implicit in those policies - it is one of
            * the user the policy is attached to
            * the user in the group the policy is attached to
            * the user which has assumed the role
            ```javascript
            // examples (a given policy only has one Prinipal block)

            "Principal": {
                "AWS": [
                    "arn:aws:iam::123456789012:root", // root account
                    "123456789012", // root account (exactly same as above)
                    "arn:aws:iam::AWS-account-ID:user/user-name-1",  // an IAM user (you can't do this cross account)
                    "999999999999"
                ]
            }

            "Principal": { "Federated": "accounts.google.com" }


            "Principal": { "AWS": "arn:aws:sts::AWS-account-ID:assumed-role/role-name/role-session-name" }

            "Principal": {
                "Service": [
                    "elasticmapreduce.amazonaws.com",
                    "datapipeline.amazonaws.com"
                ]
            }

            "Principal" : { "AWS" : "*" }
            "Principal": "*" // same as above
            ```

Gotcha: `arn:aws:iam::444455556666:root` in a principal doesn't just refer to the root account - it refers to all principals in that account

#### 4. NotPrincipal

    Apply this policy to all principals except the named one

* same syntax as `Principal`
* lets you define exceptions to the policy defined in `Principal`
* lets you do whitelisting

Consequences

    NotPrincpal + Allow => Allow all principals except the named principals (WARNING: this gives access to all users including unauthenticated - DO NOT USE THIS)
    NotPrincpal + Deny => Deny the actions to all principals except the named ones

> Depending on the service that you include in your policy, AWS might validate
> the account first and then the user. If an assumed-role user (someone who is
> using a role) is being evaluated, AWS might validate the account first, then the
> role, and then the assumed-role user. The assumed-role user is identified by the
> role session name that is specified when they assumed the role. Therefore, we
> strongly recommend that you explicitly include the ARN for a user's account, or
> include both the ARN for a role and the ARN for the account containing that
> role.

#### 5. Action

* matches the explicitly listed actions
* There is mostly a 1:1 between actions and AWS API endpoints (there are some exceptions)

#### 6. NotAction

* matches all actions except the explicitly listed actions
* A `NotAction` is not the same as a `Deny`. If you use `"NotAction": "iam:*` to give a user access to everything **except** IAM, a separate policy could still give them access to IAM. If you use a `Deny` effect instead (i.e. explicitly deny IAM rather than implicitly) then the `Deny` will win if another policy accidentaly gives them access to IAM

#### 7. Resource

* statements much include either a Resource or NotResource statement

#### 8. NotResource

* statements much include either a Resource or NotResource statement

#### 9. Condition

https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html

* Optional
* In the Condition element, you build expressions in which you use condition operators (equal, less than, etc.) to match the condition in the policy against values in the **request**.

Condition evaluation logic

* Multiple **values** in a the same key are evaluated using logical OR
* Multiple keys in the same condition are evaluated using logical AND
* Multiple conditions in the same `Condition` statement are evaluated using logical AND


## randoms

Q: is it enough to allow in a resource policy or does the user need to have a policy attached to it allowing it too?