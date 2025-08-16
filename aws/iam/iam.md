# Identity Access Management IAM

- allows you control authentication **and** authorization in your account
- IAM is global
    - it is not tied to a region
    * any users/groups/rules you create are global
- is eventually consistent - it may take a while for your changes to propagate
- gives you identity federation to
    - Facebook
    - LinkedIn
    - ActiveDirectory
- supports multi-factor authentication
- allows you to provide temporary access for users/devices and services when
  necessary
    - I presume this refers to roles?
- allows you to setup your own password rotation policy
- supports PCI-DSS compliance

## The request context

When you make a request to perform an action on a resource, AWS builds a
_request context_ which includes

1. The action(s) that the sender wants to perform
1. Principal - the user/role/group/federated user/application that sent the
   request
1. Environment data:
    - Your IP address
    - Your user agent
    - Time of day
    - SSL enabled status
1. The resource you are requesting to perform the action on
1. Resource data: other information about the resource e.g. tag name, DynamoDB
   table name
1. Your signature of the request

The _request context_ is used to evaluate and authorize a request.

IAM does authorization by using values from the _request context_ to compare
against whatever policies are defined.

# Users & Groups

Users and groups both have

1. Friendly name
    - Intended for human consumption
1. Path
    - will be used as part of the ARN for this user/group
    - can use `/` as separator e.g. user `JohnDoe` can have path
      `/nz/welly/admins/john-doe`
    - this allows you to create policies which target parts of that path e.g.
      `/nz/welly/*`

# ARN format

```
arn:partition:service:region:account:resource
```

- partition - the "partition" that the resource is in.
    - usually `aws` but in China it would be `aws-cn`
- service
    - name of the service e.g. `iam`
- region
    - region if applicable - it is always empty string for `iam`
- account
    - account Id (no hyphens)
- resource
    - resource name
