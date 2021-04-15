

6 policy types

1. Identity based
2. Resource based
3. Permissions boundary
4. SCP
    * The pattern in an ACP is to have one Allow statement which allows everything and then explicitly Deny things in granular way
    * If an SCP is considered as part of a request, it **must** have an Allow for the action
5. ACL
6. Session policy

Basics

* statements are logical OR'd together
* policies have a size limits which vary between type and can be extended https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html

Important parts

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


Evaluation

Good diagram: https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html

Every principal except the root account is implicitly denied everything by default. The root account is allowed by default in their account

1. AWS evaulates the collection of policies which apply to a given request
1. An explicit Allow in a policy overrides the default implicit Deny
1. An explicit Deny in a policy overrides any Allow in another policy

* => so the policies must have an Allow for the action in the request
* => but if another policy has an explicit Deny then the deny wins

