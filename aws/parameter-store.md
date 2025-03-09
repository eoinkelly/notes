# AWS Parameter store

rovides support for three types of parameters: String, StringList, and SecureString

```
{{ssm:parameter-name}}
```

With one exception, when you create or update a parameter, you enter the parameter value as
plaintext, and Parameter Store performs no validation on the text you enter. For String parameters,
however, you can specify the data type as aws:ec2:image, and Parameter Store validates that the
value you enter is the proper format for an Amazon EC2 AMI; for example: ami-12345abcdeEXAMPLE.

Types:

1. StringList parameters contain a comma-separated list of values, as shown in the following
   examples.
2. String parameters consist of any block of text you enter.
3. A SecureString parameter is any sensitive data that needs to be stored and referenced in a secure
   manner. If you have data that you don't want users to alter or reference in plaintext, such as
   passwords or license keys, create those parameters using the SecureString data type

We recommend using SecureString parameters for the following scenarios:

1. You want to use data/parameters across AWS services without exposing the values as plaintext in
   commands, functions, agent logs, or CloudTrail logs.
1. You want to control who has access to sensitive data.
1. You want to be able to audit when sensitive data is accessed (CloudTrail).
1. You want to encrypt your sensitive data, and you want to bring your own encryption keys to manage
   access.

Only the value of a SecureString parameter is encrypted. Parameter names, descriptions, and other
properties aren't encrypted.

Systems Manager uses AWS KMS to encrypt the parameter value Parameter Store only supports symmetric
encryption KMS keys.

Two parameter tiers: standard, advanced
https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html
store can set a default tier or use intelligent tiering

you can set a throughput limit if the default limit is not enough ?? default limit

A parameter name must be unique within an AWS Region If you specify a parameter hierarchy, the
hierarchy can have a maximum depth of fifteen levels.

A parameter label is a user-defined alias to help you manage different versions of a parameter. When
you modify a parameter, AWS Systems Manager automatically saves a new version and increments the
version number by one. A label can help you remember the purpose of a parameter version when there
are multiple versions

Parameter labels are a lightweight alternative to parameter tags. Your organization might have
strict guidelines for tags that must be applied to different AWS resources. In contrast, a label is
simply a text association for a specific version of a parameter.

## ?? how to get all parameters with a given prefix

you can use

````
aws ssm describe-parameters \
    --parameter-filters "Key=Path,Values=/Production/West"


aws ssm get-parameters-by-path --path /Dev/Web/Oncall
	```
?? can do this from JS?
````
