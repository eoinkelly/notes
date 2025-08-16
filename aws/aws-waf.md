# AWS WAF

https://docs.aws.amazon.com/waf/latest/developerguide/how-aws-waf-works.html

## Pricing

- Pricing based on
    - Number of web ACLs (%5.00 USD/mon per ACL)
    - Number of rules in each web ACL ($1.00 USD/mon per rule)
    - Number of web requests processed ($0.60 USD per million requests
    - Number of rule groups ($1.00 USD/mon for each rule group, whether managed
      or not)
- No up-front commitment.
- Pricing is same in all regions
- Monthly fees are pro-rated hourly

Implications of the pricing

- Each managed rule group is only $1/month

## Case study

Currently WoM WAF is averages 30 USD per month

2 x web ACL

5 (web ACL fee) + 3 (3 managed rule groups) = 8 USD per

    3 rule groups

## Capacity

- AWS assigns each rule a "capacity" based on how intensive it is to run that
  rule
    - e.g. a rule that checks request size will be lower capacity to a rule
      which regex's the request body
- Capacity measured in WCU = Web ACL Capacity Unit
- web ACLs have a max capacity of 1500 WCU
- Rule groups must get an **immutable** capacity at creation (applies to both
  managed groups and custom made groups)
    - => you can edit the rules in a rule group but cannot exceed the capacity
      set when the group was first created
    - Setting this limit on rule groups means that the web ACLs do not exceed
      their capacity

## Rule groups

Rule groups _ can be managed or unmanaged _ can be paid or free

1. Paid rule groups
    - AWS WAF charges an added fee for each request that it evaluates using a
      paid rule group.
    - AWS WAF also charges a monthly fee for each use of a paid rule group in a
      web ACL.
    - The standard service charges for AWS WAF still apply
2. Free rule groups
    - Managed by AWS
        - These are versioned
        - New versions are not automatically applied - you have to opt in
    - cost is included ???
3. From AWS Marketplace (paid subscriptions)
    - Examples
        - https://aws.amazon.com/marketplace/pp/prodview-wruwzxkc6uvsm
        - https://aws.amazon.com/marketplace/pp/prodview-5srxsbffmxeao
    - Charged
        - base price: monthly per region e.g. $30 USD per region per month
        - then cost per million requests e.g. $1.80 USD per million requests per
          region

## Regexes

AWS WAF does not support the following PCRE patterns:

- Backreferences and capturing subexpressions
- Subroutine references and recursive patterns
- Conditional patterns
- Backtracking control verbs
- The \C single-byte directive
- The \R newline match directive
- The \K start of match reset directive
- Callouts and embedded code
- Atomic grouping and possessive quantifiers

## Logging

If a request matches one of your `COUNT` rules, it will continue processing
until it matches a rule which either `ALLOW` or `DENY`.

This means you cannot easily use the log filtering to filter requests which
match `COUNT` - log filtering only works with labels or terminating actions
(ALLOW|DENY).

You could have each COUNT rule add a label and filter on the label. This is fine
for rules you make yourself but if you put any of AWS's managed rule groups in
COUNT mode, you have to add a label for each rule within the group - this can be
an annoying number of labels to add.

Instead I chose to log everything and use log insights queries to get the info I
needed.

### Cloudwatch log insights query examples

NOTE: this only searches the first element of the nonTerminatingMatchingRules
array. I'm not aware of an easy way to search all elements. It seems the "best"
way is to parse the `@message` string with either globs or regexes to extract
it. The string `nonTerminatingMatchingRules` appears many times in the JSON blob
so this is sufficiently tricky that I didn't bother.

```sh
# find only requests which matched a COUNT rule
fields @message
| filter nonTerminatingMatchingRules.0.action = "COUNT"
| display nonTerminatingMatchingRules.0.ruleId, action, httpRequest.httpMethod, httpRequest.uri, httpRequest.args


# summarise requests which matched at least one COUNT rule
fields @message
| filter nonTerminatingMatchingRules.0.action = "COUNT"
| stats count(*) as NumMatches by nonTerminatingMatchingRules.0.ruleId as MatchingCountRule


# find requests which matched a particular COUNT rule as the first match
fields @message
| filter nonTerminatingMatchingRules.0.ruleId = "MyApp-RateLimit-WholeApp"


# find requests which were blocked but not by the most common block rule
fields @message
| filter action = "BLOCK" and terminatingRuleId != "MyApp-ExcludeBannedFileTypes"
| display terminatingRuleId, action, httpRequest.httpMethod, httpRequest.uri, httpRequest.args
```
