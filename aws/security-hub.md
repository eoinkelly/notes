Requires AWS Config to be enabled

## AWS Config

monitors your account and records it's configuration

You tell it

1. Which resource types you want to record
2. Which S3 bucket it should save configuration snapshots and configuration
   history to
3. Optional: setup up SNS to get notifications about the configuration
4. Give Config permissions to access the bucket and the (optional) role
5. Which rules and conformamcne packs you want to turn on

Conformance pack = rules + remidation actions

Aggregator = a resoure type that collects configuation from other accounts and
regions into a single account and region

You can write queries against the stored configuration
