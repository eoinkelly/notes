# Serverless framework

- Use camelCase not snake_care or kebab-case for service and function names
    - it converts \_ to 'Underscore' and - to 'dash' in AWS resource names which
      is very ugly
- `invoke local`
    - runs your code locally direclty in node/ruby/java/python and in docker for
      other things
    - `sls invoke -f my-func` - all AWS API calls are performed as the role
      setup for the lambda
    - `sls invoke local -f my-func` - all AWS API calls are performed as the AWS
      user you have configured locally! Be careful here!

## When does it make sense

- If you think about your app as a collection of endpoints then serverless makes
  sense if the usage of all of them together would drop to 0 for a significant %
  of the time.
- OR If your workload is unpredictable.
    - If the usage of some of the endpoints has a really big variation - a
      variation which could not be handled easily by autoscaling?
- Cloud cron jobs
