
Open questions

* How to manage errors?


How does serverless work if you need to string
FaaS enforces an event based model
    is it the micro'st of microservices?

QUESTION: can an AWS lambda call another lambda and get a value from it?

You could think of your "big app" as many "mini apps"

## When does it make sense

If you think about your app as a collection of endpoints then serverless makes sens if the usage of all of them together would drop to 0 for a significant % of the time

Or

If your workload is unpredictable
If the usage of some of the endpoints has a really big variation - a variation which could not be handled easily by autoscaling?

Or

"cloud cron jobs"

