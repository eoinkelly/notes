Microservices

Negatives

* all you devs need to be good at dev ops - you have many services
    * all need failover and backup
    * many different environments
    * load balancers and mesasging layers for plumbing
    * all your devs need to be competent DBAs - hiring is harder
* a change that cuts across multiple services will need to be rolled out in a coordinated way or else you have to support multiple messaging formats which becomes ahrd to visualise

* introducing a new service introduces synchronous coupling so is not always the best solution. The alternative is to add the new ability to all the existing services that need it - this is duplication
* you are introducing a lot more asynchonsity to the system - makes it harder to think about
* testing is hard: when the logic of the app is spread across many different environments it is very hard to recreate the whole thing to test it
* you need good release & deployment automation - not a lot of OSS tooling for this at the moment
* can lead to code duplication e.g. you need a foo-lib but your services are written in different languages

Positives

* each service can be focused on one taks
* you can use the best tool for the job (language, db etc.)
