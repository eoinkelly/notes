# Data intensive applications

We design the app around the tools we have, app is glue code between what pg, me ache, Redis Kafka etc can do

Should we add assumptions about volumes of traffic to quotes template? Should we document dollar/feature vs dollar/request in quotes?



Fault vs failure

* Faults are not a problem but failures are
* Our systems have faults but we can engineer away failures
* Although we generally prefer tolerating faults over preventing faults, there are cases where prevention is better than cure (e.g., because no cure exists). This is the case with security matters,
* Can we use the “focus on tolerating faults rather than preventing them “ to our human processes too?


Measuring response times

* Median response time means half clients were faster and Hale were slower
    * https://www.vividcortex.com/blog/why-percentiles-dont-work-the-way-you-think

