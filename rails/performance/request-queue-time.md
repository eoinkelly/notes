# Request queue time

> If I was the king of the world, I would make it illegal to horizontally scale
> based on execution time. Scale based on queue depths, people!
>
> Nate Berkopec

Layers the request passes through:

Heroku case

1. The Heroku router
1. ??? maybe some heroku stuff that I'm not aware of ???
1. my rails app

AWS case:

1. ELB
1. nginx on the box
1. puma on the box
1. my rails app

Request queue time is the time between the load-balancer/router getting the request and one of your workers accepting it

Ideally the router would add an `X-Request-Start-Time` header and you (or your APM gem) can add some middleware which subtracts that timestamp from `Time.now` to give us the metric


APM which does this:

* New Relic
* Scout

APM which (as of 2019-06-02) does not do this

* Skylight


NateB recommends that _Request queue time_ should be less than 20mS, higher than that means you can scale up/out

Routes/Load-balancer comparison

* Heroku adds the header
* AWS ELB does not so you have to make your own reverse proxy to track it

Addons/helpers

* https://railsautoscale.com/ uses _Request queue time_ to implement an autoscaler which is smarter than the default Heroku one


Measuring latency for AWS load balancers

AWS does measure latency at ELBs. The metrics are called different things for Application ELB and Classic ELB

Application ELB provides `TargetResponseTime` - The time elapsed, in seconds, after the request leaves the load balancer until a response from the target is received. This is equivalent to the target_processing_time field in the access logs.

* https://stackoverflow.com/questions/25712966/understanding-aws-elb-latency
    * discusses what exactly that means.
    * indicates that the TargetResponseTime corresponds pretty well to a `Tr` time metric logged by Haproxy
        > Tr: server response time (HTTP mode only). It's the time elapsed between the
        > moment the TCP connection was established to the server and the moment the
        > server sent its complete response headers. It purely shows its request
        > processing time, without the network overhead due to the data transmission.
    * TODO: this seems to indicate that a client uploading a lot of data would appear as a high response time

