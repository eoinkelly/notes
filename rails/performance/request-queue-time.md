# Request queue time

> If I was the king of the world, I would make it illegal to horizontally scale
> based on execution time. Scale based on queue depths, people!
>
> Nate Berkopec

Request queue time is the time between the load-balancer/router getting the request and one of your workers accepting it

Ideally the router would add an `X-Request-Start-Time` header and you (or your APM gem) can add some middleware which subtracts that timestamp from `Time.now` to give us the metric


APM which does this:

* New Relic
* Scout

APM which (as of 2019-06-02) does not do this

* Skylight


NB says Request queue time should be less than 20mS, higher than that means you can scale up/out


Routes/Load-balancer comparison

* Heroku adds the header
* AWS ELB does not so you have to make your own reverse proxy to track it


Addons/helpers

* https://railsautoscale.com/ uses _Request queue time_ to implement an autoscaler which is smarter than the default Heroku one
