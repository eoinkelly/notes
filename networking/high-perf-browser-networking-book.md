# High performance browser networking


## Part 1

Definitions

* Latency: the time for a packet to get from source to destination
    * it is a _time_ e.g. 3ms, 4s
    * it can also be phrased as a _delay_
    * note it is not the same as RTT
* Bandwidth: the maximum "throughput" of a logical or physical communication path
    * throughput = a _rate_ i.e. `{packets/thingies} per {unit-time}`
    * througput = the rate of successful message delivery of the channel

Latency of networks is the sum of 4 delays:

1. Propagation delay
    * time for message to travel from sender to receiver
    * depends on
        1. distance
        2. medium through which the packet travels
    * refractive index = the factor by which propagation is slower in medium X
      than it is in light
        * optical fibre ~ 1.5
2. Transmission delay
    * time required to push all the packet bits onto the link
    * depends on
        1. lenght of packet
        2. data rate of the link
    * examples
        * a 10Mbps link vs a 100Mbps link and a 10Mb file
            * 10Mbps link takes 1s of transmission delay
            * 100Mbps link takes 0.1s of transmission delay
3. Processing delay
    * time required (on the receiver) to process packet header, check for
      bit-level errors and determine packet destination.
    * depends on
        * peformance of the receiver
4. Queueing delay
    * time (on receiver side) that the incoming packet is waiting on a queue to be processed
    * only occurs if the packets arrive at a faster rate than the router can handle
    * depends on
        * the speed of the router
        * how many packets are on the link (aka load)

    Aside:
        large incoming buffers can break TCPs backoff algorithm and have an
        overall negative effect on network performance

* Latency not bandwidth is the reason most websites are slow
* `traceroute` can be used to measure latency of each hop along the path to a server

NOTES UP TO P 27

## Part 2


HAVE QUICK READ TO END OF PART 2
