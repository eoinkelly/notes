# Time

Physical clocks

* Count number of seconds elapsed
* Tell us the time and date

Logical clock


## How time is measured

### Astronomical observations

* Calculate time based on observations of the sun

### Atomic clocks

* Caesium-133 resonates at approx 9 GHz
* 1 second is now defined to be 9,192,631,770 osciallations of that signal
* Accuracy is 1x10-14 (1 sec every 3 million years)
* Very expensive (20k+)

### GPS Satellites (which contain atomic clocks)

* Each GPS satellite contains an atomic clock
    * broadcasts the current time and current location
    * receiver gets signal from multiple satellites
    * reciever can work out where you are based on triangulating these signals
* GPS satellites can be used to get very accurate time

### Quartz crystal osciallators (keeping time in computers)

* Used to implement clocks in computers
* Quartz artificically grown, cut to a size and shape by laser
* Resonates at a frequency which can be tuned by changing size and shape
* Creates an oscillator
* The frequency is _quite_ predicatable
    * Not all crystals osciallate at exactly the same freq due to manufacturing difference
    * Frequency varies by temperature
        * Generally stable 20-30 deg, but deviates quadratically outside that so can drift a lot at high temps e.g. 70 deg C can drift 80 ppm
    * Most computer clocks correct to within 50 ppm
* Drift
    * measured in ppm (parts per million - like % but 1million instead of a 100)
    * 1 ppm drift = 1 microsecond/second = 86 ms/day = 32s/year
* ++ Much less expensive and bulky than an atomic clock

## Time standards

### GMT

* original meaning was time based on astronomical observations
* aka _Solar time_, the way we measured time for thousands of years
* when is the sun in the south as observed from Grenwich observatory in England
* the earth wobbles a bit so the speed of rotation is not constant
* varies a bit so it is averaged over the year

### IAT International Atomic Time (TAI)

* International Atomic Time (TAI)
* Defines `1 day = 24 x 60 x 60 x 9,192,631,770` periods of caesium-133
* Very regular

### UTC

* A compromise between IAT and GMT
* Defined based on atomic clock but builds in adjustments based on astromonical observations
    * These adjustments are called _leap seconds_
    * There can be positive and negative leap seconds added/removed to keep us in line with the
    * There have been 24 since we started doing it (all positive)
* All our timezones are defined as offsets to UTC

> The present form of UTC, with leap seconds, is defined only starting from 1
> January 1972. Prior to that, since 1 January 1961 there was an older form of
> UTC in which not only were there occasional time steps, which were by
> non-integer numbers of seconds, but also the UTC second was slightly longer
> than the SI second, and periodically changed to continuously approximate the
> Earth's rotation. Prior to 1961 there was no UTC, and prior to 1958 there was
> no widespread atomic timekeeping; in these eras, some approximation of GMT
> (based directly on the Earth's rotation) was used instead of an atomic
> timescale

> The meaning of Unix time values below +63072000 (i.e., prior to 1 January
> 1972) is not precisely defined. The basis of such Unix times is best
> understood to be an unspecified approximation of UTC. Computers of that era
> rarely had clocks set sufficiently accurately to provide meaningful
> sub-second timestamps in any case.

#### How is UTC derived from IAT

* The correction is announced several months ahead .e.g. https://datacenter.iers.org/data/latestVersion/6_BULLETIN_A_V2013_016.txt
* Every year, on 30 june and 31 Dec at exactly `23:59:59 UTC`, one of three things happens:
    1. UTC Clock moves to `00:00:00` after 1 second (no change)
    2. Clock moves to `23:59:60` after one second, then to `00:00:00` after another second (positive leap second)
    3. Clock **immediately** moves to `00:00:00`, skipping the `23:59:59` second i.e. clock goes form `23:59:58 -> 00:00:00` (negative leap second)

### Unix time

> the essential definition of Unix time as an encoding of UTC rather than a linear time scale.

* Is UTC time encoded as a number rather than being a linear count of seconds
* Is (mostly) num seconds since 1 Jan 1970
* The unix time **standard** ignores leap seconds!
* Everyone says it is UTC but it's not really
    * Kinda sorta defined in terms of UTC but actually is more defined in terms of IAT

Unix time **in practice** does handle leap seconds:

* Positive leap second
    * Strict POSIX approach
        * reset the counter at the end of the leap second so that the same unix timestamp refers to both the 23:59:60 second and the 00:00:00 second
        * => the counter for that second is ambigious - it refers to either the leap second or the one just after it
        * => if you measure a time diff across this boundary it will be wrong by 1 second
    * NTP approach
        * Varies, can smear the change across a longer period of time
* Negative leap second
    * skip the leap second
    * => no ambiguity but there is now a counter value that doesn't refer to a real second
    * this has never actually happened so we don't really know what could go wrong if we try

### ISO 8601

* A string representation of a timestamp: year, month, day, hour, minute, second, Timezone offset relative to UTC
* Based on UTC so includes leap seconds
    * => To accurately convert between ISO-8601 and Unix time we should really account for leap seconds but we don't.

## A better solution than leap second stepping: Leap second smearing

* Proposed by Google in 2011 in https://googleblog.blogspot.com/2011/09/time-technology-and-leaping-seconds.html
* Don't just insert the new second
* Spread it out over a time period e.g. 1 day (12h before and after where it would normally be inserted)
* Bit of a hack, probably practically as good as we can do

## Clock synchronisation

* Clock skew = diff between two clocks at a point in time
* In asynchronous/partially-synchronous networks (i.e. networks we can't model as perfect) we cannot remove it but we can minimize it

### NTP

* Sources
    * https://developers.redhat.com/blog/2015/06/01/five-different-ways-handle-leap-seconds-ntp/

* Network Time Protocol
* Servers arranged into strata
    1. Statum 0: Atomic clock or GPS receiver
    1. Statum 1: Gets time directly from a stratum 0 device
    1. Statum 2: Gets time directly from a stratum 1 device
    1. Statum 3: ...
* Queries multiple servers, discards outliers, average the rest
* Makes multliple requests to same server, use stats to reduce random error due to variations in network latency
* Reduces clock skew to a few milliseconds **in good network conditions**
* Assumptions
    * Assumes network latency is the same for the outgoing request and incoming response

#### NTP algorithm

```
t1 = send request to server
    message = t1
t2 = time server receives the request
t3 = time that server sends the response
    message = t1, t2, t3
t4 = time that client recieved the response

Total time from send to recieve from client pov = (t4 - t1)
Processing time on server = (t3 - t2)
Round-trip delay = (t4 - t1) - (t3 - t2)

Assume network latency is symmetric
=> Response latency = round-trip-delay / 2

Accurate time = t3 + (response-latency / 2)

if skew < 125ms
    slew the clock
if skew >= 125ms && skew < 1000ms
    step the clock
else
    do nothing, let human sort it out
```

* Terminology
    * Slew
        * slightly speed up/slow down the clock by up to 500ppm per second
        * brings clocks in sysnc within approx 5 mins
    * Step
        * suddenly reset the client clock to estimated server timestamp

**IMPORTANT: NTP can't fix time if it's more than 1 second out!**

NTP running on a machine means that:

* Not all seconds have the same length
* Sometimes time can just jump forwards **or backwards**
    * e.g. if NTP decides to step the clock when you are in the middle of measuring something
* If you are measuring some time period and care about the accuracy then use a _monotonic clock_ instead

#### NTP clients and servers

* systemd-timesyncd.service
    * default on Ubuntu 20.04
    * client only
    * https://wiki.archlinux.org/index.php/systemd-timesyncd
    * `timedatectl show-timesync --all` to verify the config
* chronyd
    * faster to sync than ntpd
    * handles hosts which hibernate/sleep better than ntpd
    * handles flaky network better
    * includes a client and a server
* ntpd
    * the original
    * includes a client and a server

### Monotonic clocks

* Monotonic clocks aren't changed by ntp _stepping_ but are by _slewing_
* Doesn't tell you the time and date - clock usually resets when computer reboots
* Monotonic timesamps cannot be compared across computers
* Is still effected by slewing (presumably because slewing actually changes the notion of what "1 second" means on the box and should, in theory, make the box more accurate at keeping good time)
* But not effected by NTP stepping the clock
* Linux provides `clock_gettime(CLOCK_MONOTONIC)` to get a monotonic clock

```ruby
Process.clock_gettime(Process::CLOCK_MONOTONIC)
```

### Precision Time Protocol (PTP)

https://en.wikipedia.org/wiki/Precision_Time_Protocol

    TODO


