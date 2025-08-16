# Dates in JS

Javascript date objects are internally stored as a **single number** - the
number of **milliseconds** (note: not seconds) since the epoch. The epoch is
January 1, 1970, UTC (note: the epoch is UTC)

Javascript date objects do not **store** a timezone - they store a single number
representing a date+time in UTC. Everything after that is calculated.

The Date object is UTC but **the basic methods to fetch the date and time or its
components all work in the local (i.e. host system) time zone and offset**.

Date calculations in JS take an implicit extra argument - a timezone. Some
functions allow you to provided a TZ but all will fall back to the system TZ if
you don't.

```js
// Get the name of the TZ that JS thinks is your current local zone
Intl.DateTimeFormat().resolvedOptions().timeZone; // => 'Pacific/Auckland'
```

```bash
# start node with system timezone
$ node

# start node with a custom timezone
$ TZ=UTC node
$ TZ=America/New_York node
```
