# CockroachDB

Questions

* How are connections to cluster secured?


Basics

* Note you have to pass the `--cluster=...` option to GUI clients e.g. DBeaver - they don't read it from the connection string very well


## Latency

**TL;DR Setting up the connection is very sensitive to latency. This impacts serverless things like Lambda more than normal apps.**

`./code` uses

the node pg to test latency to a serverless CockroachDB in Singapore

I noticed that creating the connection pool and releasing a client from it are very sensitive to the latency to the database server e.g

* running against Postgres on my local machine
    * it took approx 20ms to create and 20ms to release the pool.
* Running the same against a CockroachDB database in Singapore measued 200ms latency
    * it took 2 sec  to create and 2 sec to release the pool.
    * It makes sense that there is a lot of traffic involved in setting up and releasing the pool.
* Running on an EC2 instance in Sydney measured 100ms latency
    * I forgot to take pool setup and release results but IIRC it was 1.2s - 1.5s range

For "normal" apps we don't really care about this because that cost is only paid at app boot and shutdown but this could be a problem if running in AWS Lambda.

Assuming we host somewhere with 200ms latency:
    Assuming you need data from the database to generate the response, you'll have to wait for that 2 sec pool setup time before you can send it. The release time doesn't impact how quickly your lambda can respond but it does mean it will stay executing for 2 sec after you have sent your response. This implies that the minimum lambda execution time is 4 secs which is a lot longer than we might expect and will cost money in a high traffic environment.

I tried the native extensions for pg but results were (unsurprisingly) the same.

I tried skipping creating the pool in pg. It improves things but the first SQL query takes nearly 2 sec so I'm guessing the work is just deferred. Overall this is an improvement however because it skips the 2sec "release pool" operation.

Possible mitigations

* put the compute in Singapore too. User sees more network latency but they (potentially) get their answer faster because computer exchanges multiple messages with the DB
* Wait until cockroachdb serverless is available in Sydney region