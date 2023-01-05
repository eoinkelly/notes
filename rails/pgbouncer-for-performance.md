# Rails and pgBouncer

An overview of using pgBouncer to improve the scalability of a Rails app.

- [Rails and pgBouncer](#rails-and-pgbouncer)
  - [Sources](#sources)
  - [Background](#background)
    - [How many connections can my Postgres DB handle?](#how-many-connections-can-my-postgres-db-handle)
    - [Why is the practical maximum no. of connection not the same as max\_connections?](#why-is-the-practical-maximum-no-of-connection-not-the-same-as-max_connections)
    - [Why can't we just set the size of the ActiveRecord pools to match our available DB connections?](#why-cant-we-just-set-the-size-of-the-activerecord-pools-to-match-our-available-db-connections)
    - [Doesn't ActiveRecord drop connections when they are not needed?](#doesnt-activerecord-drop-connections-when-they-are-not-needed)
    - [Deploys can temporarily spike the number of DB connections](#deploys-can-temporarily-spike-the-number-of-db-connections)
    - [What is max\_connections set to in RDS?](#what-is-max_connections-set-to-in-rds)
    - [What problems does pgBouncer fix?](#what-problems-does-pgbouncer-fix)
    - [What are the downsides of pgBouncer?](#what-are-the-downsides-of-pgbouncer)
    - [pgBouncer alternative: RDS Proxy](#pgbouncer-alternative-rds-proxy)
    - [pgBouncer alternative: odyssey](#pgbouncer-alternative-odyssey)
    - [pgBouncer alternative: pgpool-II](#pgbouncer-alternative-pgpool-ii)
    - [pgBouncer alternative: pgcat](#pgbouncer-alternative-pgcat)
  - [pgBouncer](#pgbouncer)
    - [How to set up pgBouncer](#how-to-set-up-pgbouncer)
      - [Using pgBouncer to do rudimentary read-write vs read routing](#using-pgbouncer-to-do-rudimentary-read-write-vs-read-routing)
      - [Using pgBouncer as a rudimentary weighted load balancer](#using-pgbouncer-as-a-rudimentary-weighted-load-balancer)
      - [pgBouncer failover](#pgbouncer-failover)
      - [pgbconsole](#pgbconsole)
    - [Where should I run pgBouncer?](#where-should-i-run-pgbouncer)
    - [How do I choose the ratio of Rails connections to real connections (N:M)?](#how-do-i-choose-the-ratio-of-rails-connections-to-real-connections-nm)
    - [What is the optimal number of DB connections for a given RDS instance size?](#what-is-the-optimal-number-of-db-connections-for-a-given-rds-instance-size)

## Sources

* Conversations with folks on NZ and AU Ruby Slacks
* https://gist.github.com/Gargron/aa9341a49dc91d5a721019d9e0c9fd11
  * Creator of Mastodon
  * advocates for using pgBouncer
* https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing
* https://edu.postgrespro.com/postgresql_internals-14_parts1-4_en.pdf , but it has a whole section on locks and the whole thing is gold.
  * page 94 of this book talks about why it has to look at each connection
  * > lightweight locks (the non user facing kind) are currently our biggest
    > contributor to db load, page 275 of the book mentions it briefly it as
    > "unpleasant effects". without multiplexing connections through pgbouncer we
    > couldn't run at all
* https://techcommunity.microsoft.com/t5/azure-database-for-postgresql/analyzing-the-limits-of-connection-scalability-in-postgres/ba-p/1757266#wh[…]nt
* https://brandur.org/postgres-connections
* https://www.enterprisedb.com/postgres-tutorials/why-you-should-use-connection-pooling-when-setting-maxconnections-postgres
* https://aws.amazon.com/blogs/database/performance-impact-of-idle-postgresql-connections/
* https://www.youtube.com/watch?v=9_pbEVeMEB4 (How to tame a Mastodon talk)

## Background

### How many connections can my Postgres DB handle?

* The hard limit on number of connections is the `max_connections` setting.
* In theory, you can set this number based on the available RAM.
* In practice, Postgres performance degrades with very high numbers of connections so your practical limit may be lower than the `max_connections` limit.


> New rule of thumb: If you have to set postgres max_connections to above 512, don't.
> https://hazelweakly.me/blog/scaling-mastodon/

No evidence cited for the above number.

> While it is possible to have a few thousand established connections without
> running into problems, there are some real and hard-to-avoid problems
> https://techcommunity.microsoft.com/t5/azure-database-for-postgresql/analyzing-the-limits-of-connection-scalability-in-postgres/ba-p/1757266

The article above is primarily an argument for PG improving snapshot scalability
to better handle large numbers of connections.

    TODO: read this article properly, some interesting details in there


> most users find PostgreSQL's default of max_connections = 100 to be too low
> ...
> Talk to any PostgreSQL expert out there, and they'll give you a range, "a few
> hundred," or some will flat-out say, "not more than 500," and "definitely no
> more than 1000."
> But where do these numbers come from? How do they know that, and how do we
> calculate that? Ask these questions, and you'll only find yourself more
> frustrated, because there isn't a formulaic way to determine that number.
> ...
> So it seems that for this server, the sweet spot was really somewhere between
> 300-400 connections, and max_connections should not be set much higher than
> that, lest we risk forfeiting performance.
>
> So for this server that I’ve set up to be similar to some enterprise-grade
> machines, the optimal performance was when there were 300-500 concurrent
> connections. After 700, performance dropped precipitously (both in terms of
> transactions-per-second and latency). Anything above 1000 connections performed
> poorly, along with an ever-increasing latency. Towards the end, the latency
> starts to be non-linear
>
> https://www.enterprisedb.com/postgres-tutorials/why-you-should-use-connection-pooling-when-setting-maxconnections-postgres

The above article ran PG on a really beefy machine and still the optimal was in the 300-500 connection range.

> our largest RDS postgres instance typically sits at around 1200-1300 open
> connections at peak and it works ok day to day. It's definitely possible to have
> > 500 connections with a beefy server
> james.healy on Ruby AU slack

There is evidence that over 500 can work fine.

### Why is the practical maximum no. of connection not the same as max_connections?

* Sources:
  * https://brandur.org/postgres-connections
* You can create an instance with enough RAM that the memory ceiling isn't the limiting factor for Postgres
* Each connection to Postgres spawns a process
  * Process starts at ~5MB but may grow a lot larger
* the Postmaster and its backend processes use shared memory for communication, and parts of that shared space are global bottlenecks.
* https://brandur.org/postgres-connections says that connection numbers above 500 are problematic.
* The performance of even simple postgres tasks (simple INSERT, SELECT, DELETE) degrades the more backend processes Postgres has open
* So even if it doesn't yet create "a problem", it does slow down your system even when everything is working well

### Why can't we just set the size of the ActiveRecord pools to match our available DB connections?

* Rails connection pools are per individual process
  * which means you have a large number of small pools which grow with the number of processes you run.
* From https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/ConnectionPool.html
  * The idle timeout defaults to 5m (300 sec)
  * ActiveRecord does not release connections fast enough to be of any use in real production scenarios

Running out of connections is bad. Load balancers can rarely distribute work in a way that is totally fair from the DB usage pov because not all work durations are the same - i.e. requests need to check a connection out of the pool for different durations.

This means that load balancing can never be fully fair. An individual instance might get an unfair allocation of requests which could cause it to run out of connections even when other Rails processes are idle. We solve this problem by allocating more connections that we have, assuming that all our instances will not use them at the same time. But if the system is fully loaded then that will happen.

We created a "system at full load" problem by solving the "individual nodes run out of connections at sub full loads" problem.

### Doesn't ActiveRecord drop connections when they are not needed?

* Yes but it doesn't do it quickly enough to be any use in production environments.
* From https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/ConnectionPool.html
  * The idle timeout defaults to 5m (300 sec)
  * ActiveRecord does not release connections fast enough to be of any use in real production scenarios

### Deploys can temporarily spike the number of DB connections

Even if your system is normally stable, deploys can cause spikes.
If your deploy creates new Rails processes/instances before killing old ones then you will get a temporary spike in DB connections.

### What is max_connections set to in RDS?

* `max_connections` may not be our real limit but it is still important
* All DB providers set `max_connections` carefully. The value will depend on the instance size.
* From the docs
  ```bash
  # From: https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Limits.html
  AWS RDS Postgres max_connections:
    Allowed values: 6 - 8388607
    Default value: LEAST({DBInstanceClassMemory/9531392}, 5000)
  ```
* Note the formula above implies that the maximum `max_connections` for Postgres on **any RDS instance** is 5000
* About `DBInstanceClassMemory`
  * measured in bytes
  * the memory available to the DB instance minus stuff required for OS etc.
    * => it is **not the same as the memory available for the given instances class**
  * I have not found a way to read `DBInstanceClassMemory` :-(
* The only way I know of to find out `max_connections` is to spin up a DB of the given size and `show max_connections` in psql.
* You can override `max_connections` in the parameter group but probably shouldn't unless you are super confident you know wtf you are doing.
* Some stuff I found on the web where people had done this:
    ```
    Eoin: I haven't verified this:
    https://serverfault.com/a/982173
    Actual info for Postgresql t3-instances (default.postgres10 parameter group):
    db.t3.micro - 112 max_connections
    db.t3.small - 225 max_connections
    db.t3.medium - 450 max_connections
    db.t3.large - 901 max_connections
    db.t3.xlarge - 1802 max_connections
    db.t3.2xlarge - 3604 max_connections
    Its similar for default.postgres9 and default.postgres11

    https://gist.github.com/guizmaii/1cacffef793c2ba9645083c3e18b3d8c
    So, here are the values I got when I ran the SQL commmand: `show max_connections;` in some RDS instances:
    | Instance type | RAM (GB) | max_connections |
    | ------------- | -------- | --------------- |
    | db.t2.small   | 2        | 198             |
    | db.t2.medium  | 4        | 413             |
    | db.t2.large   | 8        | 856             |
    | db.m4.large   | 8        | 856             |
    | db.r4.large   | 15.25    | 1660            |
    ```
* https://aws.amazon.com/premiumsupport/knowledge-center/rds-mysql-max-connections/
* has some best practices for Postgres as well as MySQL
  * => they recommend increasing instance size first

Aside: https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Managing.html is docs on how Aurora does this

I have observed each PG connection process taking 15-20MB on one project

* https://aws.amazon.com/blogs/database/performance-impact-of-idle-postgresql-connections/
* https://aws.amazon.com/blogs/database/resources-consumed-by-idle-postgresql-connections/
  * notice that once a process takes memory it stays allocated to that process until the process is closed by killing the connection
  * CPU utilisation does go up as the connection count goes up
    > The utilization increased to 2% with 100 idle connections, increased to 3%
    > with 500 idle connections, increased to 5% with 1,000 idle connections,
    > increased to 6% with 1,500 idle connections and increased to 8% with 2,000
    > idle. Note that this utilization is for an instance with 2 vCPUs


### What problems does pgBouncer fix?

Introducing pgBouncer does have a latency cost but it solves the following problems:

1. The DB just gets slower as it has more connections
2. ActiveRecord per-process pools interact with slightly unfair load balancing to mean that some processes run out of DB connections.
3. Deploys can cause a big spike in the number of Rails processes which in turn causes a spike in the number of DB connections.

### What are the downsides of pgBouncer?

* You can't run SQL commands which would change the global state of the connection
  * In particular, you can't use prepared statements (when running in transaction mode which is almost certainly how you'll want to configure it)
* Adds latency
* Additional complexity
* More stuff to maintain
* Security implicaitons - the pooler needs to be secured too, creds need to be managed, https etc.
* You may need server(s) to run the pooler(s) - servers cost money and need patching etc.

### pgBouncer alternative: RDS Proxy

> RDS Proxy is a fully managed, highly available database proxy that uses
> connection pooling to share database connections securely and efficiently
> https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-proxy.html

Price is $0.015 per vCPU-hour. Pricing seems to increase based on the number of vCPUs in your DB: larger DB => larger RDS proxy bill

I have no data points on RDS Proxy except that it's PG version can lag RDS's PG version which in turn lags the official PG version.
As of End 2022, PG 15 is latest version, RDS supports PG 14 as does RDS Proxy so this may not be an issue anymore.

### pgBouncer alternative: odyssey

* https://github.com/yandex/odyssey
  * multi-threaded unlike pgBouncer

### pgBouncer alternative: pgpool-II

* https://www.pgpool.net/mediawiki/index.php/Main_Page
* also handles replication and load balancing but has rep of being a bit more heavyweight than pgBouncer
* https://www.enterprisedb.com/blog/pgpool-vs-pgbouncer
  > In typical scenarios, PgBouncer executes pooling correctly “out of the box,”
  > whereas Pgpool-II requires fine-tuning of certain parameters for ideal
  > performance and functionality
  > ...
  > Pgpool-II is often implemented by organizations because of its added
  > capabilities, but that doesn’t necessarily make Pgpool-II the ideal choice for
  > all use cases. Many perceive Pgpool-II as an end-all solution, but in reality,
  > PgBouncer is often a better solution for scenarios where bringing down database
  > connections is key

### pgBouncer alternative: pgcat

* new, not battle tested yet
* written in rust
* does sharding too so you can grow into sharding if need be

## pgBouncer

### How to set up pgBouncer

You have 3 possible options:

1. Session pooling
  * Real DB connections are assigned when the client opens a session and closed when the session is closed
  * This is not very effective with a Rails app
1. Transaction pooling
  * A real DB connection is assigned for the duration of a transaction
  * Best choice for Rails
  * You cannot make "global" changes to the connection e.g. prepared statements, pub/sub
    * Does this mean that PG based background job managers would be a problem?
  * **You cannot use prepared statements with this** - watch out if you are writing your own raw SQL
1. Statement pooling (not viable)
   * you have to avoid using transactions to do this which means it's not really viable

Heroku has a buildpack which implements a node level pgBouncer by default

This is good but not as good as a single shared pgBouncer

> https://www.crunchydata.com/blog/postgres-at-scale-running-multiple-pgbouncers
Remember that pgBouncer is single threaded

> In general, a single PgBouncer can process up to 10,000 connections. 1,000 or
> so can be active at one time. The exact numbers will depend on your
> configuration and the amount of data you it is copying between the database and
> the application.


You can use systemd to run multiple instances of pgBouncer (one per vCPU on your box)  - see
> https://www.crunchydata.com/blog/postgres-at-scale-running-multiple-pgbouncers
This uses SO_REUSEPORT in linux kernel and systemd to run multiple pgBouncer processes.

pgBouncer creates a virtual `pgbouncer` database which you access via `psql` just like any other DB

* users in the admin users list can do everything
* users in the stats users list can view stats

pgBouncer has the notion of users which can have different limits applied.
You can use this to lock down some apps more tightly than others if they are at risk of overwhelming the DB

#### Using pgBouncer to do rudimentary read-write vs read routing

You can use pgBouncer's aliasing of databases with a the SO_REUSEPORT trick of running multiple pgBouncer processes to achieve some advanced outcomes. Note that if you need these outcomes, one of the alternatives to pgBouncer might be better - these are somewhat clever hacks.

* pgBouncer creates alias DB names which are mapped to real DBs on real PG servers
* You can use this aliasing to have a "readwrite" (or similarly named) DB which only points at your read+write primary and an "readonly" db which points only at a follower DB
  * From the app's POV there are two different databases.
* systemd will invoke each pgBouncer process on a round-robin basis. You can use this to do load balancing if you configure each pgBouncer process to use different databases e.g.
  ```
  pgBouncer@1
    readwrite: primary
    readonly: standby1
  pgBouncer@2
    readwrite: primary
    readonly: standby2
  ```
* Connections to pgBouncer will get either @1 or @2 on a round-robin basis
* This means that the app connecting to `readonly` will get either standby1 or standby2 on a round-robin basis
* Note that the linux kernel round-robin invoking of processes with SO_REUSEPORT is not perfect and can be a bit skewed

#### Using pgBouncer as a rudimentary weighted load balancer

You can tune the round-robin distribution of load by adding more pgBouncer processes.

Imagine that standby2 is much beefier than standby1 - we can control how much load it gets by having more pgBouncer processes target it e.g.

```
pgBouncer@1
  readwrite: primary
  readonly: standby1
pgBouncer@2
  readwrite: primary
  readonly: standby2
pgBouncer@3
  readwrite: primary
  readonly: standby2
```

#### pgBouncer failover

The app connects to pgBouncer. pgBouncer connects to the DB server(s).

If a connection to the DB server goes down, pgBouncer will not terminate the app's connection but will try to find another available DB connection.

This gives you failover if one of your DB servers goes down.

#### pgbconsole

* a `top` alike thing
* a resource monitor for pgBouncer
* can manage multiple pgBouncer instances


### Where should I run pgBouncer?

Options

1. Run pgBouncer on the same instance as the Rails app
  * it seems common to start here
  * ++ you get to multiplex N Rails connections to M real DB connections
  * ++ this works well if the DB is on the same instance as the Rails app because in that case you do care about running out of memory (normally you will hit other PG perf issues before running out of memory on well configured DB hosts)
  * -- the no. of real DB connections scales with the number of instances
1. Run pgBouncer on dedicated instances
  * can be a single pgBouncer cluster or multiple clusters for different workloads with different N:M ratios
  * but eventually teams move to dedicated pgBouncer instance(s)
1. run multiple pgBouncers for different parts of your load (e.g. background jobs, puma etc.)
  * lets you tailor how you allocate real DB connections

### How do I choose the ratio of Rails connections to real connections (N:M)?

Teams use pgBouncer when they need to increase the number of Rails processes/threads and they cannot add more real DB connections without significant slowdowns.

* Teams seem to grow into choosing the ratio.
* They hit the limits of scaling without pgBouncer and then start using it.
* They seem to push the ratio as far as possible for their workload
* Some examples
  * pgBouncer on instance with Rails app. Turns > 50 Puma threads into < 10 real DB connections (approx. 5:1 ratio)
  * pgBouncer on a dedicated instance: Turning 30k Rails connections into 1500 real DB connections (approx. 20:1 ratio)

### What is the optimal number of DB connections for a given RDS instance size?

Obviously this is workload dependent but are there any useful heuristics?

    TODO

In theory, your instance can only run as many parallel processes as it has CPU cores.

https://docs.joinmastodon.org/admin/scaling/#pgbouncer-why
  configures it's PG to top out at 100 connections and suggests you use pgBouncer after that?

https://aws.amazon.com/blogs/database/resources-consumed-by-idle-postgresql-connections/
  hints that for long runninq queries dropping from 100 concurrent connections to 20 can make the 2vCPU DB faster





