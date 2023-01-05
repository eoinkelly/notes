# Rails and pgBouncer

An overview of using pgBouncer to improve the scalability of a Rails app.

- [Rails and pgBouncer](#rails-and-pgbouncer)
  - [Sources](#sources)
  - [Background](#background)
    - [How many connections can my Postgres DB handle?](#how-many-connections-can-my-postgres-db-handle)
    - [Why is the practical maximum no. of connection not the same as max\_connections?](#why-is-the-practical-maximum-no-of-connection-not-the-same-as-max_connections)
    - [Why can't we just set the size of the ActiveRecord pools to match our available DB connections?](#why-cant-we-just-set-the-size-of-the-activerecord-pools-to-match-our-available-db-connections)
    - [Deploys can temporarily spike the number of DB connections](#deploys-can-temporarily-spike-the-number-of-db-connections)
    - [What is max\_connections set to in RDS?](#what-is-max_connections-set-to-in-rds)
    - [What problems does pgBouncer fix?](#what-problems-does-pgbouncer-fix)
    - [Doesn't ActiveRecord drop connections when they are not needed?](#doesnt-activerecord-drop-connections-when-they-are-not-needed)
    - [What about RDS Proxy?](#what-about-rds-proxy)
  - [pgBouncer](#pgbouncer)
    - [How to set up pgBouncer](#how-to-set-up-pgbouncer)
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

## Background

### How many connections can my Postgres DB handle?

* The hard limit on number of connections is the `max_connections` setting.
* In theory, you can set this number based on the available RAM.
* In practice, Postgres performance degrades with very high numbers of connections so your practical limit may be lower than the `max_connections` limit.

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

### What problems does pgBouncer fix?

Introducing pgBouncer does have a latency cost but it solves the following problems:

1. The DB just gets slower as it has more connections
2. ActiveRecord per-process pools interact with slightly unfair load balancing to mean that some processes run out of DB connections.
3. Deploys can cause a big spike in the number of Rails processes which in turn causes a spike in the number of DB connections.

### Doesn't ActiveRecord drop connections when they are not needed?

* Yes but it doesn't do it quickly enough to be any use in production environments.
* From https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/ConnectionPool.html
  * The idle timeout defaults to 5m (300 sec)
  * ActiveRecord does not release connections fast enough to be of any use in real production scenarios

### What about RDS Proxy?

> RDS Proxy is a fully managed, highly available database proxy that uses
> connection pooling to share database connections securely and efficiently
> https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-proxy.html

Price is $0.015 per vCPU-hour. Pricing seems to increase based on the number of vCPUs in your DB: larger DB => larger RDS proxy bill

I have no data points on RDS Proxy except that it's PG version can lag RDS's PG version which in turn lags the official PG version.
As of End 2022, PG 15 is latest version, RDS supports PG 14 as does RDS Proxy so this may not be an issue anymore.

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

    TODO

In theory, your instance can only run as many parallel processes as it has CPU cores.
