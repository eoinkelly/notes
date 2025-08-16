# Postgres Sharding

Sharding in PG

- MySQL has Vitesse - in the past Postgres didn't have good open source sharding
- Vitesse are not going to do support postgres in the forseeable future
- Open source projects which implement sharding
    - [Citus](https://www.citusdata.com/) (part of MS) was fully open sourced in
      2022
        - by far the most mature solution - actually usable in production
        - the only managed option I know of is Azure
        - https://docs.citusdata.com/en/stable/get_started/what_is_citus.html
        - Citus is a PG extension which enables sharding
    - SQPR
        - https://github.com/pg-sharding/spqr
        - building a sharding system in go
        - very very early
        - seems similar to pgcat in that it's a routing layer which speaks PG
          wire protocol and which dispatches the queries to shards behind the
          router
    - pgcat
        - https://github.com/levkk/pgcat
        - currently in beta
        - sharding (rust) as part of a proxy (also does connection pooler, load
          balancing, failover)

> You can grow to 100k transactions/sec on one big cluster before you _need_ to
> shard. From postgres.fm podcast

TODO: Get more detail/evidence on this
