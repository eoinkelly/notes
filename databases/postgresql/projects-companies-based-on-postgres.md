# Projects/companies based on Postgres

1. OrioleDB
    - has it on the roadmap
    - https://www.orioledata.com/
    - A postgres plugin
    - Open source https://github.com/orioledb/orioledb/
    - written in C
    - a new storage engine for Postgres
    - public alpha at start 2023
1. Hydra
    - https://hydra.so/
    - an attempt to make an open source Snowflake
1. Supabase
    - an attempt to make an open source Firebase
1. Neon
    - self describes as: Serverless fault-tolerant, branchable (currently only
      schemas but plans for data too)
    - https://neon.tech/
    - based on Postgres
    - runs postgres as compute and have their own custom storage backend
    - custom backend is open source https://github.com/neondatabase/neon
    - scales down to 0
    - allows you to branch your DB
    - still super early, pricing not announced yet (as of end 2022)
    - currently only in 4 regions https://neon.tech/docs/introduction/regions/
    - singapore closest to us
    - there are plans for more regions
    - seems to be built on AWS
    - You have to choose a region
    - so it's serverless but not edge
1. Hasura
    - https://hasura.io/
    - "Instant graphql"
    - GraphQL and REST APIs with auth
    - Open source
1. Timescale
    - Postgres + time series addon
1. Aiven
    - https://aiven.io/postgresql
    - seems to be "managed data platform as a service"
    - an attempt to compete with AWS Aurora I think
    - not just Postgres - they do hosted MySQL, OpenSearch, Redis, Kafka and
      others
    - lives on Azure - has many regions
      https://docs.aiven.io/docs/platform/concepts/availability-zones
1. Crunchy bridge

- https://www.crunchydata.com/products/crunchy-bridge
- are in heaps of regions because they are really a management layer
  https://docs.crunchybridge.com/concepts/plans-pricing/ on top of AWS, GCP,
  Azure

1. Citus

- Available in the cloud as Azure Cosmos DB for PostgreSQL
- https://www.citusdata.com/product/citus-on-azure/

Aside: Oracle has a managed postgres service
