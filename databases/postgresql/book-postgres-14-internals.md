# PostgreSQL 14 Internals

## Overview

* _Postgres instance_
    * A single instance of the database software in memory (includes many processes)
* _Database cluster_
  * All the databases managed by an instance
* `PGDATA`
  * the env var which can set the base directory for all data files in the cluster
  * the data dir is also called `PGDATA` in the Postgres docs
    * the cluster must be initialized (created)
* A newly initialized cluster contains the following DBs:
    * `template0`
        * used when restoring data from a logical backup or creating a DB with a different encoding
        * **You must never modify template0!**
    * `template1`
        * used as the template for all DBS that a user can create in the cluster
    * `postgres`
        * a regular, initially empty, DB you can do whatever you want with
        * it's handy to know the name of a DB which will almost certainly exist when you first connect to the instance
* System catalog
    * The cluster contains objects e.g.
        * tables
        * indexes
        * data types
        * functions
        * etc.
  * Postgres stores all metadata about objects in a set of DB tables **and views** called the _system catalog_
  * Each **database** has it's own system catalog
  * Some system catalog tables belong to the cluster as a whole (technically they are attached to a DB with oid 0)
    * these tables can be accessed from all databases
* You interact with the system catalog with normal SQL queries and DDL queries to make changes
* `psql` has a number of commands which are short-hands for SQL interacting with the system catalog
* naming
  * system catalog tables begin with `pg_`
  * system catalog columns start with a 3 letter prefix which corresponds to the table name
    * e.g. `dat` prefix within `pg_catalog.pg_database`
    * all system catalog tables have a primary key called `oid` which has a type `oid` (a 32 bit int under the hood)
* Schemas
  * are namespaces
  * store all the objects in a **database** (note not cluster objects)
    * each schema is confined to a single database
    * all database objects must belong to a schema
  * can be user defined
  * the predefined schemas are:
    1. public
      * default schema - used for all objects unless another schema is specified
    1. pg_catalog
      * the schema which olds the system catalog tables and views
    1. information_schema
      * an alternate view of `pg_catalog` which complies with the SQL standard
    1. pg_toast
      * used for objects related to TOAST
    1. pg_temp
      * used for temp tables
      * is actually an alias for pg_temp_N schemas which are created and limited to different users
  * posgres has a search path with is used to search for objects when no schema is specified
    * pg_catalog and pg_temp are implicitly added to this path
    ```sql
    postgres=# show search_path;
      search_path
      -----------------
      "$user", public
      (1 row)
    ```
* Sequences
  * are basically a one row table
* "Relation"
  * catch-all term for
    * table
    * view
    * materialized view
    * sequence
    * index
  * These aren't an exact mapping to the mathematical idea of a "relation" - see my CJ Date notes for more detail than you want on that.
    * are stored in the `pg_class` system catalog (it used to be called `pg_relation` - the columns still have the `rel` prefix because of that history)

```bash
# Logical organisation
Cluster
    Databases
        Schemas
            public
                tables
                views
                sequences
                triggers
                ... etc. ...
    Tablespaces
```

```bash
# physical organisation
PGDATA
        ??
```
### Tablespaces

* a directory on a filesystem
* a cluster can have multiple table spaces
  * you can manually distribute your data between tablespaces
  * **the logical and physical layout of data are separate in postgres**

* during cluster init, two tablespaces are created:
    1. pg_default
        * located in `PGDATA/base` directory
        * the default tablespace which is used unless a tablespace is explicitly specified
    1. pg_global
        * stores system catalog objects that are common to the whole cluster
        * located in `PGDATA/global` directory
* other tablespaces are symlinks form `PGDATA/pg_tblspc` to wherever they live on the filesystem

#### How a database is stored: a directory under PGDATA/base

* Every database object has an `oid`
* A database's data files are in `PGDATA/base/<database-oid>/`

```sql
-- Find the database oid from the database name
select oid,datname from pg_database;
--   oid  |                    datname
-- -------+------------------------------------------------
--  14021 | postgres
--  16385 | eoinkelly
--      1 | template1
--  14020 | template0
-- ...
```

### Basic unit of IO: the page

* pages are also called blocks
* usual size is 8kB
  * you can change at compile time but that's rare
* all pages in an instance use the same size
* page lifecycle
    1. pages are moved to the buffer cache
    1. processes can read/write them
    1. pages are flushed back to disk as required

### How a relation is stored: Files & forks

* A single _relation_ is stored on disk across 3 forks (sometimes 4 if table is UNLOGGED)
* Each fork contains a different kind of data
* Each fork is stored across 1+ segment files (max size 1GB)
    * new segments files created automatically when 1GB limit hit
* forks:
    1.  main fork
        * stores the actual tuples (table rows or index rows)
        * all relations (except views) have a main fork
    2.  free space map
        * keeps track of available space within pages
        * organised as a tree
        * is always **at least** 3 pages (24KB @ 8KB page size)
        * exists for both tables and indexes
            * subtly different for indexes: B-tree indexes are sorted so you cannot just insert new data wherever there is a gap. For indexes it keeps track of fully empty pages which can be re-used.
        * used to quickly find a page which can fit new data being inserted
        * has `<main-fork-oid>_fsm` naming scheme
        * only created after the first vacuum happens
        * VACUUM will change the size of these files as the amount of free space changes
    3.  visibility map
        * exists for tables only, not indexes
        * stores two bits for each page:
          1. does this page contain only up-to-date row versions?
            * vacuum can skip these pages because there is nothing to clean up
            * PG knows that row visibility checks can be skipped when reading rows from this page
          2. does this page contain only frozen row versions (these bits are also called the "Freeze map")
        * has `<main-fork-oid>_vm` naming scheme
    4.  Initialization fork (optional)
        * Only available for unlogged tables
        * has `<main-fork-oid>_init` naming scheme


```sql
-- Find the main fork file of the given table name
select pg_relation_filepath('memberships');
--  pg_relation_filepath
-- ----------------------
--  base/19028/19118
-- (1 row)
```

### The Oversized Attributes Storage Technique (TOAST)

* pages are 8kB (by default)
* Postgres likes to have at least 4 **rows** per page (remember that the row is all the attributes!)
    * TODO: why 4 per page?
* This means the soft max length of a **row** is 2000 B
  * there are some headers etc. so 2000 bytes is approximate and can vary a bit between versions
  * i.e. the sum total of the storage requirements for all the attributes in the row must be less than 2kB
* You can adjust the limit at the table level via the `toast_tuple_target` storage parameter
* TOAST is a collection of strategies for handling rows longer than the `toast_tuple_target` (2kiB by default)
* When faced with a value longer than `toast_tuple_target` Postgres will use the _storage strategies_ of each of the columns to decide what to do.
* The available strategies for a column are:
    * `plain`
        * TOAST is not used (only used for short values)
    * `extended`
        * allow for compressing attributes **and** storing them in ~2kB slices in the separate TOAST table
        * it always does **both**
    * `external`
        * allow for storing attributes in ~2kB slices in the separate TOAST table but **not compressing them** beforehand
        * suitable if you know your data won't compress well do Postgres shouldn't try
    * `main`
        * try compressing the attribute first. only move to the TOAST table if it's still too long
* You can inspect the storage strategy with `\d+ my_table` (the `Storage` column)
* Compression is LZ i.e. same algorithm as gzip
  * I'm not sure what the settings are
* The strategy for each column is automatically chosen based on type but you can override
  * columns with types which can be long trigger the creation of the toast table e.g. `text`, `json`
* The algorithm which implements the strategies tries to keep data inline in the page as much as possible
* toast tables always have the same structure
    * `chunk_id` and `chunk_seq` used together to access a chunk
    * `chunk_data` (type `bytea`) to store the 2kiB chunk
    * the storage strategy for the toast table is alwasy `plain` to avoid recursive toasting
    ```sql
    \d+ pg_toast.pg_toast_19254
    -- TOAST table "pg_toast.pg_toast_19254"
    -- Column   |  Type   | Storage
    -- ------------+---------+---------
    -- chunk_id   | oid     | plain
    -- chunk_seq  | integer | plain
    -- chunk_data | bytea   | plain
    -- Owning table: "public.users"
    -- Indexes:
    --     "pg_toast_19254_index" PRIMARY KEY, btree (chunk_id, chunk_seq)
    -- Access method: heap
    ```

How does postgres index a very long value?
* You can use the hash index type if you column is very long
  * -- it only supports `=` operator whereas b-tree supports many more comparison operators

```sql
-- show the namespace and relation name of the toast table which corresponds to the given table name
SELECT relnamespace::regnamespace, relname
FROM pg_class
WHERE oid = (
    SELECT reltoastrelid
    FROM pg_class WHERE relname = 'mytable'
);


-- toast tables always have the same structure
\d+ pg_toast.pg_toast_19254
-- TOAST table "pg_toast.pg_toast_19254"
--    Column   |  Type   | Storage
-- ------------+---------+---------
--  chunk_id   | oid     | plain
--  chunk_seq  | integer | plain
--  chunk_data | bytea   | plain
-- Owning table: "public.users"
-- Indexes:
--     "pg_toast_19254_index" PRIMARY KEY, btree (chunk_id, chunk_seq)
-- Access method: heap
```

Consequences

* You should set the storage to `external` for any column that you know could be long and won't compress well e.g encrypted data or already encrypted data
    * This will make sure Postgres doesn't futilely compress/uncompress it during storage and retrieval
* You probably shouldn't gzip data for storage in Postgres - instead you should make sure that the column used `extended` storage strategy

### Aside: unlogged tables

```sql

CREATE UNLOGGED TABLE foo( ... )
ALTER TABLE foo SET UNLOGGED;

-- show unlogged tables
SELECT relname FROM pg_class WHERE relpersistence = 'u';
```

* Actions performed on UNLOGGED tables are not written to the WAL so these operations are faster
* But not being in the WAL means that
  * you cannot restore consistent data if there is a failure
  * your table will not be replicated to other instances
* So PG deletes all forks of an UNLOGGED relation during recover and overwrites the main fork with the initialization fork
* unlogged tables are like temporary tables which persist after the session
* speed up varies by use-case but it can be significant

Use cases

* anytime data is easy to get again e.g. an import from another table or file
* anytime the data wouldn't be useful after a crash e.g. user sessions should probably be in an unlogged table
* could be handy during migrations or anytime speed matters more than data resiliency


### Aside: always explain, analyse with buffers options

https://www.postgresql.org/docs/current/sql-explain.html

* buffers option is important because it shows IO usage

```sql
EXPLAIN (buffers true, analyze true) SELECT * FROM users;
```

```sql
explain select * from users;
--                         QUERY PLAN
-- ----------------------------------------------------------
--  Seq Scan on users  (cost=0.00..12.10 rows=210 width=344)
-- (1 row)

explain (buffers true) select * from users;
--                         QUERY PLAN
-- ----------------------------------------------------------
--  Seq Scan on users  (cost=0.00..12.10 rows=210 width=344)
--  Planning:
--    Buffers: shared hit=5
-- (3 rows)

explain analyze  select * from users;
--                                              QUERY PLAN
-- ----------------------------------------------------------------------------------------------------
--  Seq Scan on users  (cost=0.00..12.10 rows=210 width=344) (actual time=0.003..0.004 rows=0 loops=1)
--  Planning Time: 0.121 ms
--  Execution Time: 0.024 ms
-- (3 rows)

explain (buffers true, analyze true) select * from users;
--                                              QUERY PLAN
-- ----------------------------------------------------------------------------------------------------
--  Seq Scan on users  (cost=0.00..12.10 rows=210 width=344) (actual time=0.005..0.006 rows=0 loops=1)
--  Planning:
--    Buffers: shared hit=5
--  Planning Time: 0.169 ms
--  Execution Time: 0.033 ms
-- (5 rows)
```

### Aside: Rails auto explain threshold

```ruby
# config/environments/development.rb
config.active_record.auto_explain_threshold_in_seconds = 0.5
```

```ruby
# you can also run explain in ActiveRecord
User.all.explain

# Be aware that Rails will run your query **twice** - it is the equivalent of:
#
#     SELECT "users".* FROM "users"
#     EXPLAIN SELECT "users".* FROM "users"
#
```

### Aside: now() only evaluated once per transaction

This is a bit of a gotcha when writing raw SQL!

```sql
begin;
select now();
--               now
-- -------------------------------
--  2022-07-24 14:36:38.002628+12
-- (1 row)

select now();
--               now
-- -------------------------------
--  2022-07-24 14:36:38.002628+12
-- (1 row)

select now();
--               now
-- -------------------------------
--  2022-07-24 14:36:38.002628+12
-- (1 row)
```

## 1.2 Processes and memory

1. postmaster
    * parent and supervisor of all the other processes
    * will restart its children if they fail
    * allocates some shared memory which is available to all children
        * shared memory includes the page cache (Postgres' own cache of recently accessed pages)
        * the OS also has a page cache which is totally separate but PG pages are read in through the kernel page cache so they may also be cached there (PG does not bypass the kernel cache)
2. startup
    * restores the system after a failure
3. autovacuum
    * removes stale data from tables and indexes
4. wal writer
    * writes WAL entries to disk
5. checkpointer
    * executes checkpoints
    * TODO: ??
6. writer
    * flushes dirty pages to disk
7. stats collector
    * collects usage stats about the instance
8. wal sender
    * sends WAL entries to a replica
9. wal receiver
    * gets WAL entries on a replica
10. connection backend processes (one per connection)
    * one backend process per connection
    * the session exists between the client process (e.g. psql, Rails via pg gem etc.) and the backend process
    * each backend process has memory overhead (caching intermediate results, parsed queries etc.)
    * each backend process has a startup/teardown cost so you don't want to cycle through them too quickly

### Connection pooling

* used to overcome limitation where PG needs a separate process for each connection (connections in PG are fairly heavyweight)
    * Are other SQL dbs better? How does nosql do? AWS DynamoDB has no connection so is good at this but how about the others?
* no built-in support for connection pooling.
* Popular options
    * PgBouncer
        * https://www.pgbouncer.org/
        * cannot load balance between multiple servers out of the box but can use DNS round-robin or a TCP proxy to achieve this
    * Odssey
        * https://github.com/yandex/odyssey
        * Yandex
        * can load balance between multiple servers as well as holding open connections
    * Some frameworks do it for you e.g. ActiveRecord


# Part I: Isolation & MVCC
