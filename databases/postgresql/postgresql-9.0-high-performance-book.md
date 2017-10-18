# Chapter 1

## Upgrading Postgres

* PG changes data formats between "minor" revision numbers (minor in semver, "major" in PG culture)

There are 3 main options

1. dump and restore
    1. dump with current version
        * `pg_dump`
            * ++ exports to binary or SQL formats
            * -- only works on a single database
        * `pg_dumpall`
            * -- can only export to SQL
            * ++ dumps all databases as a single SQL file
    2. upgrade to new version (with new tools)
    3. restore with new version
        * pg_restore can do parallel restores in 9.x +
2. in-place upgrade with `pg_upgrade`
    * -- you still need old and new versions installed at same time
    * used to be called `pg_migrator`
    * -- there is more risk with in-place upgrade than dump and restore
    * `pg_upgrade --help` for details of how to use it
        * you still have to manually create the new cluster using the new version of `initdb`
3. use statement replication to bring up a new version
    * slony does _statement replication_ and can go across PG versions
    * ++ you just wait for the replica to catch up and the flip it over w. very little downtime

### QUESTION: can i install two versions of postgres at the same time on ubuntu? (Sat 28 Mar 23:33:19 2015)

* Yes. It seems pretty easy - just have to be careful which version of the command line tools you load in shell
* See http://blog.endpoint.com/2012/08/using-different-postgresql-versions-at.html

# Postgres Contrib

The contrib tools are porting tools, analysis utilities, and plug-in features
that are not part of the core PostgreSQL system.

* brew installs the postgres-contrib tools by default
* `apt-get install postgres-contrib` to get them on Ubuntu

Interesting tools

* Skytools
    * tools from Skype for replication and failover
    * https://wiki.postgresql.org/images/2/28/Moskva_DB_Tools.v3.pdf
* pgpool
    * connection pool manager
* PgBouncer
    * connection pool manager
* pb_buffercache
    * represents the shared buffers as a relation so you can query them
    * can see how much of a relation has been cached
    * this seems like a key tool when performance profiling a DB
    * http://www.postgresql.org/docs/9.4/static/pgbuffercache.html
* pbtune
    * python script to tweak your postgresql.conf based on detected system params
    * original script untouched since 2013
    * online version has seen more recent work https://github.com/le0pard/pgtune
* pgFouine
    * postgres log analyzer in PHP
    * untouched since 2010

Profiling a database usually starts with OS tools like:

* vmstat
* iostat

TODO: dig into those os level tools sometime (but not urgent) (Sun 29 Mar 00:28:54 2015)

General tips for performance tuning

* find the "current bottleneck" and fix that - the results of other fixes will not be visible
* Approach it at a system level not an individual app level

END CHAP 1

# Chapter 9: Database Indexing

An index is an _organised_ list of values that appear in one or more columns in a table

They speed up

1. searching for a value in those columns
2. sorting the results based on those columns (indexes already have an order
    * do NOT rely on an index to force ordering of a query - use an explicit ORDER BY

## Types of index

There are 4 types

1. Balanced-tree (B-tree)
    * the amount of data on the left and right of each split is kept the same
      so the no. of levels you have to descend for any individual row  is
      approximately equal
    * can find a value
    * can compare on
        * equality
            * is/is not equal
            * is/is not NULL
        * range
            * greater than and/or equal
            * less than and/or equal
    * can help with LIKE patterns which search from the start of the value e.g.
      'foo%' not '%foo' but only if locale setup correctly
2. Hash index
    * useful if you are only doing equality searching and don't allow NULL values
    * -- can easily become corrupt if DB crashes
    * ++ only slightly faster than B-tree
    * recommendation: do not use
3. Generalized Inverted Index (GIN)
    * used when the data doesn't have the "single key" <--> "single value" form
    * stores a list of keys with a "posting list" of rows (each row contains that key)
    * a row can appear in the posting list of multiple keys
    * is that a many-to-many relationship ???
    * you can customise the operator class used for an index
    * uses B-tree strucuture to actually store data on disk
    * useful for
        * indexing array values (can search for sub arrays, a value in an array etc.)
        * implementing full text search (there are other ways)
4. Generalized Search Tree Index (GiST)
    * allows you to create a balanced tree structure just by defining how keys are to be treated
    * useful when
        * you ned a customized index
        * operators beyond the normal equality and range checks
        * still want a tree structure
    * can answer questions beyond the usual equality and range comparisons
    * examples:
        * geometric data types allow an index to sort on distance between items
          and whether they intersect.
        * can be used for full-text search
    * can be used instead of a table (not just as indexes) depending on what data you have
    * GiST indexes are lossy because each document is represented in the index by a fixed-length signature
    * is lossy, meaning that the index may produce false matches, and it is
      necessary to check the actual table row to eliminate such false matches.
      (PostgreSQL does this automatically when needed.)

Comparing GIN to GiST

* GIN lookups are ~ 3x faster than GiST
* GIN builds take ~ 3x longer than GiST
* GIN indexes are a bit slower than GiST to update
* GIN indexes are 2x - 3x larger than GiST indexes
* => use GIN for data that doesn't change a lot as it is faster
* => use GiST for data that changes a lot because updates are faster

## Creating indexs without locking

* Creating an index is one of the most intensive things you can do on a database
* building indexes is usually the longest part of a `pg_restore` command
* creating an index locks the table against writes
* you can create indexes concurrently `CREATE INDEX CONCURRENTLY`
    * ++ does not lock the table for writes
    * -- takes much longer to build as it does 2 passes:
        1. scan table once to build index
        2. scan table again to add whatever has changed
    * -- adds risk that duplicate data could be added to a "unique" column while it is being built
        * if PG detects this it will mark the index as INVALID
        * INVALID index is still on disk and is still updated when the table is (so takes resources)
        * To fix INVALID index:
            1. manually drop INVALID indexes and start again
            2. REINDEX it (does not happen concurrently so will lock for writes!)


## Unique indexes

* help with data integrity _and_ performance
* only B-tree indexes can be used as unique indexes!

There are 3 ways to create a unique index

1. mark column as `PRIMARY KEY` in the table create statement (this is the preferred form)
    ```sql
    CREATE TABLE t(k serial PRIMARY KEY, v integer);
    ```

2. alter table to add an index (does exactly the same as 1.)
    ```sql
    -- exactly same as PRIMARY KEY example above
    CREATE TABLE t(k serial PRIMARY KEY, v integer);
    ALTER TABLE t ADD CONSTRAINT k_key UNIQUE (k)`
    ```

3. Explicitly create a unique index
    ```sql
    CREATE UNIQUE INDEX k_key
    ```
    This is considered bad form as it effectively creates a constraint but doesn't label it as such in the table constraints

TODO: Which does rails do? (Tue 31 Mar 06:13:26 2015)

## Important note about NULL:

`NULL` values are not considered equal to one another! You should prevent any
columns that will have unique indexes on them from having NULL value.

## Clustering an index

Makes a new copy of the table sorted by the index you provide, then drops the old data
    * if table takes X MB on disk you need 2X free to do a cluster
Makes it look like the data was inserted into the table in that order
* Run ANALYZE afterwards to update the stats for the table
* is a one-time act - future INSERT statements do not honor the order
* ++ gets faster results for range based queries

## Fill factor

* When you create a new index not every entry in the index block is used
* `fillfactor` controlled amounts of free space are left to allow for
  changes/insertions into the index can happen on the same index blocks thereby
  reducing fragmentation
* default fill factor for B-tree indexes is 90% i.e. 10% free space
* create an index with 100% fillfactor iff the table data will not change
    ```sql
    CREATE INDEX i ON t(v) WITH (FILLFACTOR=100)
    ```

# Rebuilding indexes

* Indexes can get fragmented  over time as inserts and deletes happen to the table
* Use the REINDEX command to rebuild the index
* -- cannot happen concurrently

# Locale

* can effect whether an index is used for text searches
* indexes search text using `C` locale by default
* index needs to be created differently to use any other locale for text comparison
    ```sql
    CREATE INDEX i on t (s text_pattern_ops);
    ```
TODO: find out more abou this (Mon 30 Mar 06:59:02 2015)

```sql
-- show current locale
show lc_collate;
-- "en_NZ.UTF-8"
```

## Multicolumn indexes

* An index can include up to 32 columns
* You can have multiple indexes that include the same column
* e.g. a category and subcategory in your db might have
    * index on category
        * for searchs on category only
    * index on subcategory
        * for searchs on subcategory only
    * index on category, subcategory
        * for searches that involve both
        * not uses for searches that just involve one or the other
* The trade-off with all indexes is that while some searches will get quicker, updates will be slower as the index has to be updated too.
* The more indexes you have the greater the update slowdown.

# sorting

* B-tree indexes store their entries in _ascending_ order.
* NULLs are also stored in the index - they are put last in the table
    ```sql
    CREATE INDEX i ON t(v DESC NULLS FIRST);
    ```
    * remember the query planner will only use an index for a table if a small no. of rows are being returned
    * its thinking is
        * reading the index blocks is optional
        * the planner considers index reads to be random access not sequential accss
        * if enough of the table is being returned it knows that it will have
          to read most of the table blocks anyway so reading the index blocks
          too is unnecessary
            * down side of this is that it has to create a temporary sort table
              which can take a lot of disk activity


# Partial index

* An index doesn't have to cover all values in a column
    ```sql
    CREATE INDEX active_users ON users WHERE active IS true;
    ```
* This index will be used in queries that have a WHERE clause that requires checking that active is `true`
* It can be combined with other indexs using a bitmap index scan

# Expression based indexes

Indexes do not need to contain the same values as columns in a table

```sql
-- the followi
CREATE INDEX i_lower ON t (lower(name));

SELECT * FROM t WHERE lower(name) = 'x';
```

Note that the function lower() needs to be called every time you do an INSERT
or UPDATE on this table now so overhead has increased

# Full text search

* GIN and GiST can be used to implement full text search
* GIN is better for static data, GiST better for data that changes a lot

QUESTION: how do I find out which indexes are in a db (Tue 31 Mar 06:16:50 2015)
QUESTION: how do I find out about a particular index in a database (Tue 31 Mar 06:16:37 2015)

# Find out about indexes

In psql:

```
# describe a table (including show indexes)

    \d+ tablename

# show all indexes in the database (shows index size too)

    \di+

# show indexes on system tables

    \diS+
```

System tables that contain info about indexes:

1. `pg_index`
2. `pg_class`

END CHAP 9

# Chapter 10: Query Optimisation

* Query plans will change over time because they depend on how much data is in
  the table.
* `\timing` in pqsql turns on the display of how long each query took to plan
  and execute
* EXPLAIN will not run a query, EXPLAIN ANALYSE will run a query!
    * the only way PG can know for sure how long it will take is to do it
* Note that running EXPLAIN (ANALYSE) on a query will slow it down so you
  should not read the times it reports as being the same as you will see in
  production. Use the `\timing` option in `psql` to see the time the query
  takes without EXPLAIN

### The cache

There are 2 caches

1. postgres buffer cache
2. OS disk cache

that combine to make data reads faster

The result of a query can come from a "cold cache" state or a "hot cache state"
- you have to account for this when trying different plans - the difference in
speed may just come from the cache being hot, not the difference in your plan!

Use a pattern of running each query 3 times and see if the timing settles
around a time - this allows you to see the "hot cache" state of the query.

In a hot cache staet you get reliable "processing times" but not disk access time


## EXPLAIN output

* is a set of "plan nodes" at various levels
* lower level nodes scan tables and lookup indexes
* higher level nodes take the output of lower level nodes and operate on it

```
eoin_play=# explain select * from blah;
                          QUERY PLAN
---------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37)
(1 row)
Time: 30.762 ms

eoin_play=# explain (analyse) select * from blah;
                                                   QUERY PLAN
-----------------------------------------------------------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=3.022..702.642 rows=1000000 loops=1)
 Planning time: 0.041 ms
 Execution time: 780.472 ms
(3 rows)

Time: 780.934 ms
```

* notice that `EXPLAIN (ANALYSE) ...` really runs the query
* the difference between EXPLAIN and `EXPLAIN ANALYSE` is the "actual" section
* the first set of costs show are estimated only. `EXPLAIN ANALYSE` also shows actual costs by running the query

Breaking down the output:

* `Seq Scan on blah`
    * the action this node represents
* `(cost=0.00..18334.00`
    * first cost is the "startup cost" of the node
        * how much "work" is estimated before this node produces its first row of output
    * second cost is estimates much work it takes to finish running the node
        * the estimate might be wrong e.g. with a LIMIT the node will finish sooner
* `rows=1000000`
    * the no. of rows this node expects to _output_ if it runs to completion
* `width=37)`
    * the estimated average no. of bytes each row output _by this node_ will use in memory
        * it is not the width of the table unless you are doing something like `SELECT *`
* `(actual time=3.022..702.642`
    * first time is how long (in seconds) this node took to produce its first row of output
    * second time is how long this node took to finish executing this node (produce its final row of output)
    * note that the estimates are costs but the acutals are time
* `rows=1000000`
    * the no. of rows this node actually output
    * NB difference between expected rows and actual rows is one of the most common sources of mistakes by the query optimizer
* `loops=1)`
    * some nodes e.g. joins execute more than once so will have a higher `loops` value
    * note that the times reported are _per loop_ so you have to multiply them by the no. of loops to get the true total cost

### How the query optimizer works

The job of the query optimizer is to generate as many query plans as possible for the given query and then pick the one with the lowest cost to execute

* cost computurations are done using arbitrary units that are only loosely associated with real world execution cost
* the optimizer just needs to be able to compare query "ideas" not figure out an absolute cost!!!


* `seq_page_cost`
    * how long it takes to read a single database page from disk when the expectation is that you'll be reading many next to each other
    * the other cost parameters are "essentially relative" to this value
    * defaults to 1.0
* `random_page_cost`
    * Read cost when rows involved are expected to scattered across the disk at random
    * defaults to 4.0 (4X slower than the reference cost)
    * apparently should be changed to 1.4 for SSDs
    * a multiplier that expresses how much more costly a random read is vs. a sequential read
    * Th real-world ratio of a random access vs. sequential access on a hard disk is approx 50:1 but we don't use a 50X multiplier here becasue we need to account for PG buffer cache and the OS disk cache. 4X accounts for these caches
    * Josh berkus recommends a RPC of  1.5 to 2.5 for SSDs and 1.1 - 2.0 for Amazon EBS and Heroku
        * See http://www.databasesoup.com/2012/05/random-page-cost-revisited.html
    * You should also drop RPC if you have a lot of RAM and know you DB is likely to fit in it
* `cpu_tuple_cost`
    * how much it costs to process a single row of data
    * I THINK it is relative to the cost of reading a sequential page in memory
    * default to 0.01 (100X slower than reference cost)
* `cpu_index_tuple_cost`
    * cost to process a single index entry during an index scan
    * default to 0.005 (200X slower than reference)
    * It is a lot less than the cost to process a single row because rows have more header info than index rows do
* `cpu_operator_cost`
    * expected cost to process a _simple_ operator or function
    * e.g. if the query needs to add two numbers then that is an operator cost
    * defaults to 0.0025 (400X slower than reference)

Notice that the optimizer does not know (or care) which pages are in cache when it is considering which plan to use

Every plan breaks down into 5 operatoins

1. sequential read
2. random read
3. process a row
4. process an index entry
5. process an operator

these 5 basic operations are used to build more complicated structures

### Plan analysis tools

1. Yaml output can be easier to read that the default output sometimes
    ```sql
    EXPLAIN (FORMAT YAML) ...
    ```
2. [http://explain.depesz.com/](http://explain.depesz.com/) is a good tool for visualising complex query
3. pgAdmin has the visual explain stuff
    * it will use the thickness of the line to show the % time each node took

# The importance of VERBOSE for ORM problems

```
EXPLAIN (VERBOSE) ...
```

The `VERBOSE` option will show what columns are actually being passed around by
each node - this is really useful for finding ways to cut down the amount of
data you are moving around in the query e.g. queries written by ActiveRecord
that might be pulling too much data

up to p 245
