# Query Optimisation

Sources

- Chap 10 of High-perf Postgres book

Overview

- Query plans will change over time because they depend on how much data is in
  the table.
- `\timing` in pqsql turns on the display of how long each query took to plan
  and execute
- EXPLAIN will not run a query, EXPLAIN ANALYSE will run a query!
    - the only way PG can know for sure how long it will take is to do it
- Note that running EXPLAIN (ANALYSE) on a query will slow it down so you should
  not read the times it reports as being the same as you will see in production.
  Use the `\timing` option in `psql` to see the time the query takes without
  EXPLAIN

### The cache

There are two levels of cache

1. postgres buffer cache
2. OS disk cache

They combine to make data reads faster.

The result of a query can come from a "cold cache" state or a "hot cache" state

- you have to account for this when trying different plans - the difference in
  speed may just come from the cache being hot, not the difference in your plan!

Tip: Run each query 3 times and see if the timing settles around a time - this
allows you to see the "hot cache" state of the query.

In a hot cache state you get reliable "processing times" but not disk access
time.

## EXPLAIN output

- is a set of "plan nodes" at various levels
- lower level nodes scan tables and lookup indexes
- higher level nodes take the output of lower level nodes and operate on it

QUESTION: do the nodes make a chain or a tree?

```
-- note I should have used EXPLAIN VERBOSE
eoin_play=# EXPLAIN SELECT * FROM blah;
                          QUERY PLAN
---------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37)
(1 row)
Time: 30.762 ms

-- note I should have used EXPLAIN (VERBOSE, ANALYSE)
eoin_play=# EXPLAIN (ANALYSE) SELECT * FROM blah;
                                                   QUERY PLAN
-----------------------------------------------------------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=3.022..702.642 rows=1000000 loops=1)
 Planning time: 0.041 ms
 Execution time: 780.472 ms
(3 rows)

Time: 780.934 ms
```

- notice that `EXPLAIN (ANALYSE) ...` really runs the query!
- the difference between EXPLAIN and `EXPLAIN ANALYSE` is the "actual" section
- the first set of costs show are estimated only. `EXPLAIN ANALYSE` also shows
  actual times by running the query

Breaking down the output:

- `Seq Scan on blah`
    - the action this node represents
- `(cost=0.00..18334.00`
    - first cost is the "startup cost" of the node
        - how much "work" is estimated before this node produces its first row
          of output
    - the second cost is estimates much work it takes to finish running the node
        - the estimate might be wrong e.g. with a LIMIT the node will finish
          sooner
- `rows=1000000`
    - the no. of rows this node expects to _output_ if it runs to completion
- `width=37)`
    - the estimated **average** no. of bytes each row output _by this node_ will
      use in memory
        - it is not the width of the table unless you are doing something like
          `SELECT *`
- `(actual time=3.022..702.642`
    - the first time is how long (in ms) this node took to produce its **first**
      row of output
    - the second time is how long this node took to produce its **final** row of
      output
    - note that the estimates are costs but the acutals are time
- `rows=1000000`
    - the no. of rows this node actually output
    - NB: the difference between expected rows and actual rows is one of the
      most common sources of mistakes by the query optimizer
- `loops=1)`
    - some nodes e.g. joins execute more than once so will have a higher `loops`
      value
    - note that the times reported are _per loop_ so you have to multiply them
      by the no. of loops to get the true total cost

### How the query optimizer works

- The job of the query optimizer is to generate as many query plans as possible
  for the given query and then pick the one with the lowest cost to execute
- cost computurations are done using arbitrary units that are only loosely
  associated with real world execution cost
- the optimizer just needs to be able to compare query "ideas" not figure out an
  absolute cost!!!

Cost examples

- `seq_page_cost`
    - how long it takes to read a single database page from disk when the
      expectation is that you'll be reading many next to each other
    - the other cost parameters are "essentially relative" to this value
    - defaults to 1.0
- `random_page_cost`
    - Read cost when rows involved are expected to scattered across the disk at
      random
    - defaults to 4.0 (4X slower than the reference cost)
    - apparently should be changed to 1.4 for SSDs
    - a multiplier that expresses how much more costly a random read is vs. a
      sequential read
    - Th real-world ratio of a random access vs. sequential access on a hard
      disk is approx 50:1 but we don't use a 50X multiplier here becasue we need
      to account for PG buffer cache and the OS disk cache. 4X accounts for
      these caches
    - Josh berkus recommends a RPC of 1.5 to 2.5 for SSDs and 1.1 - 2.0 for
      Amazon EBS and Heroku
        - See
          http://www.databasesoup.com/2012/05/random-page-cost-revisited.html
    - You should also drop RPC if you have a lot of RAM and know you DB is
      likely to fit in it
- `cpu_tuple_cost`
    - how much it costs to process a single row of data
    - I THINK it is relative to the cost of reading a sequential page in memory
    - default to 0.01 (100X slower than reference cost)
- `cpu_index_tuple_cost`
    - cost to process a single index entry during an index scan
    - default to 0.005 (200X smaller than reference seq_page_cost)
    - It is a lot less than the cost to process a single row because rows have
      more header info than index rows do
- `cpu_operator_cost`
    - expected cost to process a _simple_ operator or function
    - e.g. if the query needs to add two numbers then that is an operator cost
    - defaults to 0.0025 (400X smaller than reference seq_page_cost)

Plans do not take caching into account! The optimizer does not know (or care)
which pages are in cache when it is considering which plan to use

Every plan breaks down a combination of 5 basic operations

1. sequential read
2. random read
3. process a row
4. process an index entry
5. process an operator

```
# commands I have seen in EXPLAIN output
Seq Scan on {sometable}
Nested Loop
GroupAggregate
Sort
Hash Right Join
Index Scan using {someidx} on {sometable}
```

These 5 basic operations are used to build more complicated structures

### Plan analysis tools

1. Yaml output can be easier to read that the default output sometimes
   `EXPLAIN (FORMAT YAML) ...`
2. http://explain.depesz.com is an ok tool for visualising complex query
3. pgAdmin has the visual explain stuff
    - it will use the thickness of the line to show the % time each node took

# Basically always use EXPLAIN VERBOSE for ORM problems

```
EXPLAIN (VERBOSE) ...
```

The `VERBOSE` option will show what columns are actually being passed around by
each node - this is really useful for finding ways to cut down the amount of
data you are moving around in the query e.g. queries written by ActiveRecord
that might be pulling too much data

up to p 245

# EXPLAIN

```sql
-- Setup database
CREATE TABLE blah (c1 INTEGER, c2 TEXT);
INSERT INTO blah SELECT i, md5(random()::text) FROM generate_series(1, 1000000) AS i;

-- note I should have used EXPLAIN VERBOSE
eoin_play=# EXPLAIN SELECT * FROM blah;
                          QUERY PLAN
---------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37)
(1 row)

eoin_play=# ANALYZE blah;
ANALYZE

eoin_play=# EXPLAIN SELECT * FROM blah;
                          QUERY PLAN
---------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37)
(1 row)

eoin_play=# EXPLAIN (ANALYZE) SELECT * FROM blah;
                                                   QUERY PLAN
-----------------------------------------------------------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=0.042..104.635 rows=1000000 loops=1)
 Planning time: 0.030 ms
 Execution time: 156.670 ms
(3 rows)
```

### Costs

Interpreting the output

    Seq Scan on {table-name}
    (cost={cost-to-get-first-row}..{cost-to-get-all-rows} rows=1000000 width={avg-width-of-a-single-row-in-bytes})
    (actual time = {time to initialize step in ms}..{time to complete whole step} rows={???} loops={???})

- times are reported in XXX..YYY format where
    - XXX = cost (for estimates) or time (for actuals) to return the first row
      the step
    - YYY = cost (for estimates) or time (for actuals) to return the final row
      of the step
- The costs are in postgres "page cost" units
- WARNING: Be careful doing `EXPLAIN (ANALYZE)` on INSERT, UPDATE, DELETE as PG
  will really do the query - make sure to wrap it in a transaction!
- width is the average width of a row in bytes

When PG does a "sequential scan" it

- reads all blocks of the table and for each block:
    - find and check each row in the "block"

Your config file defines `seq_page_cost` which is the cost to read a single
"page". The cost to read the whole table is the `seq_page_cost` times the number
of blocks in the relation

The "total cost" number you see from EXPLAIN is calculated by the planner as
follows

```
Total cost = cost-to-read-all-pages              + cost-to-read-all-tuples
Total cost = (seq_page_cost * pg_class.relpages) + (cpu_tuple_cost * pgclass.reltuples)
```

There are 2 config variables used

1. `seq_page_cost`
2. `cpu_tuple_cost`

### ANALYZE

- updates the stats of the given table (or all tables if none given)
- reads (300 \* `default_statistics_target`) random rows from the database (or
  table if given)
    - `default_statistics_target` defaults to 100 so reads 30k rows by default
- computes stats values of that 30K rows
    - % of NULL values
    - average width of row
    - no. of distinct values
    - most common values and frequencies
- stores its computed stats in the `pg_statistics` catalog
    - The `pg_stats` view is a nicer view of that catalog

The "planner" is the part of PG that decides how to execute the query based on
these stats.

### How do I see the average width of a table in postgres

    ANALYZE blah;
    SELECT sum(avg_width) AS width FROM pg_stats WHERE tablename='blah';

### How do I see an estimate of the no of rows in the table

    ANALYZE blah;
    SELECT reltuples FROM pg_class where relname='blah';

`pg_class` contains stats about the cluster

- reltuples
    - estimated no. of rows
- relpages
    - estimated no. of file pages
    - the estimated no. of "pages" in the "relation"
- relallvisible
    - estimated no. of pages containing only visible rows for all current
      transactions

## BUFFERS option

```
eoin_play=# EXPLAIN (ANALYZE,BUFFERS) SELECT * FROM blah;
                                                   QUERY PLAN
-----------------------------------------------------------------------------------------------------------------
 Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=0.045..101.098 rows=1000000 loops=1)
   Buffers: shared hit=96 read=8238
 Planning time: 0.029 ms
 Execution time: 154.231 ms
(4 rows)
```

Buffers: shared hit={num-blocks-read-from-cache}
read={num-blocks-not-from-cache}

The BUFFERS option shows you how many blocks (8kB) were read from Postgres'
cache vs. had to be read off disk. Postgres' cache is a ring buffer so is shared
between all sessions

# Scan nodes

There are 4 major kinds of "scan nodes" you will see in EXPLAIN output

1. Sequential scan
    - read all the table in sequential order
2. Index scan
    - read the index to filter on the WHERE clause
    - and the table to filter on the "invisible" rows
3. Bitmap index scan
    - same as index scan but read the index fully and then read the table
    - much quicker for larger number of rows
4. Index only scan
    - comes back when all the info needed for the query can be found in an
      index - a so called "covering index"

Other, less important types of scan node:

- function scan
- values scan

In the output of EXPLAIN the nodes to the right are the first to be executed and
the ones on the left are the last to be executed

# making ORDER BY faster

```
eoin_play=# EXPLAIN (ANALYZE,BUFFERS) SELECT * FROM blah ORDER BY c1;
                                                      QUERY PLAN
-----------------------------------------------------------------------------------------------------------------------
 Sort  (cost=145337.34..147837.34 rows=1000000 width=37) (actual time=1519.463..1639.967 rows=1000000 loops=1)
   Sort Key: c1
   Sort Method: external sort  Disk: 45952kB
   Buffers: shared hit=131 read=8206, temp read=5744 written=5744
   ->  Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=0.025..117.407 rows=1000000 loops=1)
         Buffers: shared hit=128 read=8206
 Planning time: 0.074 ms
 Execution time: 1737.203 ms
(8 rows)
```

- Notice we have 2 nodes here a "Sort" and a "Seq Scan".
- The seq scan is executed first so it indented to the right
- The sort
    - couldn't be done in memory so PG did it on disk
    - we see the 'temp' buffers read and wrote 5744 blocks (5744 \* 8kB =
      45952kB)
        - so this sort definitely happened on disk

Now we increase the working memory PG has (1MB by default)

```
GET work_mem;
SET work_mem TO '200MB';

eoin_play=# EXPLAIN (ANALYZE,BUFFERS) SELECT * FROM blah ORDER BY c1;
                                                      QUERY PLAN
-----------------------------------------------------------------------------------------------------------------------
 Sort  (cost=117991.84..120491.84 rows=1000000 width=37) (actual time=229.943..290.324 rows=1000000 loops=1)
   Sort Key: c1
   Sort Method: quicksort  Memory: 102702kB
   Buffers: shared hit=160 read=8174
   ->  Seq Scan on blah  (cost=0.00..18334.00 rows=1000000 width=37) (actual time=0.044..104.811 rows=1000000 loops=1)
         Buffers: shared hit=160 read=8174
 Planning time: 0.048 ms
 Execution time: 349.458 ms
(8 rows)
```

- Now we see that PG did the sort in memory and used approx 100MB for it

But if we add an index to c1 then that index will be sorted too

QUESTION: are indexes always sorted by default?

```
eoin_play=# CREATE INDEX ON blah(c1);
CREATE INDEX
eoin_play=# EXPLAIN (ANALYZE,BUFFERS) SELECT * FROM blah ORDER BY c1;
                                                             QUERY PLAN
-------------------------------------------------------------------------------------------------------------------------------------
 Index Scan using blah_c1_idx on blah  (cost=0.42..34317.43 rows=1000000 width=37) (actual time=0.067..222.746 rows=1000000 loops=1)
   Buffers: shared hit=224 read=10845
 Planning time: 0.178 ms
 Execution time: 278.294 ms
(4 rows)
```

Notice that

- even with heaps of working memory PG prefered to read the index and it was
  faster! (278 ms vs 349 ms)
- we got back a single 'Index Scan' node in the output

# The effect of LIMIT

- If the query has a LIMIT clause then the planner tells the executor that it
  can stop once it has found enough matching rows to fill the LIMIT.
- This means that PG doesn't know in advance how many rows it will need to read
  to satisfy the LIMIT

Remember `blah` has 1,000,000 rows

```
eoin_play=# EXPLAIN (ANALYZE,BUFFERS) select * from blah WHERE c2 LIKE 'ab%' LIMIT 10;
                                                  QUERY PLAN
--------------------------------------------------------------------------------------------------------------
 Limit  (cost=0.00..20.63 rows=10 width=37) (actual time=0.159..0.650 rows=10 loops=1)
   Buffers: shared hit=29
   ->  Seq Scan on blah  (cost=0.00..20834.00 rows=10101 width=37) (actual time=0.158..0.647 rows=10 loops=1)
         Filter: (c2 ~~ 'ab%'::text)
         Rows Removed by Filter: 3418
         Buffers: shared hit=29
 Planning time: 0.070 ms
 Execution time: 0.674 ms
(8 rows)
```

- PG had to read 3428 rows in a sequential scan before it had enough to satisfy
  the limit condition (the output mentions it threw away 3418 + the 10 it kept)

# Explaining the Postgres Query Optimizer - Bruce Momjian

- Video: https://www.youtube.com/watch?v=svqQzYFBPIo
- Slides: http://momjian.us/main/writings/pgsql/optimizer.pdf
    - These slides have good pseudocode descriptions of how the different join
      types work.
- SQL: http://momjian.us/main/writings/pgsql/optimizer.sql

What decisions does the optimizer have to make?

1. Choose a scan method. The choices are:
    1. Sequential scan
        - When you have a very common value it is faster to do a sequential scan
          rather than reading the index and accessing the table randomly to pull
          rows out of it.
        - A sequential scan is faster if the value you are searching for is in
          over 8% (ish) of the rows i.e. if the value in your JOIN or WHERE
          clause appears in more than 8% of rows then Postgres will do a
          sequential scan - index based scans are reserved for quite uncommon
          values (1% - 7%).
    1. Bitmap index scan (also called _Bitmap heap scan_)
        - Used when the optimizer stats indicate that this value is in more than
          8% (ish of the rows)
        - Create a bitmap based on the index where 1 => the value was found in
          the index and 0 => missing
        - Multiple bitmap scans (from multiple indexes) can be ANDed together to
          quickly eliminate rows which don't match all criteria
    1. Index scan
        - Used when the optimizer stats indicate that this value is quite rare
          (rarer than a Bitmap index scan)
1. Choose a join method. The choices are
    1. Nested loop with inner sequential scan
        - unlike other join methods, nested loop requires no setup work so can
          be fast in some cases
        - conceptually like two nexted for loops
    1. Nested loop with inner index scan
    1. Hash join
    1. Merge join
        - typically used for a join with no where clause
1. Choose a join order
    - the order you list tables in a join doesn't matter - the optimizer will
      pick the order it thinks is best based on the available statistics.

Aside: Autovacuum cannot vacuum temporary tables because they are only visible
to the session that created them. Temporary tables don't use the shared buffer
cache (they use a separate _temp buffer cache_ (surprise!)

You can turn off scan methods!

```sql
-- examples
SET enable_seqscan = false;
SET enable_bitmapscan = false;
```

Analyze

- Runs whenever 10% of your table has changed
- Samples N values from the table
- Creates a histogram of the 100 most common values (a 100 bucket histogram) to
  know how "spread out" your data is
- These optimization stats are critical to help the optimizer to make better
  decisions
