# EXPLAIN

Options:






```sql
-- Setup database
CREATE TABLE blah (c1 INTEGER, c2 TEXT);
INSERT INTO blah SELECT i, md5(random()::text) FROM generate_series(1, 1000000) AS i;


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

Interpreting the output

    Seq Scan on {table-name}
    (cost={cost-to-get-first-row}..{cost-to-get-all-rows} rows=1000000 width={avg-width-of-a-single-row-in-bytes})
    (actual time = {time to initialize step in ms}..{time to complete whole step} rows={???} loops={???})

* times are reported in XXX..YYY format where
XXX = time/cost to initialize the step
YYY = total time/cost for the step

* The costs are in postgres "page cost" units
* WARNING: Be careful doing `EXPLAIN (ANALYZE)` on INSERT, UPDATE, DELETE as PG will really do the query - make sure to wrap it in a transaction
* the sequential page cost is 1.0 by default
    * ???
* width is the average width of a row in bytes

That is the output of EXPLAIN but how is it calculated? PG begins with `ANALYZE`

Significant tables

1. `pg_class` "system catalog"
2. `pg_statistics`

### ANALYZE

* updates the stats of the given table (or all tables if none given)
* reads (300 * `default_statistics_target`) random rows from the database (or
  table if given)
    * `default_statistics_target` defaults to 100 so reads 30k rows by default
* computes stats values of that 30K rows
    * % of NULL values
    * average width of row
    * no. of distinct values
    * most common values and frequencies
* stores its computed stats in the `pg_statistics` catalog
    * The `pg_stats` view is a nicer view of that catalog

TODO: how to view those stats well

The "planner" is the part of PG that decides how to execute the query based on
these stats.

### How do I see the average width of a table in postgres

    ANALYZE blah;
    SELECT sum(avg_width) AS width FROM pg_stats WHERE tablename='blah';

### How do I see an estimate of the no of rows in the table

    ANALYZE blah;
    SELECT reltuples FROM pg_class where relname='blah';

`pg_class` contains stats about the cluster

Aside: terminilogy

relation == table
page == block ??
a "block" seems to be 8 kB on my machine
block contains rows
catalog == table ??

* reltuples
    * estimated no. of rows
* relpages
    * estimated no. of file pages
    * the estimated no. of "pages" in the "relation"
* relallvisible
    * estimated no. of pages containing only visible rows for all current
      transactions

Cost

When PG does a "sequential scan" it

* reads all blocks of the table and for each block:
    * find and check each row in the "block"

Your config file defines `seq_page_cost` which is the cost to read a single
"page". The cost to read the whole table is the `seq_page_cost` times the
number of blocks in the relation

The "total cost" number you see from EXPLAIN is calculated by the planner as
follows

```
Total cost = cost-to-read-all-pages              + cost-to-read-all-tuples
Total cost = (seq_page_cost * pg_class.relpages) + (cpu_tuple_cost * pgclass.reltuples)
```

There are 2 config variables used

1. `seq_page_cost`
2. `cpu_tuple_cost`

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

   Buffers: shared hit={num-blocks-read-from-cache} read={num-blocks-not-from-cache}

The BUFFERS option shows you how many blocks?? were read from Postgres' cache vs. had to be read off disk
* Postgres' cache
* is a ring buffer so is shared between all sessions


# Scan nodes

There are 4 major kinds of "scan nodes" you will see in EXPLAIN output

1. Sequential scan
    * read all the table in sequential order
2. Index scan
    * read the index to filter on the WHERE clause
    * and the table to filter on the "invisible" rows
3. Bitmap index scan
    * same as index scan but read the index fully and then read the table
    * much quicker for larger number of rows
4. Index only scan
    * comes back when all the info needed for the query can be found in an
      index - a so called "covering index"

Other, less important types of scan node:
* function scan
* values scan

In the output of EXPLAIN the nodes to the right are the first to be executed and the ones on the left are the last to be executed



# making ORDER BY faster (sorting faster)

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

* Notice we have 2 nodes here a "Sort" and a "Seq Scan".
* The seq scan is executed first so it indented to the right
* The sort
    * couldn't be done in memory so PG did it on disk
    * we see the 'temp' buffers read and wrote 5744 blocks (5744 * 8kB = 45952kB)
        * so this sort definitlely happened on disk


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

* Now we see that PG did the sort in memory and used approx 100MB for it

But if we add an index to c1 it will be sorted too

Aside: are indexes always sorted by default?

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

* even with heaps of working memory PG prefered to read the index and it was faster! (278 ms vs 349 ms)
* we got back a single 'Index Scan' node in the output


# The effect of LIMIT

* If the query has a LIMIT clause then the planner tells the executor that it
  can stop once it has found enough matching rows to fill the LIMIT.
* This means that PG doesn't know in advance how many rows it will need to read
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

* PG had to read 3428 rows in a sequential scan before it had enough to satisfy
  the limit condition (the output mentions it threw away 3418 + the 10 it kept)
