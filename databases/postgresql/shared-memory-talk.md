# Postgres Shared Memory

* PG came from berkeley uni

Video: https://www.youtube.com/watch?v=Nwk-UfjlUn8
Slides: http://momjian.us/main/writings/pgsql/inside_shmem.pdf

PG uses all its data as ordinary files in `/data` on the OS to store everything not raw devices as filesystems are good these days

* Reading from buffers vs reading from disk can be up to 1000x faster!

data/pgx_log
    * write ahead log
    * sync to disk on crash

* `data/base/`
    * where the "data base" is stored
    * one subdir for each database per cluster
    * the `oid2name` tool lets you see how the numbered subdirs match up to databases

* `data/base/{numbered database}/{numbered file}`
* inside each file data is stored in 8kB "pages"
    * all the files in a database dir are multiples of 8kB
    * 8kB is a default page size and can be changed
    * when postgres does somethign to a file it does it in terms of "pages"
* inside each page is a "layout"

    [start]
    [page header]
    [item pointer] * many
    [tuple] * many
    [special area]
    [end]

    * item pointers are filled up from the front
    * tuples (the row data) are filled form the back
    * when the two meet in the middle the page is full

* inside each "tuple" there is a layout too

    [header]
    [value] * many


    * header contains stuff like
        * visiblity masks
        * a bitmask to represent which values in the tuple are null (1 bit for each column in the table)
            * => null values are not "stored" in the field
        * tuple id
        * the transaction ids that create and destroy this tuple
    * so tuples (rows) are stored sequentially in pages
    * nulls seem
    * PG has functions that read the "value" bytes from a tuple and interpret them as a particular type

PG uses a process model NOT a thread model! PG is a child of unix!
They reckon threads don't make a lot of sense for databases

PG starts the `postmaster` process first
    it sits on a socket waiting for a connection
    when a connection comes in that the hba says is from a valid user
    it then fork() a _copy_ of itself off a number of postgres worker processes
each process has
    Program (Text) area
        * the executable code goes here
    Heap (Data) area
    Shared memory area
    Stack area
Interestingly the heap and stack grow towards each other in opposite directions - I presume this makes it less likely for mistakes to scribble on other process memory

After postmaster forks ("clones") itself the new clone will have a new stack and data area _but_ the shared memory are will be in common with the original postmaster

Each fork() of postmaster represents a "session" in postgres
each session has a private stack and data area but can read/write the shared area too
The process model allows for a misbehaving process to be killed in a much cleaner way as only a small amount of shared memory is at risk

One of the main uses of shared memory is the "shared buffer area"

ALL reads and writes (IO) in PG goes through these shared buffers

* When PG has to read an 8kB page it:
    * read it from the shared buffers if possible
    * if its not in the shared buffers copy it from disk into the buffers and then
    read it
* The shared buffer cache is just a shared memory area that contains a bunch of 8kB _pages_.
* If you need a particular tuple from a particular page of a particular table
  (file) then PG will still read the whole page into buffers
* My PG install has 128MB of shared buffers
* how does PG read/write the shared buffer without stomping on other
* it has a few kinds of locks
    * spin locks
        * designed for being locked for short amount of time
        * it uses a "test and set lock"
            * a single byte that contains either 0 or 1
            * if process tries to put a 1 in and gets 0 out it has the lock
            * if process tries to put a 1 in and gets 1 out it does not have the lock
                * if you faile to get the lock you go to sleep
                * every time you fail to get the lock you sleep for slightly longer
            * implemented in assembly for speed and atomicity
    * there are more heavy duty locks for opeations that are expected to take a "long time"

PG team is concerned that using threads for parallelism would make PG less stable

PG uses the CPUs notion of what a 64bit int and a float is directly e.g. timestamps are just 64bit ints formatted in whatever way the CPU likes them.

    Q: Does this imply that a DB created by one CPU could not be moved to another?

* Shared buffers means that if two sessions are writing to the same page at the same time their writes can be synchronised so they don't just overwrite clumisly
    * how does PG do locking?

* the primary processes dirty pages with writes and walks away - the "background writer" is what does the actual writing to disk

# Postgres Optimizer

`libpq` is the library that your client software uses to talk to the PG
JDBC is an alternative to libpq - it lives at the same area

https://momjian.us/main/writings/pgsql/optimizer.pdf

1. Parse the query statement into tokens
2. Go though "traffic cop".
    * If it is a query continue to the rewrite stage
    * If is a utility command e.g. "CREATE TABLE, COPY" redirect to the utility
      commands stage
3. Rewrite query
    * views are handled here
4. Generate paths
5. Generate plan (optimizer)
6. Execute plan
    * runs a state machine that executes the query


## PG Optimizer

Talk: https://www.youtube.com/watch?v=svqQzYFBPIo
Slides: https://momjian.us/main/writings/pgsql/optimizer.pdf

The query string is tokenized and converted into a "query structure". What
choices does the optimizer have?

1. choose the scan method
    * how do we "get" the data from the DB
    * 3 possible options:
        1. sequential scan
        2. bitmap index scan
            * creates a bitmap of the index and somehow ??? uses it to hit the
              index in a better order
            * creates a bitmap and puts 1 in it for each page that might have
              the value we need
                * following index entries causes PG to jump around between
                  pages of the index - this attempt to be more efficient about
                  it
                * PG uses this for values that are not very rare. I think it
                  goes through each page as it finds it and marks in the bitmap
                  if the page needs to be read - then it can used the bitmap to
                  find the pages that have entries - this is quicker iff
                  values are rare but not "very rare"
        3. index scan
2. choose the join method
3. choose the join order

ANALYZE randomly samples your data

The "optimization statistics" created by ANALYZE are _critical_ in helping the optimizer to make good decisions
PG does NOT generate statsitcs based on the query you run

### Why it would sometimes not used index

PG chooses which way to get data from the DB based on how frequent the values you are looking for are

* frequent (greater than 8%): prefer sequential scan
* somewhat frequent: bitmap index scan
* rare: index scan

* When you are choosing a value that is very common in the table it is very
  expensive to read through the index and then the value from each tuple so PG
  uses a sequential scan instead when it thinks it would be faster
* PG will do a sequential scan if the value is 8+% of the rows! i.e. if the
  value you are looking for is there 10% of the time PG will ignore the index
    * row width will tweak this - if the row is very wide it makes sequential
      scans more expensive
* You can turn sequential scan on/off

### Aside: costs
QUESTION: what is a good value for cost - can it be compared across dbs
cost is measured in "page access" - a deliberately fudgy number
things that go into cost:
    page IO
    row access cost
    CPU time
the number itself is not important - only useful comparing with other options
random page cost for SSD should prob be 1.1 (not the default 4)
PG uses the cost to decide which way to do the query so having these set correctly can actually improve database

PG has to balance the time taken to generate a plan vs execution - PG doesn't want to spend more time generating the best plan if the difference in execution would make our "better" plan slower

## Autovacuum system

* will automatically run an ANALYZE on your tables if they change by more than
  10% - it does not run on temporary tables as they are only visible in the
  session that created them
* it preloads the optimization stats
* if you are not getting a good plan from your query you might want to run ANALYZE yourself
* if you have bulk loaded a heap of data into a table you want to run ANALYZE yourself as there will be a delay before it finds it


## Temporary tables

* not stored in shared buffer cache
* stored in `temp_buffers` in memory (8MB by default) - if they fill, they will spill to disk
* not visible to the autovacuum system

## How PG does joins

When PG is joining it has the notion of the "outer side" and the "inner side"

These "sides" can be

* a real on-disk table
* an in-memory representation of a table that is the result of some other SQL
  operation e.g. filtering, joining, grouping.

There are 4 methods available

1. Nested Loop
    * two types
        1. with inner sequential scan
        2. With inner index scan
    * for each row in the "outer side", search every row of the "inner side"
      for matches based on the JOIN clause condition
    * this kind of JOIN can be visualised as two nested for loops
    * the "outer side" and "inner side" can be the result of applying a filter
      or sorting to an on-disk table i.e. if you have a a JOIN with a WHERE
      clause the WHERE is used to filter both sides of the JOIN and then the
      resulting "tables" are joined.
    * -- can be very slow
    * ++ no setup required
    * used for very small tables
    * A CROSS JOIN will always use this join type
3. Hash Join
    * create a hash from the "inner side" (which must fit in main memory
    * as PG is going through the "outer side" looking for matches to the JOIN
      condition it does hash lookups on the hash
    * very commonly used by PG
4. Merge Join
    * procedure
        1. sort inner side
        2. sort outer side
        3 walk down the sored sides in a synchronised way:
            1. take first row of outer side
            2. walk down inner side enough to find matches (both outer an inner
            3. are sorted so this should be quick)
            4. take the next row of outer side
            5. walk down the inner side starting at where we left off the last time
    * ++ very good for large tables
    * -- tables need to be sorted or you have an index
    * an index can be used to elimiate the sort <-- NB
    * the most restrictive relation is _always_ on the outer side of a merge join
        * => it does not matter what order you put things in the JOIN condition

When joining, PG uses the stats is has about values in the tables to make good decisions - these stats include the frequency and distribution of certain values in the table

* There is a global setting to turn off merge-join
* PG does not provide a way to choose which JOIN method it will use - this is deliberate as the JOIN type will depend on the data volume


Because JOINs require lots of access to the columns in the JOIN condition you should probably have an index on both of them - this lets PG avoid sequential scans when choosing JOIN type.

There are still some instances where PG will sequential scan rather than use index and these depend on what its stats tell it.

TODO: figure out how to read and interpret PG stats so I can understand what it is seeing and why it makes particular decisions

If the system knows from its stats that one of the values you have specified is not in the table it iwll do a cursory look anyway for one row just to be sure

LIMIT can effect join usage by PG - PG optimizer is smart enough to take account of the LIMIT clause i.e. PG might use a different join type based on the LIMIT - if LIMIT is high it probably won't do anything different but if its small it might.

TODO: understand bitmap index scan properly

```sql
-- SELECT oid
-- FROM pg_proc
-- ORDER BY 1
-- LIMIT 8;

-- CREATE TEMPORARY TABLE sample1 (id, junk) AS
--  SELECT oid, repeat('x', 250)
--  FROM pg_proc
--  ORDER BY random();  -- add rows in random order
--
-- CREATE TEMPORARY TABLE sample2 (id, junk) AS
--  SELECT oid, repeat('x', 250)
--  FROM pg_class
--  ORDER BY random();  -- add rows in random order

-- these tables have no statistics

EXPLAIN SELECT sample2.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id)
WHERE sample1.id = 33;

-- "Nested Loop  (cost=0.00..289.64 rows=434 width=32)"
-- "  ->  Seq Scan on sample1  (cost=0.00..253.75 rows=62 width=4)"
-- "        Filter: (id = 33::oid)"
-- "  ->  Materialize  (cost=0.00..30.48 rows=7 width=36)"
-- "        ->  Seq Scan on sample2  (cost=0.00..30.45 rows=7 width=36)"
-- "              Filter: (id = 33::oid)"

EXPLAIN SELECT sample1.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id)
WHERE sample2.id > 33;

-- "Merge Join  (cost=1163.01..2532.00 rows=90774 width=32)"
-- "  Merge Cond: (sample2.id = sample1.id)"
-- "  ->  Sort  (cost=104.45..108.14 rows=1476 width=4)"
-- "        Sort Key: sample2.id"
-- "        ->  Seq Scan on sample2  (cost=0.00..26.76 rows=1476 width=4)"
-- "  ->  Sort  (cost=1058.56..1089.31 rows=12300 width=36)"
-- "        Sort Key: sample1.id"
-- "        ->  Seq Scan on sample1  (cost=0.00..223.00 rows=12300 width=36)"

EXPLAIN SELECT sample1.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id);

-- "Merge Join  (cost=1163.01..2532.00 rows=90774 width=32)"
-- "  Merge Cond: (sample2.id = sample1.id)"
-- "  ->  Sort  (cost=104.45..108.14 rows=1476 width=4)"
-- "        Sort Key: sample2.id"
-- "        ->  Seq Scan on sample2  (cost=0.00..26.76 rows=1476 width=4)"
-- "  ->  Sort  (cost=1058.56..1089.31 rows=12300 width=36)"
-- "        Sort Key: sample1.id"
-- "        ->  Seq Scan on sample1  (cost=0.00..223.00 rows=12300 width=36)"

ANALYZE sample1;
ANALYZE sample2;

EXPLAIN SELECT sample2.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id)
WHERE sample1.id = 33;

-- "Nested Loop  (cost=0.00..149.45 rows=1 width=254)"
-- "  ->  Seq Scan on sample1  (cost=0.00..133.56 rows=1 width=4)"
-- "        Filter: (id = 33::oid)"
-- "  ->  Seq Scan on sample2  (cost=0.00..15.88 rows=1 width=258)"
-- "        Filter: (id = 33::oid)"

EXPLAIN SELECT sample1.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id)
WHERE sample2.id > 33;

-- "Hash Join  (cost=19.75..159.77 rows=310 width=254)"
-- "  Hash Cond: (sample1.id = sample2.id)"
-- "  ->  Seq Scan on sample1  (cost=0.00..126.85 rows=2685 width=258)"
-- "  ->  Hash  (cost=15.88..15.88 rows=310 width=4)"
-- "        ->  Seq Scan on sample2  (cost=0.00..15.88 rows=310 width=4)"
-- "              Filter: (id > 33::oid)"

-- NOTE: this has now become a hash join not that PG has stats on the table

EXPLAIN SELECT sample1.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id);

-- "Hash Join  (cost=18.98..158.99 rows=310 width=254)"
-- "  Hash Cond: (sample1.id = sample2.id)"
-- "  ->  Seq Scan on sample1  (cost=0.00..126.85 rows=2685 width=258)"
-- "  ->  Hash  (cost=15.10..15.10 rows=310 width=4)"
-- "        ->  Seq Scan on sample2  (cost=0.00..15.10 rows=310 width=4)"

-- NOTE: this has also become a hash join

-- CREATE INDEX i_sample1 on sample1 (id);
-- CREATE INDEX i_sample444 on sample2 (id);

EXPLAIN SELECT sample2.junk
FROM sample1 JOIN sample2 ON (sample1.id = sample2.id)
WHERE sample1.id = 33;

-- "Nested Loop  (cost=0.43..16.47 rows=1 width=254)"
-- "  ->  Index Only Scan using i_sample1 on sample1  (cost=0.28..8.30 rows=1 width=4)"
-- "        Index Cond: (id = 33::oid)"
-- "  ->  Index Scan using i_sample444 on sample2  (cost=0.15..8.17 rows=1 width=258)"
-- "        Index Cond: (id = 33::oid)"

-- NOTE: this join now uses the available index not a sequential scan
```
