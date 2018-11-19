# Postgres sequence generators

* [CREATE SEQUENCE docs](http://www.postgresql.org/docs/current/static/sql-createsequence.html)
* sequence number generators are very flexible ways to generate sequences of
  numbers that can be queried like SQL tables.
* You can control
    * where sequence starts
    * where it ends
    * whether it will wrap or not
    * its step size
    * whether to cache some values in memory for speed
* using `OWNED BY table.column` you can associate the sequence to a table
  column so that the sequence will automatically be dropped if the column is.
  Handy.
* They appear as a special single-row table
* they use `bigint` (8 byte integer) for counting

## Caching complications

* e.g. you have a cache of 10 numbers and multiple clients accessing the
  database then each connection will get a cache of 10 numbers so it might end
  up that some numbers are unused.
* if you have cache of 10 and multiple connections then a `setval` won't be
  noticed by other connections until they run out of cached values

Tl:DR probably best to leave cache at 1

## Functions

A sequence number generator has 3 functions

1. nextval('seq_name')
2. currval('seq_name')
3. setval('seq_name')

Sequence generator "tables" have the form:

* sequence_name
* last_value
* start_value
* increment_by
* max_value
* min_value
* cache_value
* log_cnt
* is_cycled
* is_called

List sequences

```
-- in psql
\ds
```

Query a sequence

```sql
SELECT * from my_seq; -- show the single row "table" for this sequence
SELECT nextval('my_seq');
SELECT currval('my_seq');
```

Set sequence value

```sql
SELECT setval('my_seq', 123);
```

Create a sequnce generator

```sql
-- accept all defaults
CREATE SEQUENCE my_seq;

-- becomes

CREATE SEQUENCE my_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807 -- maximum value of bigint
  START 1
  CACHE 1;
```

Create a sequence that will automatically be dropped when a particular table
column is:

```sql
CREATE SEQUENCE my_seq OWNED BY some_table.some_col;
```

Drop a sequence just like a table

```sql
DROP SEQUENCE some_seq;

-- becomes
DROP SEQUENCE some_seq RESTRICT;
```

alternative ways to drop are

```sql
-- VERY DANGEROUS: drops sequence and automatically drops any objects that
-- depend on it. CASCADE is the opposite of RESTRICT
DROP SEQUENCE some_seq CASCADE;

-- don't throw error if sequence does not exist
DROP SEQUENCE IF EXISTS some_seq RESTRICT;
```