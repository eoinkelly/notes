# Indexes

Sources

- Chapter 9 High perf postgres book

An index is an _organised_ list of values that appear in one or more columns in
a table

They speed up

1. searching for a value in those columns
2. sorting the results based on those columns (indexes already have an order
    - do NOT rely on an index to force ordering of a query - use an explicit
      ORDER BY

## Types of index

There are 4 types

1. Balanced-tree (B-tree)
    - the amount of data on the left and right of each split is kept the same so
      the no. of levels you have to descend for any individual row is
      approximately equal
    - can find a value
    - can compare on
        - equality
            - is/is not equal
            - is/is not NULL
        - range
            - greater than and/or equal
            - less than and/or equal
    - can help with LIKE patterns which search from the start of the value e.g.
      'foo%' not '%foo' but only if locale setup correctly
2. Hash index
    - useful if you are only doing equality searching and don't allow NULL
      values
    - -- can easily become corrupt if DB crashes
    - ++ only slightly faster than B-tree
    - recommendation: do not use
3. Generalized Inverted Index (GIN)
    - used when the data doesn't have the "single key" <--> "single value" form
    - stores a list of keys with a "posting list" of rows (each row contains
      that key)
    - a row can appear in the posting list of multiple keys
    - is that a many-to-many relationship ???
    - you can customise the operator class used for an index
    - uses B-tree strucuture to actually store data on disk
    - useful for
        - indexing array values (can search for sub arrays, a value in an array
          etc.)
        - implementing full text search (there are other ways)
4. Generalized Search Tree Index (GiST)
    - allows you to create a balanced tree structure just by defining how keys
      are to be treated
    - useful when
        - you ned a customized index
        - operators beyond the normal equality and range checks
        - still want a tree structure
    - can answer questions beyond the usual equality and range comparisons
    - examples:
        - geometric data types allow an index to sort on distance between items
          and whether they intersect.
        - can be used for full-text search
    - can be used instead of a table (not just as indexes) depending on what
      data you have
    - GiST indexes are lossy because each document is represented in the index
      by a fixed-length signature
    - is lossy, meaning that the index may produce false matches, and it is
      necessary to check the actual table row to eliminate such false matches.
      (PostgreSQL does this automatically when needed.)

Comparing GIN to GiST

- GIN lookups are ~ 3x faster than GiST
- GIN builds take ~ 3x longer than GiST
- GIN indexes are a bit slower than GiST to update
- GIN indexes are 2x - 3x larger than GiST indexes
- => use GIN for data that doesn't change a lot as it is faster
- => use GiST for data that changes a lot because updates are faster

## Creating indexs without locking

- Creating an index is one of the most intensive things you can do on a database
- building indexes is usually the longest part of a `pg_restore` command
- creating an index locks the table against writes
- you can create indexes concurrently `CREATE INDEX CONCURRENTLY`
    - ++ does not lock the table for writes
    - -- takes much longer to build as it does 2 passes:
        1. scan table once to build index
        2. scan table again to add whatever has changed
    - -- adds risk that duplicate data could be added to a "unique" column while
      it is being built
        - if PG detects this it will mark the index as INVALID
        - INVALID index is still on disk and is still updated when the table is
          (so takes resources)
        - To fix INVALID index:
            1. manually drop INVALID indexes and start again
            2. REINDEX it (does not happen concurrently so will lock for
               writes!)

## Unique indexes

- help with data integrity _and_ performance
- only B-tree indexes can be used as unique indexes!

There are 3 ways to create a unique index

1. mark column as `PRIMARY KEY` in the table create statement (this is the
   preferred form)

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
    This is considered bad form as it effectively creates a constraint but
    doesn't label it as such in the table constraints

TODO: Which does rails do? (Tue 31 Mar 06:13:26 2015)

## Important note about NULL:

`NULL` values are not considered equal to one another! You should prevent any
columns that will have unique indexes on them from having NULL value.

## Clustering an index

Makes a new copy of the table sorted by the index you provide, then drops the
old data \* if table takes X MB on disk you need 2X free to do a cluster Makes
it look like the data was inserted into the table in that order

- Run ANALYZE afterwards to update the stats for the table
- is a one-time act - future INSERT statements do not honor the order
- ++ gets faster results for range based queries

## Fill factor

- When you create a new index not every entry in the index block is used
- `fillfactor` controlled amounts of free space are left to allow for
  changes/insertions into the index can happen on the same index blocks thereby
  reducing fragmentation
- default fill factor for B-tree indexes is 90% i.e. 10% free space
- create an index with 100% fillfactor iff the table data will not change
    ```sql
    CREATE INDEX i ON t(v) WITH (FILLFACTOR=100)
    ```

# Rebuilding indexes

- Indexes can get fragmented over time as inserts and deletes happen to the
  table
- Use the REINDEX command to rebuild the index
- -- cannot happen concurrently

# Locale

- can effect whether an index is used for text searches
- indexes search text using `C` locale by default
- index needs to be created differently to use any other locale for text
  comparison `sql     CREATE INDEX i on t (s text_pattern_ops);     ` TODO: find
  out more abou this (Mon 30 Mar 06:59:02 2015)

```sql
-- show current locale
show lc_collate;
-- "en_NZ.UTF-8"
```

## Multicolumn indexes

- An index can include up to 32 columns
- You can have multiple indexes that include the same column
- e.g. a category and subcategory in your db might have
    - index on category
        - for searchs on category only
    - index on subcategory
        - for searchs on subcategory only
    - index on category, subcategory
        - for searches that involve both
        - not uses for searches that just involve one or the other
- The trade-off with all indexes is that while some searches will get quicker,
  updates will be slower as the index has to be updated too.
- The more indexes you have the greater the update slowdown.

# sorting

- B-tree indexes store their entries in _ascending_ order.
- NULLs are also stored in the index - they are put last in the table
    ```sql
    CREATE INDEX i ON t(v DESC NULLS FIRST);
    ```

    - remember the query planner will only use an index for a table if a small
      no. of rows are being returned
    - its thinking is
        - reading the index blocks is optional
        - the planner considers index reads to be random access not sequential
          accss
        - if enough of the table is being returned it knows that it will have to
          read most of the table blocks anyway so reading the index blocks too
          is unnecessary
            - down side of this is that it has to create a temporary sort table
              which can take a lot of disk activity

# Partial index

- An index doesn't have to cover all values in a column
    ```sql
    CREATE INDEX active_users ON users WHERE active IS true;
    ```
- This index will be used in queries that have a WHERE clause that requires
  checking that active is `true`
- It can be combined with other indexs using a bitmap index scan

# Expression based indexes

Indexes do not need to contain the same values as columns in a table

```sql
-- the followi
CREATE INDEX i_lower ON t (lower(name));

SELECT * FROM t WHERE lower(name) = 'x';
```

Note that the function lower() needs to be called every time you do an INSERT or
UPDATE on this table now so overhead has increased

# Full text search

- GIN and GiST can be used to implement full text search
- GIN is better for static data, GiST better for data that changes a lot

QUESTION: how do I find out which indexes are in a db (Tue 31 Mar 06:16:50 2015)
QUESTION: how do I find out about a particular index in a database (Tue 31 Mar
06:16:37 2015)

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
