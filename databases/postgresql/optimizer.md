# Postgres Query Optimizer

Sources

- https://wiki.postgresql.org/wiki/Using_EXPLAIN
    - good starting point of other resrouces about reading EXPLAIN

- https://pgtune.leopard.in.ua/#/
    - website to help you set sensible server config

- https://www.youtube.com/watch?v=XA3SBgcZwtE&list=PLSE8ODhjZXjbeqnfuvp30VrI7VXiFuOXS&index=20
    - video by pg team member talking about what the optimizer is good/bad at

Overall takeaways

- The number of rows in table has a huge impact on which strategy is best to use
  to implement the query
- The content of the rows matters a lot too e.g. `WHERE foo.x = 1` has totally
  different outcome if only a few rows have `x = 1` or if a large number of rows
  have `x = 1  `

Consider

    SELECT *
    FROM a_1, a_2, ..., a_n
    WHERE a_1_id = a_2_id
    WHERE a_3_id = a_3_id
    AND a_(n-1)_id = a_(n)_id

where we are joining n tables. There are least $n!$ ways to join them but there
also "bushy" joins

    a_1 join a_2 = m1
    a_3 join a_4 = m2
    m1 join m2

so the number of possible join orders grows very quickly - faster than $n!$

There are 3 main algorithms that postgres can use to create a join:

1. Nested loop
2. Hash join
3. Merge join

Each of these has multiple variants

In summary there are a lot of possible choices

Postgres planner trieds to do an exhaustive search of all the possibilities

works bottom up

start with an individual table start with the 2 way joins then the 3 way joins
and so on

Consider an ex with 6 tables

a set with 6 elements => there are $2^N$ possible combinations we don't care
about the empty set so there are $2^N - 1$ possibilites we care about for 6
elements, $2^N = 64$, so 63 possible combinations

6 tables (or "one way joins") 15 possible 2-way joins 20 possible 3-way joins 15
possible 4-way joins 6 possible 5-way joins 1 possible 6-way joins

we can also look at it as:

    6 base relations
    57 join relations

All this assumes that the operations are associative and commutative I think =>
inner join only?

Aside: how is inner join communtative when the column order will be diff when
you swap sides?

At a configurable no. of joins PG will switch to the genetic algorithm default
value is 12 but speaker thinks that's kind of low

PG Genetic algorithm: try a bunch of join orders at random and pick the best
one, hope you get lucky

What stats does ANALYZE generate?

- An estimate of the fraction of of rows where the column is null
- An estimate of the average width in bytes of a value stored in this column
- An estimate of the number of distinct values appearing in a column
- Most common values (MCVs): A list of values which appear in the column most
  often and estimated frequency of each
- A histogram estimating the distribution of values other than MCVs (usually 100
  buckets)
- An estimate of physical to logical correlation (which will be high if your
  table is append only but might be low if you lots of edits and deletes)
- If the column is an array then a list of the MCVs in the array along with
  their estimated frequencies
    - they don't currently do this for JSON

All these are **per column**

How do I see them?

Speaker says that row count estimation is very much not a solved problem. The
stats above help but planner still gets it wrong sometimes

Can I manually help postgres if it gets the row count very wrong?

Can I tell PG to "just go nuts on the stats for this table, I don't care if it
takes a long time to ANALYZE or plan

## CREATE STATISTICS

https://www.postgresql.org/docs/current/sql-createstatistics.html

CREATE STATISTICS ON a, b FROM foo lets you express **functional dependencies**
between columns i.e. a and b have some relationship that the planner wouldn't
know about which would cause it make incorrect row count estimates gets stats on
the joint distribution on those columns can help if the planner would guess
wrong about the distribution of a relative to b can help with WHERE clauses eg
`WHERE a = 1 AND b = 2` can't help with joins yet because it can't handle stats
between columns across tables

Apparently you can't give the PG planner hints says in practice when you give a
hint, you end up having to hint the whole query plan rather than just one part
of it

## How postgres implements scans and joins

TODO: enumerate the kinds of index postgres can use

> PostgreSQL provides the index methods B-tree, hash, GiST, SP-GiST, GIN, and
> BRIN. Users can also define their own index methods, but that is fairly
> complicated

you can add columns to an index which are not part of the key this helps if PG
can avoid needing to hit the table at all for the cols it needs

https://www.enterprisedb.com/blog/postgresql-query-optimization-performance-tuning-with-explain-analyze

Scan Types

1. Sequential Scan
    - Basically a brute-force retrieval from disk
    - Scans the whole table
    - Fast for small tables
1. Index Scan
    - Scan all/some rows in an index; look up rows in heap
    - Causes random seek, which can be costly for old-school spindle-based disks
    - Faster than a Sequential Scan when extracting a small number of rows for
      large tables
1. Index Only Scan
    - Scan all/some rows in index
    - No need to lookup rows in the table because the values we want are already
      stored in the index itself
1. Bitmap Heap Scan
    - Scan index, building a bitmap of pages to visit
    - Then, look up only relevant pages in the table for desired rows

Join Types

1. Nested Loops
    - For each row in the outer table, scan for matching rows in the inner table
    - Fast to start, best for small tables
1. Merge Join
    - Zipper-operation on _sorted_ data sets
    - Good for large tables
    - High startup cost if an additional sort is required
1. Hash Join
    - Build hash of inner table values, scan outer table for matches
    - Only usable for equality conditions
    - High startup cost, but fast execution
