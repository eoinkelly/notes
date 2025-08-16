# SQL JOIN

- [SQL JOIN](#sql-join)
    - [Sources](#sources)
    - [Overview](#overview)
    - [Illustrating JOIN](#illustrating-join)
    - [Basic JOINs](#basic-joins)
    - [Semi join and Anti join](#semi-join-and-anti-join)
    - [Semi join](#semi-join)
    - [Anti join](#anti-join)
    - [LATERAL JOIN (not really a new kind of join tbh)](#lateral-join-not-really-a-new-kind-of-join-tbh)
    - [JOIN order](#join-order)
    - [Do JOIN always duplicate rows?](#do-join-always-duplicate-rows)
    - [How does Postgres decide which table to put in inner loop in a CROSS JOIN?](#how-does-postgres-decide-which-table-to-put-in-inner-loop-in-a-cross-join)

## Sources

- https://blog.jooq.org/2016/07/05/say-no-to-venn-diagrams-when-explaining-joins/
- https://learnsql.com/blog/how-to-left-join-multiple-tables/

## Overview

- when joining, the whole row (tuple) in the table is treated as an atom i.e.
  each table is a column vector of tuple objects. I think this is an important
  detail for the JOIN mental model
- the cross join is the "mother join" of all other joins because they can be
  derived from it
    - all other joins are a cross join with additional filters and unions

## Illustrating JOIN

Despite how commonly used they are, Venn diagrams are good to illustrate
**actual set operations** but JOIN is not a set operation!

SQL has three **set** operations:

1. UNION
1. INTERSECT
1. EXCEPT

These set operations operate on sets of elements (tuples), which are **all of
the same type**.

- JOIN allows you to combine things of different types e.g. a `films` table
  joined to `actors` table
- A JOIN is really a cartesian product (also called cross product) with a
  filter, and in the case of the OUTER JOINs some `UNION`s

## Basic JOINs

Describing how you get from cross join to other joins:

- CROSS JOIN
    - Steps:
        1. Do a cross join
- INNER JOIN:
    - Aliases:
        - THETA JOIN
    - Steps:
        1. Create a new table by doing a CROSS JOIN
        2. Remove rows from the table which don't match the join condition
    - Consequences
        - is a filtered CROSS JOIN
        - The output **is a subset of the CROSS JOIN table**
        - The rows of either table can be duplicated in the output
- LEFT OUTER JOIN:
    - Steps
        1. Do an INNER JOIN
        2. Find the rows from the LHS table which don't appear in the INNER JOIN
           output
        3. UNION the inner join table with the rows from step 2.
    - Consequences
        - The output is **not a subset of a CROSS JOIN**
        - The output is a superset of an INNER JOIN
- RIGHT OUTER JOIN:
    - Same as LEFT OUTER JOIN but just swap the tables first
- FULL OUTER JOIN:
    - Steps
        1. Do an INNER JOIN
        2. Find the rows from the LHS table which don't appear in the INNER JOIN
           output
        3. Find the rows from the RHS table which don't appear in the INNER JOIN
           output
        4. UNION the inner join table with the rows from step 2. and 3.
    - Consequences
        - The output is **not a subset of a CROSS JOIN**
        - The output is a superset of an INNER JOIN
        - The output is a superset of a LEFT OUTER JOIN
        - The output is a superset of a RIGHT OUTER JOIN

```sql
-- Use a real LEFT OUTER JOIN from the database
SELECT *
FROM owners
LEFT JOIN dogs
    ON dogs.owner_id = owners.id;

-- ************************************
-- Make a LEFT OUTER JOIN from scratch
--
-- Postgres will choose the table with fewer rows (according to it's statistics
-- at least) to be in the "inner" loop so the order of rows output from a LEFT
-- JOIN will change as the data changes!
--
-- ************************************
WITH inner_join_all_fields AS (

    SELECT *
    FROM owners
    JOIN dogs
      ON dogs.owner_id = owners.id

  ), inner_join_just_owners AS (

      SELECT
        owners.*
      FROM owners
      JOIN dogs
        ON dogs.owner_id = owners.id

  ), owner_leftovers AS (

      SELECT owners.*
      FROM owners
      EXCEPT
      SELECT *
      FROM inner_join_just_owners

  ), owner_leftovers_extended_w_nulls AS (

      SELECT
        *,
        NULL::INT8 AS id,
        NULL::STRING AS name,
        NULL::INT8 AS owner_id
      FROM owner_leftovers

  ), left_outer_join AS (

      SELECT *
      FROM owner_leftovers_extended_w_nulls
      UNION
      SELECT *
      FROM inner_join_all_fields

)
SELECT *
FROM left_outer_join;
```

## Semi join and Anti join

- A fancy name for when you put a SELECT query in the WHERE clause of your
  original query
- semi join and anti join are ideas from relational algebra
- Semi and Anti joins only return columns from one table not two
- Use them when you want to filter a table based on data in another table and
  **you don't want any duplicate rows generated**
- there is no SQL syntax for these
- you can implement them in SQL using sub selects with
    - IN()
    - EXISTS()
- The postgres optimizer will recognise semi joins and make a special query plan
- ORMs use this pattern
    - examples ???

> semi join: "give me just columns from table A but only if some data in A
> matches some data in B"

> anti join: "give me just columns from table A but only if some data in A does
> NOT match some data in B"

```sql
-- given two tables: employees, departments

-- implement "semi join" with IN
-- Find all employees whose department name is listed in the departments table
SELECT *
FROM employees
WHERE department_name IN (SELECT name from departments)

-- implement "semi join" with EXISTS
-- Find all employees whose department name is listed in the departments table
SELECT *
FROM employees
WHERE EXISTS ( SELECT 1 FROM departments where employees.department_name = departments.name)
```

- QUESTION: how does those semi join implementations go if columns can be null?

## Semi join

Sources

- https://blog.jooq.org/2015/10/13/semi-join-and-anti-join-should-have-its-own-syntax-in-sql/

Overview

- Sometimes called a "half join"

> What we really mean is we want all actors that played in films. But we don't
> want any films in the results, just the actors. More specifically, we don't
> want each actor several times, once per film. We want each actor only once (or
> zero times) in the result.

```sql

-- implement a semi-join (2 ways)
-- option 1: EXISTS
-- you should maybe choose this syntax because it's inverse (NOT EXISTS) works as expected to make an anti-join
SELECT *
FROM actor a
WHERE EXISTS (
  SELECT * FROM film_actor fa
  WHERE a.actor_id = fa.actor_id
)

-- Option 2: IN

SELECT *
FROM actor
WHERE actor_id IN (
  SELECT actor_id FROM film_actor
)

-- most database query planners will recognise these as semi-joins and behave
-- accordingly
```

## Anti join

> In principle, "ANTI" JOIN is just the opposite of "SEMI" JOIN.

What we really mean is We want all actors that **didn't** play in films. But we
don't want any films in the results, just the actors. More specifically, we
don't want each actor several times, once per film. We want each actor only once
(or zero times) in the result.

```sql

-- Option 1: NOT EXISTS
SELECT *
FROM actor a
WHERE NOT EXISTS (
  SELECT * FROM film_actor fa
  WHERE a.actor_id = fa.actor_id
)

-- Option 2: NOT IN (DANGEROUS - DO NOT USE!)
-- NOT IN can't match null values properly because NULL != NULL - use 'NOT EXISTS' instead!
-- DANGER: SELECT *
-- DANGER: FROM actor
-- DANGER: WHERE actor_id NOT IN (
-- DANGER:   SELECT actor_id FROM film_actor
-- DANGER: )
```

## LATERAL JOIN (not really a new kind of join tbh)

> It is a prefix to the right-hand side of any JOIN operation (including INNER
> JOIN, LEFT OUTER JOIN, etc.) that allows the right-hand side to access columns
> from the left hand side.
>
> This of course has nothing to do with relational algebra anymore, because it
> imposes a JOIN order (from left to right). But sometimes, that's OK and
> sometimes, your table-valued function (or subquery) is so complex, that's the
> only way you can actually use it.

The LATERAL keyword doesn't really change the semantics of the JOIN type that it
is applied to.

TODO: get more use cases for lateral joins

I think they make sense when the rhs of your join is dynamically calculated and
wants to reference the left side of your join

they don't seem to make much sense for joining two tables on disk

The LATERAL key word can precede a sub-SELECT FROM item. This allows the
sub-SELECT to refer to columns of FROM items that appear before it in the FROM
list. (Without LATERAL, each sub-SELECT is evaluated independently and so cannot
cross-reference any other FROM item.)

## JOIN order

- INNER JOIN are commutative and associative so order does not matter
    - Some databases will choose which order it thinks is best
- OUTER JOIN order does matter
    - Joins happen in the order written
    - Start with the table that you want to keep all the rows from

```sql
a LEFT JOIN b LEFT JOIN c
-- becomes
(a LEFT JOIN b) LEFT JOIN c
```

## Do JOIN always duplicate rows?

A join will almost always duplicate rows

In some ways, the whole point of a JOIN is to duplicate rows to build new,
bigger, rows that you can then filter

- All joins do start with a CROSS JOIN
    - => all joins **start** by duplicating **all the rows** i.e. they naturally
      tend to duplicate rows
        - the only time they don't is if there is a very specific filter applied
- INNER JOIN = CROSS JOIN which is then filtered
- LEFT JOIN or RIGHT JOIN = CROSS JOIN which is then filtered, and then has rows
  from one table appended and extended to fit
    - the subtle bit is that the extra rows are appended **after** the filtering
      happens
    - the filtering happens to the cross-join output but the extension adds any
      row which hasn't appeared at least once already
    - you will have all the duplicates that the INNER JOIN gave you, and
      **then** you add one copy of each row which didn't match the filter
- FULL JOIN = CROSS JOIN which is then filtered, and then has rows from **two**
  tables appended and extended to fit

You can use a GROUP BY to collapse the output back to just the rows from the
left table, provided you have some way of grouping the data from the columns of
the rhs table

## How does Postgres decide which table to put in inner loop in a CROSS JOIN?

- The table with fewer rows will always be in the "inner loop".
- If both tables have the same number of rows then it _seems_ (I can't be sure
  without checking PG source) that PG will use lhs table as the outer loop.

```ruby
# pseudo code to implement cross join
def do_cross_join(lhs_table, rhs_table)
    # shorter table is in the inner loop
    output = []
    if len(lhs_table) >= len(rhs_table)
        foreach r_lhs in lhs_table do
            foreach r_rhs in rhs_table do
                # lhs cols are always on the left
                output << r_lhs + r_rhs
            end
        end
    else
        foreach r_rhs in rhs_table do
            foreach r_lhs in lhs_table do
                # lhs cols are always on the left
                output << r_lhs + r_rhs
            end
        end
    end
end
```
