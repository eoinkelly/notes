# Learning SQL

## Chapter 1

### Heirarchial database system

* data arranged in a single parent heirarchy
* uses pointers (links) to navigate between entries
* used in XML, Active directory services

### Network database system

* records have links between each other
* can be a multiparent heirarchy
* uses pointers (links) to navigate between entries

### Relational database system

* redundant data is used to link records in different tables
* each row has
    * a "primary key" which is one or more columns that used together can uniquely identifies the row within its table
    * a "natural key" is a primary key made up of columns in the data
    * a "surrogate key" is a primary key containing columns inserted just to have a unique key e.g. an "id" column
    * a "compound key" is a key containing more than one column
    * a "foreign key" is one or more columns which can be used togother to uniquely idenify a single row in another table
        * whose only function as a way of linking this record to a record in another table

Normalization = the _process_ of refining a DB design so that each piece of information only appears _once_.

### SQL history

1. Cod proposes DSL/Alpha language for manipulating the relational data
1. IBM builds simplified version of DSL/Alpha called SQUARE
1. SQUARE is refined to become SEQUEL
1. SEQUEL is renamed SQL (Aside: SQL is not an acronym for anything)

First SQL stnadard published in 1986

Important: The _result_ of a SQL query is a _table_.

Some tables happen to be stored on disk but I should stop thinking about "tables" as an on disk thing.

```sql
SELECT * FROM foo;
# foo can be a table on disk, a view, the result of another query
```

There are 3 broad categories of SQL statements

* SQL schema statements
    * work in the things in the DB itself e.g. schemas, tables
    * the "things" these statements created are stored in a special set of tables called the "data dictionary" (database metadata)
        * these "tables" can be queried to find database metadata
* SQL data statements
    * work on the data in the DB
    * most of the book is about these
* SQL transaction statements
    * used to begin, end, rollback transactions in the DB

SQL is not a full programming language. Database vendors have languages built on top (the `"Programming Language/* SQL` family) to it to provide one e.g.

* Microsoft Transact-SQL
* Oracle PL/SQL
* MySQL stored procedure language
* Postgres PL/pgSQL

These languages are used for

1. can create stored procedures
1. can create functions that can be used like built-in functions
1. provide control structures for the language for more complex computations
1. Normally the result of **every** SQL statement is sent back to the client (even if in a script) - this in inefficient. Using the `PL/*` you can put a bunch of statements in a function/procedure that is stored in the DB and will not do this.

Things like the `pg` gem act as replacements for the client but by default will still get the results of each query back so there is still a value to using the `PL/*` languages.


How to think about building a query:

1. Decide which tables will be needed and add them to the FROM clause
    * The JOIN type tells the DB how it should combine the tables into one large table
1. Add conditions to the WHERE clause to filter out which _rows_ will be retrieved
1. add columns to the SELECT clause to filter which _columns_ will be retrieved

Note that SELECT, FROM, WHERE are _clauses_ in the overall "select statement" (terminated by a ;)

Aside: first 4 chaps are designed to be read in order, others to be read in any order

# Chapter 2: MySQL basics

The basic data types are

* character types
    * char (fixed length, max 255 bytes)
    * varchar (variable lenght, max 64kb)
    * text
    * mediumtext
    * longtext
* date types
* numeric types

Be careful in choosing character types as MySQL will truncate data to fit - it will warn you when it does that.

# Aside: Character sets and collations

QUESTION: is the lenght restriction on string types in bytes and if so how are multi-byte chars handled

* default character set is `latin1`
* you can specify character set at the database or column level (table levle???)

```sql
SHOW CHARACTER SET;
-- look at the `maxlen` column to see how many bytes required by each character set
```

END CHAP 2












# Chapter 5: Querying multiple tables

SELECT {select clause} FROM {from clause}

Joins are part of the {from clause}

t1 INNER JOIN t2 ON t1.id = t2.other_id

Types of JOINs

* JOIN
    * synonym for INNER JOIN
* CROSS JOIN
    * You get this if you do any JOIN without a join condition but it is better to explicitly use CROSS JOIN to let reader know it was deliberate.
    * returns every possible different row that can be created by combining two tables
    * this is what you get if you don't use a "join condition" (i.e. ON|USING)
    * The basic cross join with no constraints will take tables of (rows x columns) `NxM`, `AxB`
    and produce a single table of dimensions `(N * A) x (M + B)`
* INNER JOIN
    * Important: does not return rows for which the boolean test in the "join condition" fails
        * so it is like applying a filter to the carthesian product (CROSS JOIN)
        * "do a CROSS JOIN and filter the results based on a boolean test (the join condition)"
        * "do a CROSS JOIN then filter based on some criteria"
        * => it filters more aggressively than OUTER JOIN does
* LEFT OUTER JOIN
* RIGHT OUTER JOIN
* LEFT JOIN
* RIGHT JOIN

* NATURAL {any other join type}
    * Natural prefix allows you to avoid writing the join condition
    * It infers waht columns should be joined by looking for identically named
      columns in both tables
    * Generally speaking it is a bad idea

* STRAIGHT_JOIN
    * A MySQL only extension that allows you to control JOIN order

* If you don't specify an ON clause you get a CROSS JOIN - the DB gives you the
  cartesian product of the two tables.  Tables of (rows x columns) `NxM`, `AxB`
  and produce a single table of dimensions `(N * A) x (M + B)`

### Legacy syntax

There is a pre-ANSI SQL syntax for joins that puts the conditions in WHERE clause but it is not recommended because

1. it is unclear and has no advantages.
2. it is hard to pick out which parts of WHERE are part of the join and which are just filters
    * I guess they are almost the smae thing anyway???

### Join condition

* basically a boolean test that is run against each row created by the implicit CROSS JOIN
* if the column name is the same on both sides of the join you can use `USING(col_name)` as a shorthand for `left.col_name = right.col_name`
    * => USING is just a shorthand syntax

### Joining multiple tables

The DB will join N tables sequentially by picking one to start with then
joining the next onto it, then joining the next onto the result of that and so
on.

Each new table will be joined onto the current result using

1. the join type specified
2. the join condition specified to filter the rows

You could implement joins using just CROSS JOIN and lots of where clauses but the syntax would be a mess.

The order of joins does not matter i.e. `JOIN` is a binary operator that is commutative

ALl these give the same result:

    A join B join C
    A join C join B
    B join A join C
    etc.
    etc.

* SQL is **not** procedural - you do not specify _how_ the join happens, only what should happen.
* The DB picks one of the tables as a _driving table_ and starts with it.
* While the results of the JOIN do not depend on the order, there may be
  performance improvements in telling it which table to use e.g.
    * examples:
        * STRAIGHT_JOIN in MySQL
        * ORDERED/LEADING in Oracle
        * FORCER ORDER in SQL Server
        * [Postgres explicit joins](http://www.postgresql.org/docs/current/interactive/explicit-joins.html)
            * from_collapse_limit
            * join_collapse_limit
    * These are hints to the DB engine's "query planner"
    * you are mostly trading off between "planning time" and "run time" to find the optimal time for a particular query

### Aliases are necessary

```sql
... FROM table_a AS aa ...
```

Table aliases are not just a nice to have - you need them in joins sometimes
you will want to join the same table twice as part of the join e.g. a bank
account could have the bank_branch that it was opened at and the bank_branch
that its last transaction was at.

### Self joins

Self-joins make sense if you have a column in your table that refernces some
other column in the table e.g. an employee table might have a a "manager_id"
column that references its own "id" column.

Just included the same table twice using different aliases.

QUESTION: what do self joins achieve that a WHERE cannot?

Self-joins allow you to have the same input column appear as more than one output column.
WHERE can only reduce rows, it cannot duplicate (or increase) the no. of columns.

### Equi-join

If the join condition requires one side to match the other exactly it is an equi join

    ON a.id = b.id

but a join condition just has to return a boolean so there are other possible kinds:

    ON a.id <= {user_supplied_int} AND b.id >= {user_supplied_int}

You can join tables that have not foreign-key relationship.

## Exercises

```sql
# Exercise 5.1
SELECT e.emp_id, e.fname, e.lname, b.name
FROM employee e INNER JOIN branch b
ON e.assigned_branch_id = b.branch_id;


# Exercise 5.2
SELECT a.account_id, c.fed_id, p.name
FROM customer c
    INNER JOIN account a ON a.cust_id = c.cust_id
    INNER JOIN product p ON a.product_cd = p.product_cd
WHERE cust_type_cd = 'I';

# Exercise 5.3

# ON is for combining two tables, WHERE is for filtering down
SELECT e.emp_id, e.fname, e.lname
FROM employee e INNER JOIN employee sup
# the relationship between superior_emp_id an emp_id is how these two tables are related so we use that in the JOIN clause
ON e.superior_emp_id = sup.emp_id
# the department ids not matching is not part of that relationship so this is better expressed as a WHERE clause
WHERE e.dept_id != sup.dept_id;

```

# Chapter 8: Aggregate functions

Examples:

* sum()
* count()
* avg()
* min()
* max()
* count()

* take a _group_ of rows and "aggregate" them into a single row
* If used in a statement with no `GROUP BY` then an implicit group is the whole result set
* the GROUP BY clause decides what rows should be fed into an aggregate function

```sql
-- returns 1+ rows
SELECT * FROM blah;

-- always returns 1 row
SELECT count(*) FROM blah;
```

The are functions from Column -> Cell

They take a single column and turn it into a single cell

If you use a group function in a statement containing no GROUP BY clause, it is equivalent to grouping on all rows.

# GROUP BY

... GROUP BY blah ...

"taking a table as input show only unique values of "blah" and collapse other selected columns based ither on an aggregate function or just pick the first value you find"

The GROUP BY clause will gather all of the rows together that contain data in the specified column(s) and will allow aggregate functions to be performed on the one or more columns.

The GROUP BY statement is used in conjunction with the aggregate functions to group the result-set by one or more columns.

Groups a selected set of rows into a set of summary rows by the values of one or more columns or expressions in SQL Server 2014. One row is returned for each group. Aggregate functions in the SELECT clause `<select>` list provide information about each group instead of individual rows.

* it is a kind of "scoping" for the aggregate function which would otherwise just return a single value
so it is a "row collapser"
it is a way of taking a table and turning it into a table with same or less rows


if you use it without aggregate functions you get whatever representative row the DB wants to give you
    => group by is really just a helper for aggregate functions
    => without a group by the aggregate functions will take every row in the input table as their input

GROUP BY foo will collapse all foo values into one row for each distinct value
if you have selected anything else other than `foo` you need to use an aggregate function on it so that the DB knows how to collapse it down. Without an aggregate function it will simply pick a row to show (which is not meaningful)

Each column you select *will* be converted into a single representative value.
If you don't tell the server _how_ to do this then it is free to choose any
value from each group, so unless they are the same, the values chosen are
indeterminate.

How do multiple columns in a GROUP BY work???


# `count(*)` vs `count(some_col)`

* count is an aggregate function so it always takes a group as input
* count takes an _expression_ as parameter
* count(DISTINCT some_col) will count the no. of unique values of some_col in the group

```
count(*) counts rows (it will include NULL values)
count(some_col) counts values in some_col (it will ignore NULL values)
```

CHAP 8 NOT COMPLETE


# Chapter 10: Joins revisited

* An outer join includes _all_ the rows from one table and includes data from the second table only if matching rows are found.
* A join includes _all_ the rows from one table and adds data to reach row from the second table only if the join condition is satisified.
* OUTER JOIN reads as "give me all the rows from table t1 and decorate them with some data from t2 if the join condition is true"
* INNER JOIN is "give me _only_ the rows from t1 and t2 where the join condition is true"

LEFT OUTER JOIN

* => the table on the left of the JOIN operator is what we want all rows from, the rhs is just contributing values
* => the result will have same number of rows as left side table
* => "I want all rows from left side table even if there are gaps in the data"

RIGHT OUTER JOIN

* => the opposite of above
* => the result will have same #rows as rhs table

```sql
-- these are equivalent
A RIGHT OUTER JOIN B
B LEFT OUTER JOIN A
```

### Natural joins

Natural join are a dubious sugar syntax

```sql
-- this ...
A JOIN B ON B.x = B.x
-- can be written in shorthand using ...
A NATURAL JOIN B
```

* In a natural join the DB infers the join condition by inspecting the columns
  in the two tables involved in the JOIN.
* For a natural join to work there must be identically named columsn in both tables
* Author recommends avoiding this join type as it is confusing

FULL OUTER JOIN

* A combination of both LEFT and RIGHT
* MySQL does not have it. [You can emulate it](http://stackoverflow.com/questions/2384298/why-does-mysql-report-a-syntax-error-on-full-outer-join) but it is clunky.

# Creating new fake tables

Uses SELECT statemetns and UNION ALL to make a "fake" table

TODO: find out about this
