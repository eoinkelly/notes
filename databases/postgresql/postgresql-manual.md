# PostgreSQL

## Basics

* PostgreSQL is a relational database management system (RDBMS).
    * That means it is a system for managing data stored in relations.
    * Relation is essentially a mathematical term for table.
* Table
    * is a collection of tuples of the same type
* Rows
    * Rows are tuples.
    * Each row is a tuple of a partiular fixed composite type e.g. `(integer, integer, varchar, date)`
    * SQL does not promise any particular order of the rows within a table!
* Columns
    * Columns within a row have a fixed order (as you would expect from a tuple)
    * Each column within a row has a fixed data type
    * type names are not keywords (postgres has many types and can be extended with others)
* based on POSTGRES 4.2 (note the all caps)
    * POSTGRES started in 1986
* replaced PostQUEL with SQL in 1995
* it has a "master worker" architecture
    * the server forks a new process for each new connection. Once the server has created the worker process and done the plumbing for that worker to get the TCP connection then the server is no longer involved.
* Many Postgres features are aimed at a multi-user account system where individual users have their own database
    * If you don't specify a username it will default to your OS username
    * If you don't supply a db name to `createdb` it defaults to creating a DB with the same name as the user you logged into postgres as i.e. if you logged into postgres as your OS username (the default behaviour) then it will create a DB with your OS username
* it stores data in relations (a table)
    * a _table_ is a _named collection of rows_
* PG does not use other data storage options:
    * heirarchical (e.g. dirs and files on a unix system)
    * OO e.g. Mongo

## Terminology

* database: a group of tables
* cluster: the collection of databases managed by the server

## Shortcut tools

Passing `-e` to these tools makes them echo the SQL they run for you

* `createdb`
    * a convenience wrapper around `CREATE DATABASE` SQL statement
* `dropdb`
    * a convenience wrapper around `DROP DATABASE` SQL statement

## Tablespaces

https://www.postgresql.org/docs/10/manage-ag-tablespaces.html

* `\db` to list them in psql
* two are created by default `pg_global` and `pg_default`
* use cases
    * most storage is on cheap spinning disk but you want to put some heavily trafficed indexes on a fast SSD
    * you run out of space on your drive and need to create new tables on a separate drive
* the tablespace dir is part of the cluster =>
    * you should not back it up or copy it on its own - you can't attach it to a different cluster
    * if a tablespace gets corrupt then the whole cluster could go down

> Tables, indexes, and entire databases can be assigned to particular tablespaces.

> ... allow database administrators to define locations in the file system
> where the files representing database objects can be stored. Once created, a
> tablespace can be referred to by name when creating database objects.

```sql
-- create a tablespace
CREATE TABLESPACE fastspace LOCATION '/ssd1/postgresql/data';

-- tablespace can be specified explicitly when creating tables, indexes, databases
CREATE TABLE foo(i int) TABLESPACE fastspace;

-- or you can set the default tablespace
SET default_tablespace = fastspace;
CREATE TABLE foo(i int);
```

## Quoting

* double quotes for table and column names
* single quotes for values
    * most complex values (except numbers) require quoting
    * e.g. points `INSERT INTO weather (name, location) VALUES ('Dublinish', '(124.45, 34.55)';`

## The "maintenance DB"

* PG requires you to connect to a database do do things
* Defaults first the `postgres` database, then `template1` if `postgres` does not exist

## Template databases

* TODO

* if you install a new programming language extension into `template1` then it will be enabled on all new databases created

## Choices to make when creating a database

1. Encoding
1. Locale
1. Collation
1. Owner
1. Whether to base this database from a named template database


## Environment variables

```bash
# env vars showing default values
PGHOST=localhost
PGUSER=postgres
PGPORT=5432
PGDATABASE=postgres
```

    TODO: how does the LC_* vars interact with it?

## Help

Postgres SQL is documented as man pages as well e.g.

```
$ man CREATE_DATABASE
$ man DROP_DATABASE
```

## psql

* prompt is `=>` for normal user or `=#` if you are superuser
* postgres lets you do most/all of its management via a SQL like interface
    * it presents its internal server state as tables and you can operate on them
    * it provides functions you can invoke via a`SELECT` clause with no `FROM` clause - these functions to manipulate the cluster

Command line args

* `-s` enabled "single step mode where you are prompted between each command - very useful for debugging scripts

Internal psql commands begin with '\'

```
\q              -- quit
\i foo.sql      -- read in commands from foo.sql (pair it with -s
\h              -- show SQL help
\?              -- show \ commands help
\db             -- list tablespaces
```

Useful functions

All these are invoked via `SELECT funcname();`

```
current_database();
```

What are these?
```
current_date
```

UP TO END CHAP 1

## SQL

### SELECT clause

* https://www.postgresql.org/docs/current/sql-select.html

```elixir
# Elixir pseudocode showing the order of evaluation of a SELECT statement in Postgres
with temp_table_1 -> with_expression_1(),
     temp_table_2 <- with_expression_2() do
    from()
    |> where()
    |> group_by()
    |> having()
    |> select()
    |> select_distinct()
    |> order_by()
    |> union_intersect_except()
    |> limit()
    |> locking()
end
```

* the words between `SELECT` and `FROM` are called the **SELECT list** : `SELECT {SELECT list} FROM ...` - those are the words we are actually interested in here.

The **SELECT list**:

* a list of expressions which form the **output** rows of the query
* the expressions usually refer to columns from the FROM clause

```sql
-- In PG you can omit the FROM clause if you aren't referencing values from a table/view
SELECT version();
SELECT current_date;
SELECT 2 + 3;

-- this works provided there are more than 0 rows in the table
SELECT 2 + 2, version(), current_date FROM ar_internal_metadata;
--  ?column? |    version                              | current_date
-- ----------+-----------------------------------------+--------------
--         4 | PostgreSQL 10.1 on x86_64-pc-linux-gnu, | 2018-11-12
```

QUESTION: what is 'current_date'? it's not a function

There are two phases to computing the output columns

1. the values of the output columns are computed
2. DISTINCT is evaluated on the result of step 1. i.e. duplicate rows are removed from the output columns

There are 3 variants of SELECT

* `SELECT ALL ...`
    * the default - you don't have to specify the `ALL` keyword (and we usually don't)
    * returns all rows from the result set
* `SELECT DISTINCT ...`
    * all duplicate rows are removed from the result set (one row is kept for each set of duplicates)
* `SELECT DISTINCT ON (expression [, ...])`
    * keeps only the first row for each _set of rows_ where the given expressions evaluate as equal
    * IMPORTANT: the "first row" is unpredictable unless you also specify an `ORDER BY`
    * The `DISTINCT ON` expressions **must match** the left-most `ORDER BY` expressions (the `ORDER BY` can contain extra expressions to control ordering within each `DISTINCT ON` group)
    * steps
        1. create the in-memory result set (based on the FROM and WHERE clauses etc.)
        1. calculate the value of the expressions for each row
        1. keep only the first row for each unique value of the expressions set

```sql
-- the combination of col1 and col2 is used to evaluate the duplicate
SELECT DISTINCT col1, col2 FROM t1;

-- keeps the first row from each group of duplicates
-- results will be unstable unless you add an ORDER BY clause

-- 1. Build a table in memory with all rows from t1 (there is no WHERE clause to remove rows)
-- 2. Sort the in-memory table by col1 and then col2
-- 3. Return only one row (the first one in the sorted table) for each unique value of col1
SELECT DISTINCT ON (col1) col1, col2 FROM t1 ORDER BY col1, col2;
```

### FROM clause

> CROSS JOIN and INNER JOIN produce a simple Cartesian product, the same result as you get from listing the two tables at the top level of FROM, but restricted by the join condition (if any). CROSS JOIN is equivalent to INNER JOIN ON (TRUE), that is, no rows are removed by qualification. These join types are just a notational convenience, since they do nothing you couldn't do with plain FROM and WHERE.<Paste>

There are 5 kinds of join. Each kind of join can have multiple syntaxes which trigger it

1. CROSS JOIN (carthesian product)
	* the following are all the same
		* `FROM a, b`
		* `FROM a, b WHERE true`
		* `FROM a CROSS JOIN b`
		* `FROM a INNER JOIN b ON (true)` (a cross join can be expressed as an inner join)
2. INNER JOIN
	* the INNER keyword can be omited
	* `a INNER JOIN ON join_condition`
	* **inner join is just syntax sugar for a FROM and a WHERE** i.e. the following are all the same
		* `FROM a, b WHERE a.id = b.id` (FROM + WHERE)
		* `FROM a INNER JOIN b ON a.id = b.id` (INNER JOIN)
		* `FROM a JOIN b ON a.id = b.id` (CROSS JOIN)
3. LEFT OUTER JOIN
    * must have an ON/USING condition
4. RIGHT OUTER JOIN
    * must have an ON/USING condition
	* is just syntax sugar - you can achieve it with LEFT OUTER JOIN and switching position of the tables
5. FULL OUTER JOIN
    * must have an ON/USING condition

A `join_condition` is similar to WHERE in that it returns a boolean

> One thing that might not always be obvious to some is that a cross join with an empty table (or result set) results in empty table (M x N; hence M x 0 = 0)
>
> A full outer join will always have rows unless both M and N are 0.

### WHERE clause

The WHERE clause contains a Boolean (truth value) expression, and only rows for which the Boolean expression is true are returned

* WHERE filters rows **before** GROUP BY gets them
* HAVING filters rows **after** GROUP BY i.e. it filters the "group rows" created by GROUP BY

### GROUP BY

> When GROUP BY is present, or any aggregate functions are present, it is not
> valid for the SELECT list expressions to refer to ungrouped columns except
> within aggregate functions or when the ungrouped column is functionally
> dependent on the grouped columns, since there woul otherwise be more than
> one possible value to return for an ungrouped column.

### HAVING

* HAVING filters rows **after** GROUP BY i.e. it filters the "group rows" created by GROUP BY
* The presence of HAVING turns a query into a grouped query even if there is no GROUP BY clause.

### ORDER BY

* is evaluated **after** the SELECT list!
* sorts **result** rows (not input rows)
   * column names are specified by either the **output** column name or its position in the output table (note: all output, not input)

> If an ORDER BY expression is a simple name that matches both an output column name and an input column name, ORDER BY will interpret it as the output column name. This is the opposite of the choice that GROUP BY will make in the same situation. This inconsistency is made to be compatible with the SQL standard.


## COPY

https://www.postgresql.org/docs/current/sql-copy.html

```sql
COPY db_name FROM '/home/user/weather.txt';
```
