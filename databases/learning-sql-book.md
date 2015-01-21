
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

SQL history

1. Cod proposes DSL/Alpha language for manipulating the relational data
1. IBM builds simplified version of DSL/Alpha called SQUARE
1. SQUARE is refined to become SEQUEL
1. SEQUEL is renamed SQL (Aside: SQL is not an acronym for anything)

First SQL stnadard published in 1986

Important: The _result_ of a SQL query is a _table_.

Some tables happen to be stored on disk but I should stop thinking about "tables" as an on disk thing.

```sql
SELECT * FROM foo;
-- foo can be a table on disk, a view, the result of another query
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


SQL is not a full programming language
Database vendors have languages built on top (the `"Programming Language/* SQL` family) to it that make it one e.g.

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


# Aside: Character sets and collations

QUESTION: is the lenght restriction on string types in bytes and if so how are multi-byte chars handled

* default character set is `latin1`
* you can specify character set at the database or column level (table levle???)

```sql
SHOW CHARACTER SET;
-- look at the `maxlen` column to see how many bytes required by each character set
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

Groups a selected set of rows into a set of summary rows by the values of one or more columns or expressions in SQL Server 2014. One row is returned for each group. Aggregate functions in the SELECT clause <select> list provide information about each group instead of individual rows.

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
