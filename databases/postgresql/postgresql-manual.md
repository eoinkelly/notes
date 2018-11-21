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
    * IMPORTANT: SQL does not promise any particular order of the rows within a table!
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
