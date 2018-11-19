# Introspecting Postgres

* You can use the information schema and system catalog together to introspect the database
* The `information_schema` contains a standard SQL set of views.
* The `pg_catalog` contains Postgres specific views which allow introspection of the database

When you `CREATE DATABASE ...`in Postgres it gets 3 schemas by default

1. public
1. information_schema
1. pg_catalog

After that, all your `CREATE TABLE ...` commands put tables in the `public` schema by default.

## Information schema

* part of the SQL standard
* implemented by all major RDBMS except Oracle
* Used by commands such as `\d` in psql

```sql
SELECT * FROM information_schema.tables;
```

You can find all the information exposed by the INFORMATION_SCHEMA in the
PostgreSQL system tables (pg_class, pg_index, and so on), but the
INFORMATION_SCHEMA is often much easier to work with. The INFORMATION_SCHEMA
views usually contain human-readable names for things like data type names,
table names, and so on—the PostgreSQL system tables typically contain OIDs that
you have to JOIN to another table in order to come up with a human-readable
name.

## System catalogs

https://www.postgresql.org/docs/current/catalogs-overview.html

> The system catalogs are the place where a relational database management
> system stores schema metadata, such as information about tables and columns,
> and internal bookkeeping information. PostgreSQL's system catalogs are
> regular tables. You can drop and recreate the tables, add columns, insert and
> update values, and severely mess up your system that way.  Normally, one
> should not change the system catalogs by hand, there are normally SQL
> commands to do that.  (For example, CREATE DATABASE inserts a row into the
> pg_database catalog — and actually creates the database on disk.) There are
> some exceptions for particularly esoteric operations, but many of those have
> been made available as SQL commands over time, and so the need for direct
> manipulation of the system catalogs is ever decreasing.

* A few catalogs are physically shared across all databases in a cluster
    * these are noted in the descriptions of the individual catalogs.
* Most system catalogs are copied from the template database during database creation and are thereafter database-specific.
* Contains both tables and views

```sql
SELECT * FROM pg_catalog.pg_trigger;
SELECT * FROM pg_catalog.pg_enum;
```
