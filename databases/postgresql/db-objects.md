# Tablespaces

* allow the admin to control the disk layout of the postgres installation
* you can specify a tablespace when creating database objects (tables, indexes, entire databases)
* uses:
    * if the partition/volume the DB data lives on runs out of space
    * if you want to store some DB objects on fast disks and some on slow

* restrictions
    * they depend on metadata in the main data dir so
        * they cannot be used with a different Postgres instance
        * you cannot back them up separately and have the data be meaningful
        * putting a tablespace in somewhere volatile (e.g. RAM) could bork the whole cluster
    * symlinks are used to implemnet tablespaces so they only work on systems that support symlinks
        * e.g. probably won't work on windows

#### Settings

* `default_tablespace`
    * when this variable is set it is used as an implicit TABLESPACE clause to CREATE statements
* `temp_tablespaces` (note the plural)
    * used to decide where to store temp tables and indexes and data created when sorting large datasets
    * can be an array of dirs. In this case they are picked at random to be
      used by the system - this lets you spread the load across multiple places

Two tablespaces are automatically created:

1. `pg_default`
    * the default tablespace of `template0` and `template1` databases
        * because other created DBs are patterned off these it becomes the default tablespace for all new databases
2. `pg_global`
    * used for shared system "catalogs"

```sql
-- show tablespaces
SELECT * FROM pg_tablespace;

-- create new tablespace
CREATE TABLESPACE eoinspace LOCATION '/tmp/eoinspace';

-- create a table in the new tablespace
CREATE TABLE foo(i int) TABLESPACE eoinspace;

DROP TABLE foo;

-- tablespace must be empty first
DROP TABLESPACE eoinspace;
```

# Aside: Terminology

* cluster
    * cluster = postgres installation
    * a collection of unrelated databases all using the same server engine
    * a cluster _is_ a database server
    * > Exactly one cluster is associated with an SQL-session
    * > A cluster is an implementation-defined collection of catalogs.
* catalog/database
    * catalog is another name for database
    * a "database" in postgres does _not_ contain "tables"
* schema
    * a namespace of tables and a security boundary
    * it is the thing that contains tables in postgres

```
the hierarchy:
cluster > catalog > schema > table > columns + rows
```

So "schema" is the concept that is not visible in MySQL

* http://stackoverflow.com/questions/7022755/whats-the-difference-between-a-catalog-and-a-schema-in-a-relational-database

> You may hear the word schema used in a more general sense, meaning the entire
> design of a particular database's table structure. By contrast, in the SQL
> Standard the word means specifically the particular layer in the
> Cluster > Catalog > Schema > Table hierarchy.


# Schemas

* Users and groups are shared across the whole cluster
    * => cannot have same username in multiple databases for different users

A "connection" to the cluster (server) goes to one database **only**!

Schemas can contain:

* tables
* data types
* functions
* operators

* Schemas are namespaces - the same object can be used in two schemas without
  conflict e.g. you can have a table with the same name
* Schemas cannot be nested
* schema names that begin with `pg_` are reserved for system use
* Every database gets a `public` schema which is used as the default for operations unless you specify a schema name

```sql
-- this ...
CREATE TABLE foo ();
-- is equivalent to ...
CREATE TABLE public.foo ();
```

PG maintains a search path `SHOW search_path;` of schema names to search which
you can exploit to not hard-wire schema names into your SQL. By default its
value is `"$user",public` which will search for schemas with the name of the
current user and then `public` in that order

pg_catalog

Each database(cluster) contains a `pg_catalog` schema which contains

* system tables
* built-in functions
* built-in types

This schema is implicitly at the start of the `search_path`

Out of the box postgres is setup to let you ignore schemas - you are always in
the `public` schema and can pretty much pretend they don't exist.

You can create a schema for each user and restrict their access tojust their
own schema.

You can install 3rd party applications by putting them into their own schema
and then giving users access to that schema (which will give them access to the
tables, functions, types from that application)

The main use case for schemas seems to be mostly for shared hosting sort of
stuff where there are some trade-offs between 1 database per tenant or 1 schema
per tenant.

There also seems to be a usecase for using them as "namespaces" when you many
(1000's) of database objects so that you can name things more naturally.

> Today, a somewhat similar situation to mine might arise in a lab environment,
> where several researchers (or hundreds of students) each want to work on
> their own data while having access to a common set in the public schema.
> Using schemas helps stop accidents: inadvertently trampling on each others'
> data.

# Filesystem storage

## Vacuuming

* PostgresSQL is a transactional database, old rows don't get actually
  removed/replaced when you update/delete them (since they might be still
  needed in older/long running transactions).
* To actually free them you need to issue a vacuum.  A normal vacuum will only
  mark deprecated rows for reuse, to actually reclaim diskspace (e.g. when
  having deleted large amounts of data) you need to issue a full vacuum.
* it might be faster to backup the data you want to keep and truncate the table
  if you plan to remove large portions of a table.

## Toast

TOAST (The Oversized-Attribute Storage Technique).
http://www.postgresql.org/docs/current/interactive/storage-toast.html
