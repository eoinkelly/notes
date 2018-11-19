# Schemas

Postgres uses the terminology of the SQL standard:

* cluster
    * cluster = postgres installation
    * a collection of unrelated databases all using the same server engine
    * a cluster _is_ a database server
    * > Exactly one cluster is associated with an SQL-session
    * > A cluster is an implementation-defined collection of catalogs.
    * Users and groups are shared across the whole cluster
        * => cannot have same username in multiple databases for different users
* catalog/database
    * catalog is another name for database
    * a "database" in postgres does _not_ contain "tables"
    * A "DB connection" to the cluster (server) goes to one catalog **only**!
* schema
    * a namespace of tables and a security boundary
    * it is the thing that contains tables in postgres

The hierarchy is

```
cluster > catalog > schema > table > rows > columns
```

When you `CREATE DATABASE ...`in Postgres it gets 3 schemas by default

1. public
1. information_schema
1. pg_catalog

After that, all your `CREATE TABLE ...` commands put tables in the `public` schema by default.

Aside: "schema" is the concept that is not visible in MySQL for some reason.

* http://stackoverflow.com/questions/7022755/whats-the-difference-between-a-catalog-and-a-schema-in-a-relational-database

> You may hear the word schema used in a more general sense, meaning the entire
> design of a particular database's table structure. By contrast, in the SQL
> Standard the word means specifically the particular layer in the
> Cluster > Catalog > Schema > Table hierarchy.

* Schemas can contain:
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
CREATE TABLE foo ( ... );
-- is equivalent to ...
CREATE TABLE public.foo ( ... );
```

### Search path

PG maintains a search path `SHOW search_path;` of schema names to search which
you can exploit to not hard-wire schema names into your SQL. By default its
value is `"$user",public` which will search for schemas with the name of the
current user and then `public` in that order

### pg_catalog

Each database(cluster) contains a `pg_catalog` schema which contains

* system tables
* built-in functions
* built-in types

This schema is implicitly at the start of the `search_path`

Out of the box postgres is setup to let you ignore schemas - you are always in
the `public` schema and can pretty much pretend they don't exist.

You can create a schema for each user and restrict their access to just their
own schema.

You can install 3rd party applications by putting them into their own schema
and then giving users access to that schema (which will give them access to the
tables, functions, types from that application)

The main use case for schemas seems to be mostly for shared hosting sort of
stuff where there are some trade-offs between 1 database per tenant or 1 schema
per tenant.

There also seems to be a use-case for using them as "namespaces" when you many
(1000's) of database objects so that you can name things more naturally.

> Today, a somewhat similar situation to mine might arise in a lab environment,
> where several researchers (or hundreds of students) each want to work on
> their own data while having access to a common set in the public schema.
> Using schemas helps stop accidents: inadvertently trampling on each others'
> data.

