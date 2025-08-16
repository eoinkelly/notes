# Tablespaces

- allow the admin to control the disk layout of the postgres installation
- you can specify a tablespace when creating database objects (tables, indexes,
  entire databases)
- uses:
    - if the partition/volume the DB data lives on runs out of space
    - if you want to store some DB objects on fast disks and some on slow
- restrictions
    - they depend on metadata in the main data dir so:
        - they cannot be used with a different Postgres instance
        - you cannot back them up separately and have the data be meaningful
        - putting a tablespace in somewhere volatile (e.g. RAM) could bork the
          whole cluster
    - symlinks are used to implement tablespaces so they only work on systems
      that support symlinks
        - e.g. probably won't work on windows

#### Settings

- `default_tablespace`
    - when this variable is set it is used as an implicit TABLESPACE clause to
      CREATE statements
- `temp_tablespaces` (note the plural)
    - used to decide where to store temp tables and indexes and data created
      when sorting large datasets
    - can be an array of dirs. In this case they are picked at random to be used
      by the system - this lets you spread the load across multiple places

Two tablespaces are automatically created:

1. `pg_default`
    - the default tablespace of `template0` and `template1` databases
        - newly created DBs are patterned off these so `pg_default` becomes the
          default tablespace for all new databases
2. `pg_global`
    - used for shared system "catalogs"

Show tablespace details:

```
psql=# \db+
                                  List of tablespaces
    Name    |  Owner   | Location | Access privileges | Options |  Size  | Description
------------+----------+----------+-------------------+---------+--------+-------------
 pg_default | postgres |          |                   |         | 356 MB |
 pg_global  | postgres |          |                   |         | 581 kB |
(2 rows)
```

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
