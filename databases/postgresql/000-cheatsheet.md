# Postgres & MySQL Cheat sheet

```bash
# mysql: connect as <current_user>@localhost to ??? database
mysql

# mysql: connect as <current_user>@localhost user to database `some_db`
mysql some_db

# mysql: connect as root@localhost user to database `some_db`
mysql -u root some_db

# postgres: connect as <current_user> to `postgres` database
psql

# postgres: connect as <current_user> to `some_db` database
psql some_db

# postgres: connect as `foo` user to database `some_db`
psql -U foo some_db
```

```psql
### show connection info
# postgres
\conninfo
# mysql
\s

### Get help
# postgres
\?
# mysql
\?

### list all databases
# mysql
SHOW DATABASES;
# postgres
\dt
\dt+
SELECT datname FROM pg_database WHERE datistemplate = false;


### quit
# postgres
\q
# mysql
\q
quit
exit

### change database to `some_db`
# postgres
\c some_db
\connect some_db
# mysql
USE some_db;

### show all tables in current database
# postgres
\dt
\dt+
# mysql
SHOW TABLES;

### create database
# postgres
CREATE DATABASE some_db (
    # <col_name> <col_defn>
    id int
);
# mysql
???

### Describe a table
# postgres
\d some_table
\d+ some_table
# mysql
DESCRIBE some_table;

###
# postgres
# mysql
```

# See the name and size on disk of each relation (table)

```
# psql
\connect some_db
\dt+
```

# See total database size on disk

```sql
select pg_size_pretty(pg_database_size('kete_development'));
-- "242 MB"
```

# See the name and size on disk of each index

```
# psql
\connect some_db
\di+
```

# Find the biggest indexes in a DB

???

# Drop a database

```
-- sql
DROP DATABASE foo;

# shell
$ dropdb foo
```
