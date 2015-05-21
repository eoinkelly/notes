
# See the name  and size on disk of each relation (table)

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
