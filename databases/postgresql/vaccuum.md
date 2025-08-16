## Vacuuming

https://www.postgresql.org/docs/current/sql-vacuum.html

- Postgres has an autovacuum process which runs when a certain percentage of
  tuples in a table are dead
- You can vacuum at the database or table level
- PostgresSQL is a transactional database, old rows don't get actually
  removed/replaced when you update/delete them (since they might be still needed
  in older/long running transactions).
- To actually free them you need to issue a vacuum. A normal vacuum will only
  mark deprecated rows for reuse, to actually reclaim diskspace (e.g. when
  having deleted large amounts of data) you need to issue a full vacuum.
- it might be faster to backup the data you want to keep and truncate the table
  if you plan to remove large portions of a table.

```sql
VACUUM;
VACUUM FULL;
VACUUM FULL ANALYZE;
```

VACUUM reclaims storage occupied by dead tuples. In normal PostgreSQL operation,
tuples that are deleted or obsoleted by an update are not physically removed
from their table; they remain present until a VACUUM is done. Therefore it's
necessary to do VACUUM periodically, especially on frequently-updated tables.

Q: does rds do auto vacuum? Question: what does heroku do? QUESTION: do we need
to do it manually with cron on vm hsoting

## vacuumdb command

https://www.postgresql.org/docs/current/app-vacuumdb.html

- a command line wrapper around the SQL VACUUM command
- cleans the DB
- generates statistics for the query optimizer

```bash
# -a all databases
# -F full vacuum
# -v verbose (note: this output is tres verbose, not ideal combined with -a)
# -z generate analyze stats
 vacuumdb -aFz
```
