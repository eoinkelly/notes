
Some queries which are good to figure out why your database is overwhelmed


Shows currently active queries. The query age tells you how long it has been running - the longer the query the slower it is.

This benefits from being re-run a few times.

```
SELECT pid, age(query_start, clock_timestamp()), usename, query
FROM pg_stat_activity
WHERE query != '<IDLE>' AND query NOT ILIKE '%pg_stat_activity%'
ORDER BY query_start desc;
```
