# Debugging

Good queries to run to diagnose why a DB is overwhelmed

## Show currently active queries

* Shows currently active queries.
* The query age tells you how long it has been running - the longer the query the slower it is.
* This benefits from being re-run a few times to help you see a pattern

```sql
SELECT pid, age(query_start, clock_timestamp()), usename, query
FROM pg_stat_activity
WHERE query != '<IDLE>' AND query NOT ILIKE '%pg_stat_activity%'
ORDER BY query_start DESC;
```
