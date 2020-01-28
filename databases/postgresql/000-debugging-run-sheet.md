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

## Get estimate of table size

```sql
SELECT reltuples AS approximate_row_count FROM pg_class WHERE relname = 'table_name';
```

A way to estimate how many results a query would return without executing the query

```sql
-- this function is vulnerable to SQL injection so don't use it for production code
CREATE FUNCTION row_estimator(query text) RETURNS bigint
   LANGUAGE plpgsql AS
$$DECLARE
   plan jsonb;
BEGIN
   EXECUTE 'EXPLAIN (FORMAT JSON) ' || query INTO plan;

   RETURN (plan->0->'Plan'->>'Plan Rows')::bigint;
END;$$;
```
