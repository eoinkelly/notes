# Set up RDS for later performance analysis

## Sources

- https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.UsingDashboard.AnalyzeDBLoad.AdditionalMetrics.PostgreSQL.html
- https://postgres.fm/episodes/macro-query-analysis-intro

## Background

> For each SQL call and for each second that a query runs, Performance Insights
> collects SQL statistics. For other Amazon RDS engines, statistics are
> collected at the statement-level and the digest-level. However, for RDS for
> PostgreSQL, Performance Insights collects SQL statistics at the digest–level
> only.

> A SQL digest is a composite of all queries having a given pattern but not
> necessarily having the same literal values. The digest replaces literal values
> with a question mark. For example, SELECT \* FROM emp WHERE lname= ? is an
> example digest.

### pg_stat_statements

- is automatically enabled if you are on pg 11+, must manually load it on 10 or
  earlier
- has very small overhead. Safe to always have enabled.
- Good for macro analysis of workload
- Use pg_stat_statements to find which queries to focus on and then you need
  examples from query logging to dig deeper
    - plans can be quite different depending on the specific data values which
      are normalised away by pg_stat_statements

### pg_stat_activity

- A view in your database
- has one row per server process, showing information related to the current
  activity of that process.
- RDS performance insights can only collect stats for queries in
  `pg_stat_activity` **which are not truncated**
- loaded by default
- `track_activity_query_size` is 1024 bytes by default **which is quite short**
    - it can usually be safely extended to 5-10k without significant overhead

### track_io_timing

https://www.postgresql.org/docs/current/runtime-config-statistics.html#GUC-TRACK-IO-TIMING

- off by default because it is slow on OS where getting current time is slow
- what is the situation on RDS?

### auto explain

- https://www.postgresql.org/docs/current/auto-explain.html
- auto log execution plans of slow queries
- built-in Postgres module
- exists in RDS but you have to add it to the `shared*preload_libraries* param

### slow query logging

- Log all queries longer than a configured timeout
-

?? not that important if you have pg_stat_statements

## Tools

- https://github.com/darold/pgbadger
    - log file analyzer, does not work with pg_stat_statements
    - perl script
    - -- can only analyze queries that appear in the log. Only queries that are
      longer than your chosen threshold will appear in the log. Logging all
      queries is bad for performance so you may miss queries which are very
      frequent but also very fast
