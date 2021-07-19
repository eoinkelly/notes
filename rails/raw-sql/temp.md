

```
# start rails server to not use SSL to local postgres so it can be inspected with Wireshark
PGSSLMODE=disable bundle exec rails s
```

From wireshark:

A normal `exec_params()` sends all the results in one go (across many TCP packets) when you execute it


Can you copy data in from a remote server to postgres with psql
    why do I think you can't


COPY TO can also copy the results of a SELECT query.


```sql
copy table_name
to stdout with
    format csv,
    header true,
    force_quote true

copy (
    select * from ...
)
to stdout with
    format csv,
    header true,
    force_quote true
```

