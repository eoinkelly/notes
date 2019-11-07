# Postgres Functions

```sql
cast()
coalesce()
```

## count

* returns `bigint`

* has 2 variants:

1. if passed `*` literal then it returns the count of input rows
    * `*` is a hard-coded special case by the looks of it
2. if passed anything else, the argument is treated as a Postgres expression and evaluated, it returns the number of input rows **for which the value of the expression is not null**




