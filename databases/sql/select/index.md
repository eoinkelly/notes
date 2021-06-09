# SELECT clause

* https://www.postgresql.org/docs/current/sql-select.html
* https://blog.jooq.org/2016/12/09/a-beginners-guide-to-the-true-order-of-sql-operations/ (good blog about ordering)

Key idea: the input and output of each step is **not** just a table!

```elixir
# Elixir pseudocode showing the order of evaluation of a SELECT statement in Postgres
with temp_table_1 -> with_expression_1(),
     temp_table_2 <- with_expression_2() do
    from()
    |> where()
    |> group_by()   # inclusion of this clause  will trigger "grouped query", type :: [row] -> Hash<grouped_by_expression_value, [row]>
    |> apply_aggregrations() # operates on the "Hash" created by group_by() type :: Hash<grouped_col, [row]> -> [row], maybe window functions applied ere too?
    |> having()     # inclusion of this clause  will trigger "grouped query"
    |> window_functions() #
    |> select()     # inclusion of aggregate functions in here will trigger "grouped query"
    |> select_distinct()
    |> union_intersect_except()
    |> order_by() # this is the top level ORDER BY clause of the SELECT (there can be orderings within expressions too)
    |> limit_fetch_top()
    |> locking()
end
```
### FROM clause

* Evaluation of the select begins in the FROM clause.
* The FROM clause builds a large in-memory table and the later parts of the SELECT statement transform that table by
    * removing rows (via WHERE, GROUP BY, HAVING)
    * Adding rows (via UNION)
    * removing columns (via SELECT list)
    * adding columns (via SELECT list)
    * re-ordering columns (via ORDER BY)
* The FROM clause builds a table by combining one or more input tables (whether those tables are stored on disk or generated in memory or some combination of those)

There are 5 kinds of join. Each kind of join has multiple syntaxes which trigger it.

1. CROSS JOIN (carthesian product)
2. INNER JOIN
3. LEFT OUTER JOIN (aka LEFT JOIN)
4. RIGHT OUTER JOIN (aka RIGHT JOIN)
5. FULL OUTER JOIN (aka FULL JOIN)

#### 1. CROSS JOIN (carthesian product)

* the following are all the same
    * `FROM a, b`
    * `FROM a, b WHERE true`
    * `FROM a CROSS JOIN b`
    * `FROM a INNER JOIN b ON (true)` (a cross join can be expressed as an inner join)
* is just syntax sugar for a FROM and a simple WHERE that always returns true
* Number of rows in the output table:
    * If a has `N` rows and b has `M` rows the output will **always** be  `N * M` rows
    * min = `N * M`
    * max = `N * M`
    * a cross join with an empty table (or result set) results in empty table (M x N; hence M x 0 = 0)

Pseudocode for `CROSS JOIN`:

* See `./code/joins_in_ruby.rb`

#### 2. INNER JOIN

* is a _filtered cross join_
* is just syntax sugar for a FROM and a simple WHERE comparing equality of two columns i.e. the following are all the same:
    * `FROM a, b WHERE a.id = b.id` (desugared inner join)
    * `FROM a INNER JOIN b ON a.id = b.id` (inner join)
    * `FROM a       JOIN b ON a.id = b.id` (inner keyword can be omitted)
    * `FROM a       JOIN b USING (id)` (inner join using the `USING` syntax sugar)
* Number of rows in the output table:
    * min = 0 (the WHERE always returns false)
    * max = If a has `N` rows and b has `M` rows the output can have up to `N * M` rows (if the WHERE clause always returns true)
        * of course, if it does return the max number of rows you are back to a CROSS JOIN
* See `./code/joins_in_ruby.rb` for a simplistic Ruby implementation

#### 3. LEFT OUTER JOIN

* Cannot be replicated with FROM and WHERE
* must have an ON/USING condition
* A `LEFT OUTER JOIN` is computed by computing an INNER JOIN but instead of discarding rows where the join condition doesn't match, we add a new row to the output which is made by taking the row from the LHS table and filling in nulls for the RHS
* A LEFT OUTER JOIN result set is **not a subset** of CROSS JOIN - you get rows in the output of a LEFT OUTER JOIN which doen't exist in the output of a CROSS JOIN of the same tables
* If a given LHS row matches multiple RHS rows for the given join condition then a new row will be output for each match!
    * => LEFT OUTER JOIN does not output each row of the LHS table exactly once!
    * => It's not really correct to think of it as "decorating" the LHS table with columsn from the RHS
* See `./code/joins_in_ruby.rb` for a simplistic Ruby implementation

#### 4. RIGHT OUTER JOIN

* A RIGHT OUTER JOIN is just a LEFT OUTER JOIN with the left and right exchanged - no new concept to learn!
* See `./code/joins_in_ruby.rb` for a simplistic Ruby implementation

#### 5. FULL OUTER JOIN

* Cannot be replicated with FROM and WHERE
* must have an ON/USING condition
* A full outer join will always have rows unless both M and N are 0.
* You can simulate a FULL OUTER JOIN by UNIONing a LEFT OUTER JOIN and RIGHT OUTER JOIN
* An alternative to `FULL OUTER JOIN` is to use an INNER JOIN, a LEFT JOIN (with right side IS NULL) and a RIGHT JOIN (with left side IS NULL) and do a UNION - sometimes this approach is better because you can customize each individual join more obviously (and add a derived column to indicate which side is found or whether it's found in both and which one is going to win)
* See `./code/joins_in_ruby.rb` for a simplistic Ruby implementation

#### FROM clause tricks

Duplicating the columns in a table by INNER JOINing it to itself:

```sql
-- If you need to make a new table which is the original table copied beside
-- itself multiple times then you can join it to itself:

select * from genre as a, genre as b where a.genreid = b.genreid;
```


### WHERE clause

* removes rows from the table generated by the FROM clause
* returns a Boolean (truth value) expression, and only rows for which the Boolean expression is true are returned

WHERE vs HAVING

* WHERE filters rows **before** GROUP BY gets them
* HAVING filters rows **after** GROUP BY i.e. it filters the "group rows" created by GROUP BY

### GROUP BY

`GROUP BY` is only one way to trigger "grouped query" mode. There are a number of conditions that will flip the switch to make the query a "grouped query"

1. if there is a GROUP BY clause
2. if there is a HAVING clause (even if there is not GROUP BY clause)
3. if the query contains aggregate functions (even if there is no GROUP BY)

If _grouped query_ gets triggered and there is no GROUP BY to specify it then there is assumed to be one group which contains all the rows returned by the WHERE clause

Consequences of _grouped query_ being triggered:

1. in a grouped query there must be only one output row per group
1. SELECT clause must be compatible i.e. it is not valid for the SELECT list expressions to refer to ungrouped columns except
   1. within aggregate functions or
   2. when the ungrouped column is functionally dependent on the grouped columns, since there woul otherwise be more than one possible value to return for an ungrouped column.

Mental model for "grouped query":

    are the rows taht come out of WHERE are "tagged" with group ???
    or is it more like a boolean set on the query which causes the select list to be checked for "grouping validity" (my term) and then the actual grouping happens during the SELECT list evaluation
    you can't convert the WHERE table into the output oF GROUP BY without reaching into the SELECT list ???

    GROUP BY and the aggregate functions in SELECT are part of the same "grouping" operation

    aggregate functions in the SELECT clause are actually a bit of an oddity because they are about removing rows, most (maybe all???) other SELECT expressions are about adding or removing columns

a confusing thing is that the SELECT list isn't just for pulling out columns, it can also transform rows using aggregate functions

### HAVING

* removes rows from the table generated by the GROUP BY clause
* HAVING filters rows **after** GROUP BY i.e. it filters the "group rows" created by GROUP BY
* The presence of HAVING turns a query into a grouped query even if there is no GROUP BY clause.

### The **SELECT list**:

* the words between `SELECT` and `FROM` are called the **SELECT list** : `SELECT {SELECT list} FROM ...` - those are the words we are actually interested in here.
* a list of expressions which form the **output** rows of the query
* the expressions usually refer to columns from the FROM clause

```sql
-- In PG you can omit the FROM clause if you aren't referencing values from a table/view
SELECT version();
SELECT current_date;
SELECT 2 + 3;

-- this works provided there are more than 0 rows in the table
SELECT 2 + 2, version(), current_date FROM ar_internal_metadata;
--  ?column? |    version                              | current_date
-- ----------+-----------------------------------------+--------------
--         4 | PostgreSQL 10.1 on x86_64-pc-linux-gnu, | 2018-11-12
```

QUESTION: what is 'current_date'? it's not a function

There are two phases to computing the output columns

1. the values of the output columns are computed
2. DISTINCT is evaluated on the result of step 1. i.e. duplicate rows are removed from the output columns

There are 3 variants of SELECT

* `SELECT ALL ...`
    * the default - you don't have to specify the `ALL` keyword (and we usually don't)
    * returns all rows from the result set
* `SELECT DISTINCT ...`
    * all duplicate rows are removed from the result set (one row is kept for each set of duplicates)
* `SELECT DISTINCT ON (expression [, ...])`
    * keeps only the first row for each _set of rows_ where the given expressions evaluate as equal
    * IMPORTANT: the "first row" is unpredictable unless you also specify an `ORDER BY`
    * The `DISTINCT ON` expressions **must match** the left-most `ORDER BY` expressions (the `ORDER BY` can contain extra expressions to control ordering within each `DISTINCT ON` group)
    * steps
        1. create the in-memory result set (based on the FROM and WHERE clauses etc.)
        1. calculate the value of the expressions for each row
        1. keep only the first row for each unique value of the expressions set

```sql
-- the combination of col1 and col2 is used to evaluate the duplicate
SELECT DISTINCT col1, col2 FROM t1;

-- keeps the first row from each group of duplicates
-- results will be unstable unless you add an ORDER BY clause

-- 1. Build a table in memory with all rows from t1 (there is no WHERE clause to remove rows)
-- 2. Sort the in-memory table by col1 and then col2
-- 3. Return only one row (the first one in the sorted table) for each unique value of col1
SELECT DISTINCT ON (col1) col1, col2 FROM t1 ORDER BY col1, col2;
```

### ORDER BY

* changes the order of rows in the teable generated by the SELECT list
* is evaluated **after** the SELECT list!
* sorts **result** rows (not input rows)
   * column names are specified by either the **output** column name or its position in the output table (note: all output, not input)

> If an ORDER BY expression is a simple name that matches both an output column name and an input column name, ORDER BY will interpret it as the output column name. This is the opposite of the choice that GROUP BY will make in the same situation. This inconsistency is made to be compatible with the SQL standard.


