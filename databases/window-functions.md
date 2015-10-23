# Window functions

```
SELECT a, b, avg(c) OVER (PARTITION BY a) FROM some_table;
SELECT <col-1>, <col-2>, <window-function-col> FROM <source-table>
    <window-function-col> is <grouping-func> OVER (PARTITION BY <partition-col-name>)
```

How postgres generates the `<result-table>`

* foreach row in `<source-table>`
    * start building new resulttable row
        * copy `<col-1>` and `<col-2>` from `<source-table>` to `<result-table>`
        * start processing the <window-function-col>
            1. read the value of <partition-col-name> from the current row
            2. use that value to do a search of the table for all rows whose
               <partition-col-name> matches that value.
            3. take the set of rows (note: full rows of table) generated in the
               step above and feed it into the `<grouping-func>`.
            4. copy the output of the gropuing function into the result-table row


