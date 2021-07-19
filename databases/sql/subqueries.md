# Subquery Expressions

https://www.postgresql.org/docs/current/functions-subquery.html

Subquery expressions can appear within the following clauses:

1. FROM
    * Purpose: create a virtual table to use in the outer query
    * Return type: a table
    * LATERAL JOIN syntax was invented to make this use-case easier
1. WHERE
    * Purpose: use fields from a diff table to filter rows from the outer table
    * Return type: depends on the operator consuming the subquery expression
    * Return value is always consumed via an operator of some kind
    * Subquery expressions can be used everywhere WHERE is e.g. within INSERT, UPDATE, DELETE, SELECT
1. SELECT
    * Purpose: add a column to the output of the outer query which is built using data from tables not JOINed in the outer query
    * Return type: scalar value
1. ORDER BY
    * Purpose: sort the output of the outer query using data from tables not JOINed in the outer query
    * Return type: scalar value
    * you can use any expression here which would be valid in a select list
1. maybe others??? e.g. HAVING, GROUP BY ?

Overview

* also called _Nested query_ or _Inner query_
* some operators can take a _subquery expression_ as an argument
* The subquery can refer to variables from the surrounding query
    * Those variables are constants from the pov of an individual evaluation of the subquery
* It is a bad idea to write a subquery that has side effects e.g. incrementing a sequence.
    * A subquery passed to `EXISTS` is only evaluated enough to know whether it would return rows so you might get unpredictable behaviour
* Subqueries can emulate a JOIN but they are different in that they can't duplicate rows of the input table

## Questions

    Q: can you pass a subquery as an arg to a function? or just operator?

    Are there sestrictions on SELECT when using it as a subquery? some things I
    have read say yes but is that DB specific?
        e.g.  Cannot use ORDER BY but you can use GROUP BY to get a similar effect


## Subqueries in WHERE

The following operators can take subquery expressions

1. EXISTS
2. IN
3. NOT IN
4. ANY/SOME
5. ALL
6. Single row comparison

```sql
WHERE colname <OPERATOR> ( <SUBQUERY_WHICH_RETURNS_SINGLE_ROW> )

-- Examples
-- EXISTS operator only cares about the number of rows returned
WHERE EXISTS ( <SUBQUERY_WHICH_RETURNS_SINGLE_ROW> )
WHERE EXISTS ( SELECT 1 FROM othertable WHERE condition ) -- conventional way to use subquery with EXISTS

-- IN
-- use this when subquery returns multiple rows
-- * Types: The IN operator takes ? and returns ?
WHERE colname IN ( <SUBQUERY_WHICH_RETURNS_MULTIPLE_ROWS> )
```

### EXISTS

* It is a bad idea to write a subquery that has side effects e.g. incrementing a sequence. A subquery passed to `EXISTS` is only evaluated enough to know whether it would return rows so you might get unpredictable behaviour