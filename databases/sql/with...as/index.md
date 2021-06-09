# Common table expressions (WITH)

* allows you to create something somewhat equivilant to a view that only exists during that transaction
* lets you clean up your SQL by defining "query local" temporary tables and then _naming them_
* https://www.postgresql.org/docs/9.6/static/queries-with.html
* you can add the `RECURSIVE` modifier to `WITH` which allows the statement within the `WITH` to call itself
    * it is more like iteration than recursion but `RECURSIVE` is what SQL standardised on

```sql
WITH
name1 AS (),
name2 AS (),
name3 AS ()
SELECT ....

```