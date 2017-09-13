
# Why venn diagrams are not great to illustrate JOIN

Venn Diagrams are perfect to illustrate actual set operations! SQL knows three of them:

1. UNION
1. INTERSECT
1. EXCEPT

these set operations operate on sets of elements (tuples), which are **all of the same type**.

* JOIN allows you to combine things of different types e.g. a `films` table joined to `actors` table
* A JOIN is really a cartesian product (also called cross product) with a filter

* cross join:
* inner join: cross join, then remove any rows that don't match the join condition
* left outer join:
    * cross join, then remove rows where ???
* full outer join:
    * cross join

https://blog.jooq.org/2016/07/05/say-no-to-venn-diagrams-when-explaining-joins/

```

Eoin: Something about these arguments bothers me

I think the venn diagram representation is good for visualising what you can achieve even if it is inaccurate

In a venn diagram with circle A overlap circle B

A and B are sets of item X
the item is a row
a row in A does not have the same tuple type as a row in B e.g.
row in A = (a1, a2, a3, a4)
row in B = (b1, b2, b3, a4)
but they can be related by comparing one of the elements of row A with an element in row B
what if we consider the set item to be (a1, a2, a3, a4, b1, b2, b3)
```


# Semi join and Anti join

* semi join and anti join are ideas from relational algebra
* there is no SQL syntax for these
* you can implement them in SQL using
    * IN()
    * EXISTS()
* postgres optimizer will recognise semi joins and make a special query plan
* ORMs use this pattern
    * examples ???

> semi join: "give me just columns from table A but only if some data in A matches some data in B"

> anti join: "give me just columns from table A but only if some data in A does NOT match some data in B"


```sql
-- given two tables: employees, departments

-- implement "semi join" with IN
SELECT *
FROM employees
WHERE department_name IN (SELECT name from departments)

-- implement "semi join" with EXISTS
SELECT *
FROM employees
WHERE EXISTS ( SELECT 1 FROM departments where employees.department_name = departments.name)
```

* QUESTION: how does those semi join implementations go if columns can be null?

## Semi join

Sources

* https://blog.jooq.org/2015/10/13/semi-join-and-anti-join-should-have-its-own-syntax-in-sql/

> In most cases, SQL is much much more powerful than relational algebra. However,
> there are three operators in relational algebra, that have no exact
> representation in SQL, and can only be expressed through “workarounds”. These
> operators are:
>
> 1. Semi join
> 1. Anti join
> 1. Division

# NULL

* `NULL` is not equal to `NULL`

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
# EXCEPT

# INTERSECT
