# Window functions

Aside: `array_agg()`

* a very useful function to help understand window functions
* when called on a table (bag of row) as input, it will create an array of the given column value from each row


```sql
select array_agg(num) from generate_series(1,15) as a_nums(num);
--                array_agg
-- ═══════════════════════════════════════
--  {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
-- (1 row)
```

SQL is odd because the code we see in the _SELECT list_ isn't the function getting called with the args you pass it - the args you pass it are kind of "setup options" that tune the behaviour of the function when it's actual inputs (the rows) arrive.

```
# Imaginary DSL for writing SQL in a Rubyish language
# @return [Table]
generate_series(start: 1, end: 15, table_name: "a_nums", col_name: "num")
|> array_agg(on_col: "num")
```



## Basics

* are a way of running _aggregate functions_ over a given frame of rows rather than the hard-coded GROUP BY which runs over a fixed window of "all rows returned by the WHERE clause"
* lets you process several values of the result set at the same time
* a "window" is a collection of "peer rows" - you can output a single output value from that set of rows, similar to using a normal SQL  aggregate function
* a window function operates on a set of rows, **but it does not reduce the number of rows returned by the query**.
* for each input row you have access to a "frame" of data
* NB: window functions happen **after the WHERE clause** so they operate only on whatever passes those tests

```sql
-- without a frame specification, the default window for each row is the whole set of rows
-- You can use this to do aggregates over the whole set of rows
-- Q: can this behaviour be replicated via GROUP BY and HAVING ?
over ()


-- a "window defintion" aka "frame specification":
over (order by xx)

-- the above window defintion is just short-hand for the following "window
-- defintion"
over (order by xx rows between unbounded preceding and current row)
```

* You can use `PARTITION BY` to define different frames
    * it allows you to define "peer rows" as those rows which share a common property with the _current row_

Example _frame specifications_:

```
over ()
over (order by xx)
over (order by xx rows between unbounded preceding and current row)
over (order by xx rows between current row and unbounded following)

```

```sql
select *
    from generate_series(1,5) as a_nums(num);
--  num
-- ═════
--    1
--    2
--    3
--    4
--    5
-- (5 rows)

select num,
        array_agg(num) over (order by num rows between unbounded preceding and current row)
    from generate_series(1,5) as a_nums(num);
--  num │  array_agg
-- ═════╪═════════════
--    1 │ {1}
--    2 │ {1,2}
--    3 │ {1,2,3}
--    4 │ {1,2,3,4}
--    5 │ {1,2,3,4,5}
-- (5 rows)

select num,
        array_agg(num) over (order by num)
    from generate_series(1,5) as a_nums(num);
--  num │  array_agg
-- ═════╪═════════════
--    1 │ {1}
--    2 │ {1,2}
--    3 │ {1,2,3}
--    4 │ {1,2,3,4}
--    5 │ {1,2,3,4,5}
-- (5 rows)


select num, array_agg(num) over () as window_frame, sum(num) over (order by num rows between current row and unbounded following) as overall_sum from generate_series(1,15) as a_nums(num);

select num,
        array_agg(num) over (order by num rows between unbounded preceding and current row)
    from generate_series(1,15) as a_nums(num);
--  num │               array_agg
-- ═════╪═══════════════════════════════════════
--    1 │ {1}
--    2 │ {1,2}
--    3 │ {1,2,3}
--    4 │ {1,2,3,4}
--    5 │ {1,2,3,4,5}
--    6 │ {1,2,3,4,5,6}
--    7 │ {1,2,3,4,5,6,7}
--    8 │ {1,2,3,4,5,6,7,8}
--    9 │ {1,2,3,4,5,6,7,8,9}
--   10 │ {1,2,3,4,5,6,7,8,9,10}
--   11 │ {1,2,3,4,5,6,7,8,9,10,11}
--   12 │ {1,2,3,4,5,6,7,8,9,10,11,12}
--   13 │ {1,2,3,4,5,6,7,8,9,10,11,12,13}
--   14 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14}
--   15 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
-- (15 rows)

select num,
        array_agg(num) over (order by num rows between current row and unbounded following)
    from generate_series(1,15) as a_nums(num);
--  num │               array_agg
-- ═════╪═══════════════════════════════════════
--    1 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    2 │ {2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    3 │ {3,4,5,6,7,8,9,10,11,12,13,14,15}
--    4 │ {4,5,6,7,8,9,10,11,12,13,14,15}
--    5 │ {5,6,7,8,9,10,11,12,13,14,15}
--    6 │ {6,7,8,9,10,11,12,13,14,15}
--    7 │ {7,8,9,10,11,12,13,14,15}
--    8 │ {8,9,10,11,12,13,14,15}
--    9 │ {9,10,11,12,13,14,15}
--   10 │ {10,11,12,13,14,15}
--   11 │ {11,12,13,14,15}
--   12 │ {12,13,14,15}
--   13 │ {13,14,15}
--   14 │ {14,15}
--   15 │ {15}
-- (15 rows)

select num,
        array_agg(num) over () as window_frame
    from generate_series(1,15) as a_nums(num);
--  num │             window_frame
-- ═════╪═══════════════════════════════════════
--    1 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    2 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    3 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    4 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    5 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    6 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    7 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    8 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--    9 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   10 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   11 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   12 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   13 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   14 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
--   15 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
-- (15 rows)

postgres@postgres=#  select num, array_agg(num) over () as window_frame, sum(num) over () as overall_sum from generate_series(1,15) as a_nums(num);
-- num │             window_frame              │ overall_sum
--═════╪═══════════════════════════════════════╪═════════════
--   1 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   2 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   3 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   4 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   5 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   6 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   7 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   8 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   9 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  10 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  11 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  12 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  13 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  14 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--  15 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--(15 rows)

select num,
        array_agg(num) over () as window_frame,
        sum(num) over (order by num rows between current row and unbounded following) as overall_sum
    from generate_series(1,15) as a_nums(num);
-- num │             window_frame              │ overall_sum
--═════╪═══════════════════════════════════════╪═════════════
--   1 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--   2 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         119
--   3 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         117
--   4 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         114
--   5 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         110
--   6 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         105
--   7 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          99
--   8 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          92
--   9 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          84
--  10 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          75
--  11 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          65
--  12 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          54
--  13 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          42
--  14 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          29
--  15 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          15
--(15 rows)

select num,
        array_agg(num) over () as window_frame,
        sum(num) over (order by num) as overall_sum
    from generate_series(1,15) as a_nums(num);
-- num │             window_frame              │ overall_sum
--═════╪═══════════════════════════════════════╪═════════════
--   1 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │           1
--   2 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │           3
--   3 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │           6
--   4 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          10
--   5 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          15
--   6 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          21
--   7 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          28
--   8 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          36
--   9 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          45
--  10 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          55
--  11 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          66
--  12 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          78
--  13 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │          91
--  14 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         105
--  15 │ {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15} │         120
--(15 rows)
```

