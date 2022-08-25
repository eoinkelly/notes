# Block range index (BRIN)

https://www.postgresql.org/docs/current/brin-intro.html

## About

* designed for very large tables where the indexed column(s) have a strong natural correlation with the physical location of the tuple in the pages in the table
* _block range_
  * a group of pages which are physically adjacent in a table
* the index stores a summary of the _block range_

> Because a BRIN index is very small, scanning the index adds little overhead
> compared to a sequential scan, but may avoid scanning large parts of the table
> that are known not to contain matching tuples

## Pros

* much smaller than b-tree indexes
    * because the index is small, it is fast to scan and can help PG exclude whole ranges of tuples


## Cons

* requires correlation between the physical tuples in pages i.e. only works well for append-only tables
  * deletes will create gaps in the tables which PG will later fill and break correlation
  * updates will cause postgres to write a new tuple, potentially to a new page so breaks correlation
