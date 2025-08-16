Primary key

1. each key value is unique
2. no key value is null
    - `null` introduces three-value logic which makes things much more complex

Primary keys allow you to implement the 1st and 2nd normal forms

A primary key which comes from the data itself is called a _natural key_ A
primary key which is added artificaially (e.g. an `id` column added by Rails) is
a _surrogate key_

Be careful with surrogate keys because they don't prevent you from adding
duplicate data to a database! You can mitigate this with 1. a unique index
across one or more columsn 2. disallowing null in columsn that must be unique
