# Postgres data types

- Postgres type names
    - are case insensitive unless quoted (like all other identifiers)
    - can have spaces in the name which is confusing to me sometimes
- Each data type has an internal representation and has one or more external
  representations.
- Each external representation is decided by which input and output function you
  use.
- Some input and output functions for a type are not "invertible" i.e. you might
  lose precision if you put data in and out of Postgres via these functions

Postgres has 40+ built-in data types

#### Numeric types

```plain
   Name                                 Aliases         Description
1. smallint                             int2            signed two-byte integer
2. integer                              int,int4        signed four-byte integer
3. bigint                               int8            signed eight-byte integer
4. double precision                     float8          double precision floating-point number (8 bytes)
5. real                                 float4          single precision floating-point number (4 bytes)
6. numeric[(precision, scale)]          decimal[(p, s)]  exact numeric of selectable precision
7. bigserial                            serial8         autoincrementing eight-byte integer
8. smallserial                          serial2         autoincrementing two-byte integer
9. serial                               serial4         autoincrementing four-byte integer
```

General points about numeric types

- There are nine numeric types
- attempts to store data outside of range results in an error

Numeric type

- the `numeric` type can store very large values precisely
    - recommended for storing large amounts of money
    - ++ calculations are precise
    - -- calculations are slow

            QUESTION: how do i use the numeric type?

### serial types

- `serial` is not a real type!
- serial types are just syntax sugar for doing the following
    1. create a column of type
       `integer NOT NULL nextval('sertest_id_seq'::regclass)`
    2. create a sequence named `{tablename}_{columnname}_seq` with the owner set
       to the column (so the sequence is dropped if the colum is dropped)
- note: serial type does not add PRIMARY KEY or any uniqueness constraint
- when you use a serial "type" PG creates a sequence and makes the column its
  owner (so seq is dropped if col is dropped)

#### Bit string types

```plain
    Name              Alias   Description
10. bit[(n)]                  fixed-length bit string
11. bit varying[(n)]  varbit  variable-length bit string
```

#### Binary data type

```plain
    Name    Description
12. bytea   binary data ("byte array")
```

- use this for storing "raw bytes"
- allows storing of arbitrary binary strings (including null bytes and other
  sequences not allowed by the DB character sets)

          Q: Why no BLOB type?

The SQL standard defines a different binary string type, called BLOB or BINARY
LARGE OBJECT.

The input format is different from `bytea`, but the provided functions and
operators are mostly the same.

#### Boolean type

```plain
    Name       Alias           Description
13. boolean    bool            logical Boolean (true/false)
```

#### Character types

```plain
    Name                     Alias           Description
14. text                                     variable-length character string (not in SQL standard)
15. character[(n)]           char[(n)]       fixed-length character string
16. character varying[(n)]   varchar[(n)]    variable-length character string
```

- `text` and `varchar` and `character varying` (note the lack of limits on the
  varchar) are exactly the same
    - they are all implemented as a `varlea` http://www.varlena.com/varlena.php
- The database character set determines the character set used to store textual
  values
- character types measure length in characters not bytes
- values of type `character` are padded with spaces to the specified width
- Pro-tip: don't use `character` in postgres
    > character(n) has performance advantages in some other database systems,
    > there is no such advantage in PostgreSQL; in fact character(n) is usually
    > the slowest of the three because of its additional storage costs. In most
    > situations text or character varying should be used instead

There are also two internal textual types

1. name - 64 bytes, used for storing identifiers in the internal system catalogs
2. char - stores just one byte, used internally

#### Network address types

```plain
    Name          Alias           Description
17. cidr                          IPv4 or IPv6 network address
18. inet                          IPv4 or IPv6 host address
19. macaddr       MAC             (Media Access Control) address
```

#### Geometric types

```plain
    Name          Alias           Description
20. circle                        circle on a plane
21. line          infinite        line on a plane
22. lseg          line            segment on a plane
23. box                           rectangular box on a plane
24. path          geometric       path on a plane
25. point         geometric       point on a plane
26. polygon       closed          geometric path on a plane
```

#### Date and time types

```plain
    Name                                            Alias       Description
27. date                                                        calendar date (year, month, day)
28. time[(precision)] [without time zone]                       time of day (no time zone)
29. time[(precision)] with time zone                timetz      time of day, including time zone
30. timestamp[(precision)] [ without time zone ]                date and time (no time zone)
31. timestamp[(precision)] with time zone           timestamptz date and time, including time zone
32. interval[fields][(precision)]                               time span
```

#### Search types

```plain
    Name          Description
33. tsquery       text search query
34. tsvector      text search document
```

#### Misc types

```plain
    Name                    Description
35. txid_snapshot           user-level transaction ID snapshot
36. uuid                    universally unique identifier
37. xml                     XML data
38. json                    JSON data
```

#### Money type

```plain
    Name          Description
39. money         currency amount
```

- the money type depends on the `lc_monetary` DB setting (`SHOW lc_monetary`)
- if you dump a DB and restore it to a DB with a different lc_monetary setting
  can cause problems
- The only numeric type you should cast `money` to is `numeric` (not `float`
  etc.) to ensure you keep precision!

### Casting

```sql

-- Option 1:
-- * doesn't work for arrays, only works for setting the type of a "simple literal constant"
-- * SQL standard allows this on only a few types, Postgres allows it on all
type 'string'

-- Option 2:
-- historic postgres syntax
'string'::type

-- you can chain casts
-- cast x to type1 then cast that to type2 and so on
x::type1::type2::type3

-- Option 3:
-- Fully SQL standard compliant
CAST ( 'string' AS type )

-- Option 4:
-- historic postgres syntax
-- there is a 4th syntax but it doesn't work for all types
typename ( 'string' )
```

#### Types which are portable across database systems

SQL specifies that the following 19 types should exist (with this spelling)

1. bigint
2. bit
3. bit varying
4. boolean
5. char
6. character varying
7. character
8. varchar
9. date
10. double precision
11. integer
12. interval
13. numeric
14. decimal
15. real
16. smallint
17. time (with or without time zone)
18. timestamp (with or without time zone)
19. xml

These 19 types are the only types you can use portably across DB systems

You can create new types with `CREATE TYPE ..` - `man CREATE_TYPE`

    QUESTION: how do I use a "type input function"
