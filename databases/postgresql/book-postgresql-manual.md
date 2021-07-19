# PostgreSQL

Sources

* https://www.postgresql.org/docs/current/

## Preface

* _PostgreSQL_ is a descendent of the _POSTGRES, Version 4.2_ system created at Berkeley
    * POSTGRES didn't have SQL interperter, POSTGRES95 added it
    * POSTGRES95 was renamed to PostgreSQL
    * PostgreSQL v6 was released ('95 became retroactively a v5)
    * _Postgres_ (not in all caps) became a common nickname
* It it highly extensible - you can add new functions, data types, operators, programming languages, index methods etc.

Conventions in the docs

    [] = optional part
    { a | b } = must choose one option
    ... = the preceeding element can be repeated


## Part 1: Tutorial

* Architecture
    * Client/server
    * it has a "master worker" architecture
        * Server has a listener daemon (the `postgres` binary) which forks a worker to handle each active connection
        * the server forks a new process for each new connection. Once the server has created the worker process and done the plumbing for that worker to get the TCP connection then the server is no longer involved.
    * Clients are diverse: can be programming libraries, text tool, GUI tool
    * Comms are over TCP/IP or a Unix domain socket
* Manages data as relations (aka tables)
    * Data organised into rows and columns
    * Column order is fixed but **row order is not promised by SQL unless you explicitly add ORDER BY**
* Tables are organised into a database
* The set of databases managed by a single Postegres server is called a _cluster_
* Docs are available as man pages e.g. `man SELECT` (must use uppercase name for SQL commdands)
* Tip: You can use `TABLE mytable;` instead of `SELECT * FROM mytable;`
* PostgreSQL is a relational database management system (RDBMS).
    * That means it is a system for managing data stored in relations.
    * Relation is essentially a mathematical term for table.
* Table
    * is an unordered collection of tuples of the same type
        * IMPORTANT: SQL does not promise any particular order of the rows within a table!
* Rows
    * Rows are tuples.
    * Each row is a tuple of a particular fixed composite (make up of multiple parts) type e.g. `(integer, integer, varchar, date)`
        * When you create a table in Postgres, it also creates a matching composite type (as if you typed `CREATE TYPE ...`)
* Columns
    * Columns within a row have a fixed order (as you would expect from a tuple)
    * Each column within a row has a fixed data type
    * type names are not keywords (postgres has many types and can be extended with others)
    * type names can have spaces in them e.g. `character varying`
* **Many Postgres features are aimed at a multi-user account system where individual users have their own database**
    * If you don't specify a username it will default to your OS username
    * If you don't supply a db name to `createdb` it defaults to creating a DB with the same name as the user you logged into postgres as i.e. if you logged into postgres as your OS username (the default behaviour) then it will create a DB with your OS username

* Terminology
    * database: a group of tables
    * cluster: the collection of databases managed by the server

### Helper commands installed with postgres

* full list https://www.postgresql.org/docs/current/reference-client.html
* Passing `-e` to these tools makes them echo the SQL they run for you
* examples
    * `createdb`
        * a convenience wrapper around `CREATE DATABASE` SQL statement
    * `dropdb`
        * a convenience wrapper around `DROP DATABASE` SQL statement
    * `psql`
        * see below
    * `pg_config`
        * dump info about the DB (will show you where the various DB directories are)
    * `clusterdb`
        * wrapper around SQL `CLUSTER` command
        * TODO: what does "reclustering a DB" do?
* these commands are compiled binaries not scripts

### psql

* prompt is `=>` for normal user or `=#` if you are superuser
* postgres lets you do most/all of its management via a SQL like interface
    * it presents its internal server state as tables and you can operate on them
    * it provides functions you can invoke via a`SELECT` clause with no `FROM` clause - these functions to manipulate the cluster

Command line args

* `-s` enabled "single step mode where you are prompted between each command - very useful for debugging scripts

Internal psql commands begin with '\'

```
\q              -- quit
\i foo.sql      -- read in commands from foo.sql (pair it with -s
\h              -- show SQL help
\?              -- show \ commands help
\db             -- list tablespaces
```

#### variables

* psql provides variable substitution features similar to common Unix command shells.
* Variables are simply name/value pairs, where the value can be any string of any length.
* Some psql variables help configure psql - don't trample on them - use `\set` to list them.
* Variables are never created quoted - you have to quote them to use them

```sql
\set -- show all currently defined internal variables

-- set a variable
\set aa hello
\set bb `ls -l` -- you can set var based on output of some command
\set bb `pbpaste` -- set var to clipboard contents

\unset aa

\echo :aa

-- you have to quote the variable to use it in SQL
SELECT :'aa'; -- use single quotes to quote as a string constant
SELECT * FROM :"aa" -- double quotes to quote as an identifier
```

### Tablespaces

https://www.postgresql.org/docs/10/manage-ag-tablespaces.html

* Tablespaces allow database administrators to define locations in the file
  system where the files representing database objects can be stored.
* When creating tables you can specify a `TABLESPACE` to control where in storage the table will go
* Commands
    * List tablespaces: `\db`
    * Show which tablespace each database is in: `\l+`
* two are created by default:
  1. `pg_global`
  2. `pg_default`
* use cases
    * most storage is on cheap spinning disk but you want to put some heavily trafficked indexes on a fast SSD
    * you run out of space on your drive and need to create new tables on a separate drive
* the tablespace dir is part of the cluster =>
    * you should not back it up or copy it on its own - you can't attach it to a different cluster
    * if a tablespace gets corrupt then the whole cluster could go down

> Tables, indexes, and entire databases can be assigned to particular tablespaces.

> ... allow database administrators to define locations in the file system
> where the files representing database objects can be stored. Once created, a
> tablespace can be referred to by name when creating database objects.

```sql
-- create a tablespace
CREATE TABLESPACE fastspace LOCATION '/ssd1/postgresql/data';

-- tablespace can be specified explicitly when creating tables, indexes, databases
CREATE TABLE foo(i int) TABLESPACE fastspace;

-- or you can set the default tablespace
SET default_tablespace = fastspace;
CREATE TABLE foo(i int);
```

#### The "maintenance DB"

* PG requires you to connect to a database do do things
* Defaults first the `postgres` database, then `template1` if `postgres` does not exist

#### Template databases

* `template0`
    * cannot be modified
* `template1`
    * used as the **default template** for **new** databases
    * if you install a new programming language extension into `template1` then it will be enabled on all new databases created

#### Choices to make when creating a database

1. Encoding
1. Locale
1. Collation
1. Owner
1. Whether to base this database from a named template database

#### Environment variables

Env vars are probably the best way to configure postgres clients.

```
PGDATABASE
PGHOST
PGPORT
PGUSER
PG_COLOR # values: always, auto and never.

# psql only
PSQL_EDITOR
PSQL_PAGER
```

```bash
# see what
$ env|grep "^PG"

PGUSER=postgres
PGDATABASE=postgres
PGHOST=localhost
```

#### Help

Postgres SQL is documented as man pages as well e.g.

```
$ man CREATE_DATABASE
$ man DROP_DATABASE
```

## Part 2: The SQL Language

### Chapter 4 SQL Syntax

* Keywords
    * totally case insensitive: SELECT == select == SeLeCt
* Identifiers
    * they _identify_ the names of objects in the DB e.g. tables, columns,
    * must begin with a letter
    * only first 64 bytes (by default, can be altered with NAMEDATALEN constant at compile time) matter
    * > If you want to write portable applications you are advised to always quote a particular name or never quote it.
        * Rails quotes all identifiers in the SQL it generates
    * case insensitive
        ```sql
        select * from users; --works
        select * from USERS; --works
        select * from UsErS; --works
        ```
    * Can be made case sensitive by turning into a _quoted identifier_
        * surround with double-quotes e.g. `"users"`
            ```sql
            select * from "users"; --works
            select * from "UsErS"; --breaks
            ```
        * kinds of quoting
            ```sql
            "thing"
            "thing with "" within"

            U&"hell\0061" -- allow unicode characters as codepoint numbers
            u&"hell\0061" -- same as above (u|U are equivalent)
            U&"\\hell\0061" -- double the escape char to include it literally
            U&"hell!0061" UESCAPE '!' -- can use diff escape char if you want \ for something else

            select u&"id" from users limit 4;
            ```
* Constants (3 types):
    1. Strings
        * There are 4 quoting options:
            1. single quotes
            1. single quotes with `E` prefix
            1. single quotes with `U&` prefix
            1. dollar quotes `$$` or `$name$`
                * because they can be named with a tag they can be nested
                * tags are case sensitive `$$blah$$` is not same as `$$BLAH$$`
        * examples:
        ```sql
        'thing'
        'thing''s other thing' -- double ' to include it

        SELECT 'hel'
        'lo'; -- string constants separated by newlines are automatically joined

        SELECT 'hel' 'lo'; -- breaks (no newline)

        E'hello\nthere' -- prefix with E|e to enable C style escape chars
        e'hello\nthere' -- same as above

        e'hello\tthere\u1234' -- including a unicode char
        e'hello\tthere\x12\x34' -- including hex chars
        e'hello\\there' -- double \ to include a real \

        -- the u|U prefix works for string constants similarly to how it does for quoting identifers
        U&'hell\0061' -- allow unicode characters as codepoint numbers
        u&'hell\0061' -- same as above (u|U are equivalent)
        U&'\\hell\0061' -- double the escape char to include it literally
        U&'hell!0061' UESCAPE '!' -- can use diff escape char if you want \ for something else

        -- dollar quotes can be used to create string constants
        select $$id$$ as dollar_quote_ex1;

        -- they can be named so that you can nest them
        select $xy$id$xy$ as dollar_quote_ex1;

        select $xy$somebody's $$ thing$xy$ as dollar_quote_ex1; -- works
        -- dollar_quote_ex1
        -- ---------------------
        -- somebody's $$ thing
        -- (1 row)

        select $$somebody's $$ thing$$ as dollar_quote_ex1; --breaks
        -- ERROR:  syntax error at or near "as"
        -- LINE 1: select $$somebody's $$ thing$$ as dollar_quote_ex1;

        -- often used for defining pgpsql functions
        $function$
        BEGIN
            RETURN ($1 ~ $q$[\t\r\n\v\\]$q$);
        END;
        $function$
        ```
    2. Numbers
        * Examples of valid numeric constants
            ```sql
            42
            3.5
            4.
            .001
            5e2
            1.925e-3
            ```
        * Postgres puts the numeric constant into an `integer` (32 bits) if it fits, otherwise `bigint` (64 bits), otherwise `numeric`
    3. Bit strings
        ```sql
        b'1001' -- binary bit string
        B'1001' -- same as above

        x'bada55' -- hex
        X'bada55' -- same as above

        select x'bada55' as hex_ex;
        --         hex_ex
        -- --------------------------
        -- 101110101101101001010101
        -- (1 row)
        ```

#### Type casting

Constants (string, bit string, numeric) can be explicitly cast to a type using 3 syntaxes:

> A cast applied to an unadorned string literal represents the initial
> assignment of a type to a literal constant value, and so it will succeed for any
> type (if the contents of the string literal are acceptable input syntax for the
> data type).

Option 3 is probably the best one to use because it is explicit and SQL compliant
Option 2 seems popular in examples online

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

TIP: Use `pg_typeof()` function to get the type of a value.

#### Special chars

The following chars are special chars:

```
$(),;:*.[]
```

#### Comments

```sql
-- works

/*
    works
    /* works nested (even if it confused the markdown parser) */
*/
```

UP TO END https://www.postgresql.org/docs/current/sql-syntax-lexical.html

### 4.2 Value Expressions (aka expressions yielding a scalar)

* value expression = scalar expression = an expression which evaluates to a scalar
* table expression = expression that evaluates to a table

There are 16 types of value/scalar expression:

1. A constant or literal value
1. A column reference
    * `correlation.colname` where a _correlation_ is one of:
        1. a table name with schema `schemaname.tablename`
        1. an alias for a table name (defined in a `FROM` clause)
    * The correlation can be omitted if the colname is unique across all the tables in the current query
1. A positional parameter reference, in the body of a function definition or prepared statement
    * e.g. `$1`
1. A subscripted expression
    * subscripts turn an array expression into a scalar expression
    * examples:
        ```sql
        someary[sub]
        someary[start:end]
        (somefunc(x))[sub] -- the array exp must be wrapped in () if the syntax would be ambigious
        ```
    * each subscript is itself an expression which will be rounded to the nearest `integer`
1. A field selection expression
    * extract a field from a composite type e.g. a row
    * expression must sometimes be wrapped in `()` if syntax would be unclear
    * `expr.fieldname`
    * `expr.*` extract all fields
1. An operator invocation
    ```sql
    -- exp operator exp -- infix
    -- exp operator -- postfix
    -- operator exp -- prefix
    -- OPERATOR(schemaname.operatorname) -- fully qualified operator name

    -- these are the same
    SELECT 3 + 4;
    SELECT 3 OPERATOR(+) 4;
    ```

1. A function call
    * Postgres uses functions to **emulate** "computed fields" on composite types
    * A function that takes a single argument **of composite type** can optionally be called using field-selection syntax, and conversely field selection can be written in functional style.
        ```sql
        -- these are exactly the same!
        select projects.name from projects limit 1;
        select name(projects) from projects limit 1;

        -- this breaks because, while sin() takes a single arg, it doesn't take a composite type
        select num, sin(num), num.sin from projects limit 5; -- fails
        ```
1. An aggregate expression
    * is the application of an aggregate function across the rows selected by a query
    * reduces multiple inputs to a single output (scalar) value
    * aggregates can have an optional
        1. ORDER BY clause
        1. FILTER clause
            * use this when you want want the aggregate to calculate over a subset of the rows it normally would
    * some aggregates are a special class called "ordered set" aggregates
        * these functions only make sense if the input to the aggeregate is ordered
        * they use a special `WITHIN GROUP (ORDER BY blah)` syntax
1. A window function call
    * applies an aggregate function over some subset of the rows selected by the query
    * applies the aggregate once per input row
        * the the function can be fed a collection of rows surrounding the current row
        * the window definiton specifies which rows
            * the partition = a grouping of rows created by some expression
                * for each row, the window function is fed a subset of the rows in the partition of the current row
                * the default partition (if none specified) is the whole result set
        * the window frame = the subset of the partition that will be fed to the window function
            * e.g. can
                * excluce current row
                * exclude a range before and/or after the current row
                * exclude a grouping before and/or after the current row

1. A type cast
1. A collation expression
    * you can set a custom collation on an expression
    * if no collation set on an expression it defaults to the database collation
        ```sql
        SELECT a, b, c FROM tbl WHERE ... ORDER BY a COLLATE "C";
        ```
    * only text data types are collatable

    * TODO: what exactly is a collation
1. A scalar subquery
    * an ordinary SELECT query in parens which returns exactly one row and one column
    * if it returns now rows the result is NULL
    * variables from the outer query act as constants when the inner query is being evaluated
    * examples
        ```sql
        -- scalar subquery
        SELECT
            name,
            (SELECT max(pop) FROM cities WHERE cities.state = states.name)
        FROM states;

        -- I _think_ I can rewrite this as a join
        SELECT
            states.name,
            max(cities.pop)
        FROM states
            JOIN cities ON states.name = cities.state
        ```
1. An array constructor
    * An expression which builds and array
    * ex
        ```sql
        SELECT ARRAY[1,2,3+4];
        --  array
        -- ---------
        -- {1,2,7}
        -- (1 row)

        -- you can type cast literals as you create the array
        SELECT ARRAY[1,2,22.7]::integer[];
        -- array
        -- ----------
        -- {1,2,23}
        -- (1 row)

        -- you can make multi-dimensional arrays
        SELECT ARRAY[[1,2],[3,4]];
        --     array
        -- ---------------
        -- {{1,2},{3,4}}
        ```

1. A row constructor
    * A row constructor is an expression that builds a row value (also called a composite value) using values for its member fields
    * You can use `correlation.*` syntax to expand to all the columns in a row
    * By default the row is of an anonymous composite type
        * It can be cast as to a named composite type (either a table row (`CREATE TABLE ...`) or a type (`CREATE TYPE ...`))
    * ex
        ```sql
        SELECT ROW(1,2.5,'this is a test');

        -- create a new named composite type
        CREATE TYPE myrowtype AS (f1 int, f2 text, f3 numeric);

        -- these are the same.
        SELECT ROW(t.*, 42) FROM t;
        SELECT ROW(t.f1, t.f2, 42) FROM t;

        ```

1. Another value expression in parentheses (used to group subexpressions and override precedence)
    * Be careful that AND, OR are **not short circuit operators** like they are in other languages
1. Odd stuff like `IS NULL`
    * evaluates like an expression but follows diff syntax rules

### 4.3 Calling Functions

https://www.postgresql.org/docs/13/sql-createfunction.html

* introduced by
    * `CREATE FUNCTION` or
    * `CREATE OR REPLACE FUNCTION`
* argmode
    * can be `IN|OUT|INOUT|VARIADIC` (default `IN`)
* arguments
    * functions can take positional or named args (note named uses an old school ruby hash alike syntax)
        ```sql
        SELECT concat_lower_or_upper(a => 'Hello', b => 'World');
        SELECT concat_lower_or_upper('Hello', 'World');
        ```
    * you can mix and match at call time (positional first then named, similar to Ruby)
    * can have default values
* return type
    * always specified
    * can be `void`
    * can return the set of rows from the last query in the body with either:
        * `RETURNS TABLE(columns)`
        * `RETURNS SETOF sometype`
* body
    * arguments referenced by either name or position e.g. `$1`
    * written as a string constant
    * usually delimited with `$$` because it is the most flexible
    * runs SQL in order
    * all statements end with `;` except it is optional on the last one
* suffix
    * set the name of the language the function is written in
        * `LANGUAGE SQL`
    * Other flags you can set
        * `WINDOW`: is a window function
        * `VOLATILE|IMMUTABLE|STABLE`: tell the query optimizer about the function
        * `STRICT`: return null if any args are null

```sql
-- template of defining a function
CREATE FUNCTION {name}({arg}, {arg}, ...)
RETURNS {returntype}
AS
$$
{body_of_function}
$$
LANGUAGE SQL;

-- example
CREATE FUNCTION concat_lower_or_upper(a text, b text, uppercase boolean DEFAULT false)
RETURNS text
AS
$$
 SELECT CASE
        WHEN $3 THEN UPPER($1 || ' ' || $2)
        ELSE LOWER($1 || ' ' || $2)
        END;
$$
LANGUAGE SQL IMMUTABLE STRICT;
```

### Chapter 5 Data Definition

#### Chapter 5.5

Every table has a number of hidden system columns:

```sql
select tableoid,xmin,cmin,xmax,cmax,ctid from projects limit 5;
--  tableoid |  xmin  | cmin | xmax | cmax | ctid
-- ----------+--------+------+------+------+-------
--    149632 | 548735 |    0 |    0 |    0 | (0,1)
--    149632 | 548735 |    0 |    0 |    0 | (0,2)
--    149632 | 548735 |    0 |    0 |    0 | (0,3)
--    149632 | 548735 |    0 |    0 |    0 | (0,4)
--    149632 | 548735 |    0 |    0 |    0 | (0,5)
-- (5 rows)
```

* tableoid
    * the oid of the table containing this row
    * can be joined to pg_class to find the table name e.g.
        * `select * from pg_class where oid = (select distinct tableoid from projects);`
        * not sure when this is useful?
* xmin
    * transaction ID of the inserting transaction for this **row version**
* cmin
    * command identifer within the inserting transaction (starts at 0)
* xmax
    * transaction id of the deleting transaction or 0 if not deleted
    * non-zero if the row is deleted but the delting transaction has not commited
* cmax
    * command identifer within the deleting transaction or 0
* ctid
  * physical location of the row version within its table
  * changes when you VACUUM FULL
