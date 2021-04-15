# SQL Constraints

* https://www.postgresql.org/docs/current/ddl-constraints.html
* Data types are the basic way of constraining data but they don't go far enough sometimes
    * e.g. there is no data type for numbers > 12
* Constraints are part of the SQL standard
* Constraints allow you to define constraints on:
    1. table (multiple columns in the table)
    2. column (an individual column)

There are 5 types of constraints

1. Check constraints
1. Not-null constraints
1. Unique constraints
1. Foreign key constraints
1. Exclusion constraints

## Constraint: Check

* can be applied to a single column or a whole table
* if applied to a table can compare columns within the same row
* The CHECK constraint is satisfied if the expression evaluates to true or null
    * As ever, be careful with NULL's tricksy three value logic
* Caveats:
    * There are some things you can do in CHECK constraints which will cause database dump and reload to fail:
        1. Postgres assumes check constraints are immutable i.e. it will give the same result for the same inputs - so be very careful if you are referencing a function you defined yourself in the constraint
            * this can cause a dump/reload to fail
        1. PostgreSQL does not support CHECK constraints that reference table data other than the new or updated row being checked.
            * it might look like it works but it doesn't - see:
                > PostgreSQL does not support CHECK constraints that reference table
                > data other than the new or updated row being checked. While a CHECK
                > constraint that violates this rule may appear to work in simple
                > tests, it cannot guarantee that the database will not reach a state
                > in which the constraint condition is false (due to subsequent changes
                > of the other row(s) involved). This would cause a database dump and
                > reload to fail. The reload could fail even when the complete database
                > state is consistent with the constraint, due to rows not being loaded
                > in an order that will satisfy the constraint. If possible, use
                > UNIQUE, EXCLUDE, or FOREIGN KEY constraints to express cross-row and
                > cross-table restrictions.
                >
                > If what you desire is a one-time check against other rows at row
                > insertion, rather than a continuously-maintained consistency
                > guarantee, a custom trigger can be used to implement that. (This
                > approach avoids the dump/reload problem because pg_dump does not
                > reinstall triggers until after reloading data, so that the check will
                > not be enforced during a dump/reload.)

```sql
-- apply CHECK constraint to individual columns
CREATE TABLE things (
    a integer,
    b integer CHECK (b > 10),
    -- long-hand way (which allows custom naming)
    c integer CONSTRAINT b_is_sensible CHECK (b > 10),
);

-- apply CHECK constraint to multiple columns
CREATE TABLE things (
    a integer,
    b integer,
    c integer,

    -- a constraint using the short-hand syntax
    CHECK (a > b > c > 100),

    -- long-hand syntax
    CONSTRAINT oddly_specific_disaster_avoided CHECK (a > b AND b > c AND c > 100 AND c != 400),
);
```

## Constraint: Not-null

* a short cut for `CHECK column_name IS NOT NULL` constraint
    * ++ more efficient (presumably implemented differently under the hood?)
    * -- you cannot provide a name for the constraint
* you can apply a `NOT NULL` with other check constraints (order does not matter)

```sql
CREATE TABLE things (
    id integer NOT NULL
    -- equivlant check constraint:
    -- id integer CHECK id IS NOT NULL

    -- can apply multiple check constraints (order does not matter
    price NOT NULL CHECK (price > 10) CHECK (price < 20)
);
```

## Unique constraints

* IMPORTANT: Adding a `UNIQUE` constraint will automatically create a B-Tree index on the column or combination of columns in the constraint
* remember that because of the way NULL is handled (i.e. it's not equal to itself) two null values will not violate a unique constraint

```sql
-- syntax
CREATE TABLE things (
    a integer UNIQUE

    -- create the constraint with the long-hand syntax that allows us to give the constraint a custom name
    b integer CONSTRAINT must_be_different UNIQUE
);

CREATE TABLE things (
    a integer
    b integer
    c integer
    -- apply a unique constraint to the table to say that, the (a, c) tuple must be unique across rows
    UNIQUE (a, c)
);
```

## Constraint: Primary Key

* functinoally a short-hand for adding a UNIQUE constraint and a NOT NULL constraint
* ++ documents your intention to other humans and also to clients using the DB
* a table can have multiple columsn with `UNIQUE NOT NULL` contraints but only one with a `PRIMARY KEY` constraint
* Relational theory says every table needs a primary key. Postgres doesnt' enforce this but you'll probably be sad if you don't live that way.

```sql
CREATE TABLE things (
    a integer PRIMARY KEY,
    b integer
);

-- same as above
CREATE TABLE things (
    a integer,
    b integer
    PRIMARY KEY (a)
);

CREATE TABLE things (
    a integer,
    b integer

    -- create a primary key across multiple columns
    PRIMARY KEY (a, b)
);
```

## Constraint: Foreign key

* can be appled to column or table
* enforces that the column (or columns if appled to a table) must

TODO

## Constraint: Exclusion

* adding an exclusion constraint will automatically create an index of the type specified in the constraint declaration
* if all of the specified operators test for equality then this is equivalent to a UNIQUE constraint

```sql

CREATE TABLE things (
c circles,

);
```

> Exclusion constraints ensure that if any two rows are compared on the specified columns or expressions using the specified operators, at least one of these operator comparisons will return false or null.

## Deferring when constraints are applied

* Postgres constraints are enforced at the end of each **statement** by default
    * This means that if you have a transaction that temporarily puts the table in an illegal state PG will not allow it.
* You can use the following options to tweak this
    * DEFERRABLE INITIALLY IMMEDIATE
    * DEFERRABLE INITIALLY DEFERRED
    * See https://www.postgresql.org/docs/current/sql-set-constraints.html


## In Rails

Rails 6.1+ migrations have basic support for Postgres check constraints - see https://github.com/rails/rails/pull/31323

