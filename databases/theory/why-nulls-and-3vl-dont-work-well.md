# Why nulls and 3VL don't work

- CJD asserts that N-value logic is not a good solution to the problem of
  missing information.
    - he asserts it is dangerous
- SQL uses 3VL (TRUE, FALSE, NULL)

## Review of 2VL

There are 5 common connectives in 2VL:

1. NOT
2. OR
3. AND
4. IF (implication)
5. IFF (equivalence)

### AND

| a     | b     | AND   |
| ----- | ----- | ----- |
| TRUE  | TRUE  | TRUE  |
| TRUE  | FALSE | FALSE |
| FALSE | TRUE  | FALSE |
| FALSE | FALSE | FALSE |

### IF (implication)

| a     | b     | B IF A |
| ----- | ----- | ------ |
| TRUE  | TRUE  | TRUE   |
| TRUE  | FALSE | FALSE  |
| FALSE | TRUE  | TRUE   |
| FALSE | FALSE | TRUE   |

- Symbol: $\rightarrow$
- It doesn't really map to the everyday usage of _if_ or the programmer usage of
  _if_

Consider

> If you get an A then I'll give you a dollar

    a = "you get an A"
    b = "I give you a dollar"

Implication is about the **promise** that if a then B, not about whether B
actually happened. It is evaluating "did I break the promise that if a happens
then b happens"

If a doesn't happen then it's impossible to break that promise so the output is
true

if you don't get an A then whether or not I give you a dollar doesn't matter
when deciding whether the statement is true - this explains the last 2 lines of
the table.

Maybe another way to look at this:

If you think about all the possible dyadic operations you could have in 2VL,
it's clear there are 16 of them (each truth table has 4 rows of output and each
output value has 2 possible values and $2^4 = 16$ )

Of the 16 truth tables, each of which corresponds to an operation, fall into
three categories:

1. some of them correspond well to ideas we use in everyday life e.g. "and",
   "or" "not" so naming them is easy.
2. Some are useful operations but don't map as nicely to everyday concepts so
   the naming is a bit more of a stretch - I think _implication_ and
   _equivalence_ fall into this category.
3. Other operations (truth tables) aren't super useful so we don't bother with
   names

### IFF (equivalence)

| a     | b     | B IFF A |
| ----- | ----- | ------- |
| TRUE  | TRUE  | TRUE    |
| TRUE  | FALSE | FALSE   |
| FALSE | TRUE  | FALSE   |
| FALSE | FALSE | TRUE    |

- Symbol: $\leftrightarrow$
- The output is true only if both a and b are TRUE or both A and B are false
- Sometimes called "double implication"

### Connectives in 2VL

How many possible connectives are there in 2VL

- There are 4 possible monadic operations
    - monadic operation has one input and one output
    - the input can have 2 values (T or F)
    - For each input, there are 2 possible output values (T or F)
    - 2 \* 2 = 4
- THere are 16 possible dyadic connectives
    - each operation has 2 inputs and 2 outputs
    - each input can have 2 values (T or F)
    - for each input, each output can have 2 possible values
    - 2 _ 2 _ 2 \* 2 = 16

So in 2VL there are only 20 possible connectives!

### Tautology

Evaluates to TRUE no matter what truth values are assigned to operands e.g.

        P OR (NOT P)

        # demorgans law
        NOT(P AND Q) = (NOT P) OR (NOT Q)
        NOT(P OR Q) = (NOT P) AND (NOT Q)

The above are tautologies _and_ identities - identities are important for
rewriting queries

### Contradiction

Something which is always false

    P AND (NOT P)

## Three valued logic (3VL)

> If you don't know what value a given attribute should have in a given tuple,
> don't give a value - use a **null marker** instead.

- Introduced by Codd in 1979 to allow databases to represent missing values
- CJD says that it is impossible to state the basic idea concisely and
  accurately.
- Defines truth expressions involving null to return UNKNOWN instead of TRUE or
  FALSE

if you have two null values and you want to know if they are equal or not. If
you could tell whether they were equal or not then you actually know something
about them and they are no longer really "missing" values so the only outcome of
askng if two nulls are equal is UNKNOWN

| a       | NOT     |
| ------- | ------- |
| TRUE    | FALSE   |
| FALSE   | TRUE    |
| UNKNOWN | UNKNOWN |

| a      | b       | AND     |
| ------ | ------- | ------- |
| TRUE   | TRUE    | TRUE    |
| TRUE   | FALSE   | FALSE   |
| TRUE   | UNKNOWN | UNKOWN  |
| FALSE  | TRUE    | FALSE   |
| FALSE  | FALSE   | FALSE   |
| FALSE  | UNKNOWN | FALSE   |
| UNKOWN | TRUE    | UNKNOWN |
| UNKOWN | FALSE   | FALSE   |
| UNKOWN | UNKNOWN | UNKOWN  |

| a      | b       | OR      |
| ------ | ------- | ------- |
| TRUE   | TRUE    | TRUE    |
| TRUE   | FALSE   | TRUE    |
| TRUE   | UNKNOWN | TRUE    |
| FALSE  | TRUE    | FALSE   |
| FALSE  | FALSE   | FALSE   |
| FALSE  | UNKNOWN | UNKNOWN |
| UNKOWN | TRUE    | TRUE    |
| UNKOWN | FALSE   | UNKOWN  |
| UNKOWN | UNKNOWN | UNKOWN  |

- As soon as you add null you no longer have tuples because tuples cannot
  "contain" null.
- null is actually a marker or flag on the where the value should be to say tht
  it is missing
- it is not accurate to say that _a table **contains** a null_ but we do say
  that all the time.
- When you no longer have tuples you no longer have relations, we have something
  which is not quite a relation - SQL calls them **tables**

SQL where conditions can return TRUE, FALSE, UNKNOWN but for a given WHERE
condition there are only two possible outcomes: either include the row or don't.
SQL takes UNKOWN in a WHERE clause an effectively coerces it to a FALSE.

i.e. SQL gives you _definite_ output from _indefinite_ input.

```sql
-- sample data for table used in slides
create table suppliers (
    sno character varying not null,
    sname character varying,
    status bigint,
    city character varying,
    constraint suppliers_pkey primary key (sno)
);

insert into suppliers values
  ('S1', 'Smith', 20, 'London'),
  ('S2', 'Jones', 10, null),
  ('S3', 'Blake', null, 'Paris'),
  ('S4', 'Clark', 20, null),
  ('S5', null, null, null)
```

```
postgres@eoin=#  select * from suppliers;
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
 S2  | Jones |     10 | ¤
 S3  | Blake |      ¤ | Paris
 S4  | Clark |     20 | ¤
 S5  | ¤     |      ¤ | ¤
(5 rows)

Time: 4.507 ms
postgres@eoin=#  select * from suppliers where city = 'London';
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
(1 row)

Time: 1.850 ms
postgres@eoin=#  select * from suppliers where city != 'London';
 sno | sname | status | city
-----+-------+--------+-------
 S3  | Blake |      ¤ | Paris
(1 row)

Time: 1.540 ms
postgres@eoin=#  select * from suppliers where status = 10 or city = 'London';
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
 S2  | Jones |     10 | ¤
(2 rows)

postgres@eoin=#  select * from suppliers;
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
 S2  | Jones |     10 | ¤
 S3  | Blake |      ¤ | Paris
 S4  | Clark |     20 | ¤
 S5  | ¤     |      ¤ | ¤
(5 rows)

Time: 4.507 ms
postgres@eoin=#  select * from suppliers where city = 'London';
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
(1 row)

Time: 1.850 ms
postgres@eoin=#  select * from suppliers where city != 'London';
 sno | sname | status | city
-----+-------+--------+-------
 S3  | Blake |      ¤ | Paris
(1 row)

# in 2VL this would be a tautology but not in 3VL
Time: 1.540 ms
postgres@eoin=#  select * from suppliers where status = 10 or city = 'London';
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
 S2  | Jones |     10 | ¤
(2 rows)

postgres@eoin=#  select * from suppliers where city = null;
 sno | sname | status | city
-----+-------+--------+------
(0 rows)

Time: 4.789 ms
postgres@eoin=#  select * from suppliers where city is null;
 sno | sname | status | city
-----+-------+--------+------
 S2  | Jones |     10 | ¤
 S4  | Clark |     20 | ¤
 S5  | ¤     |      ¤ | ¤
(3 rows)

Time: 1.562 ms
postgres@eoin=#  select * from suppliers where city = city;
 sno | sname | status |  city
-----+-------+--------+--------
 S1  | Smith |     20 | London
 S3  | Blake |      ¤ | Paris
(2 rows)

Time: 1.813 ms
```

## MAYBE connective (does not exist in SQL databases)

Loosely, the purpose of NOT is to convert a FALSE into a TRUE

Loosely, the purpose of MAYBE is to convert an UNKNOWN into TRUE

| a       | MAYBE |
| ------- | ----- |
| TRUE    | FALSE |
| FALSE   | TRUE  |
| UNKNOWN | TRUE  |

An example of how MAYBE could be used to return all the rows where the value
**might** be true:

```
SELECT ENO FROM EMP WHERE MAYBE(JOB = 'Programmer' AND DOB < DATE '1962-01-18' AND SALARY < 50000.0)
```

Without MAYBE you have to write a very long SQL WHERE clause where you explictly
test for IS NULL

## 2VL Tautologies and Contradictions are not necessarily so under 3VL

When you go from N value logic to N+1 value logic, things which were tautologies
and contradictions under N-value might not be in N+1 value. Both Codd and SQL
are completely silent about _implication_ and _equivalence_ in 3VL

You can make truth tables for them in 3VL but there are problems.

    IF p THEN P
    is a tautology in 2VL but not in 3VL

    P IFF P
    is a tautology in 2VL but not 3VL

Consequences of this:

- It limits the moves that an optimizer can make
- Users can make mistakes because your internal optimizer might rely on those
  tautology

3VL can give you answers which are correct in the **logic** but not in the real
world.

T JOIN T isn't equal to T in SQL but it should be in relational theory

## Confusing UNKNOWN with "unknown"

- It is important to know that NULL does not mean "either thing or other
  thing" - it really means **unknown**
- SQL uses `null` instead of UNKNOWN as the result of logic connectives **as
  well as** using it to mark missing data

In 3VL there are 3 possible values: TRUE, FALSE, UNKNOWN but in natual language
we use "unknown" to be "we don't know" which is a different concept

## 3VL and higher is not defined precisely in SQL and is very hard to understand

- 3VL has 27 monadic connectives and ($3^9$) 19683 dyadic connectives = 19,710
  connectives
- It's not clear which of those connectives we should call OR, AND, NOT, IFF,
  IF - Codd didn't define it precisely
- It can be shown that there is no 3VL that preserves all the desirable
  properties of 2VL
- THere is no universally accepted 3VL!
    - See Paper: Why Relational DBMS logic must not be many-valued
- If you don't implement all the connecties then CJD thinks you don't really
  have a "logic"

Codd proposed a 4VL to cover:

    "value applicable but not known"
    "value not applicable but not known"

but it doesn't seem to have had widepsread uptake.

## Arguments in favour of null

- people will represent missing data as different things in different DBs if
  null didn't exist - databases would not be interoperable at all

## We actually need infinite kinds of null

SQL has only one kind of null i.e. "value unknown" but there are many kinds of
missing data

1. data exists but we just don't know it
2. data does not exist at all

There is an argument that we need an infinite number of kinds of null (one for
each situation)

## 3VL breaks the relational model:

- a null isn't a value
- a "type" that contains a null isn't a type
- a tuple that contains a null isn't a tuple
- a relation that contains a null isn't a realation

i.e all bets are off in terms of the relational model

## Better alternatives to nulls

- _SQL and relational theory_ book describes a way to design a database to avoid
  nulls
- See _The TransRelational Model_

## Random idea from me

```sql
-- My (probably silly) idea for annotating nulls in a table:
--
-- * lets you have different "kinds" of null in a table and be able to use them differently
-- * however it is not much different to just having a status column except you
--   get some of the "automatic behaviours" of null in SQL databases
-- * I have used an enum type below but "proper" relational design would use a
--   reference table instead (which may be a better idea for your use-case). See
--   https://tapoueh.org/blog/2018/05/postgresql-data-types-enum/ for details
-- * examples
--
--     WHERE (city IS NULL AND city_null_reason = 'SOME_REASON')
--
create type general_nr_t as enum('DATA_EXISTS_BUT_UNKNOWN_TO_US', 'DATA_DOES_NOT_EXIST')

create type city_nr_t as enum('EMPLOYEE_HAS_NOT_FILLED_OUT_FORM', 'EMPLOYEE_LIVES_RURAL')

create table suppliers (
    sno character varying not null,
    sname character varying,

    status bigint,
    status_null_reason general_nr_t

    city character varying,
    city_null_reason city_nr_t

    constraint suppliers_pkey primary key (sno)
);
```
