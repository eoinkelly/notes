# SQL and Relational Theory Masterclass

By Chris Date
Based on his book _SQL and Relational Theory_

Based on a 3 day seminar
Assumes I know SQL

Relational model is based on the papers written by Ted Codd beginning in 1969

CJD contends that

* SQL is complex
* It is very hard to test exhaustively so you need disipline
* That disipline is _using SQL "relationally"_

Using SQL relationally is good because helps you write correct programs

This course will

1. Cover relational theory (explaining what it is but often not why)
2. Apply that theory to SQL practice

CJD believes SQL deviates from relational theory in a number of ways

1. SQL omits some useful parts of relational theory
2. SQL implements some parts of relational theory incorrectly
3. SQL also impements some things that have nothing to do with relational theory

When he refers to SQL he is referring to the standard 2008 version

Key point: SQL and the relational model are NOT the same thing.

Questions we will ask and answer the following questions:

1. What exactly is the _first normal form_?
2. What is the connection between _relations_ and _predicates_?
3. What is _semantic optimization_?
4. What is an _image relation_?
5. What is _semidifference_ and why is it important?
6. Why doesn't _deferred integrity checking_ make sense?
7. What is a relation variable?
8. What is _prenex normal form_?
9. Can a relation have an attribute whose values are themselves relations?
10. Is SQL relationally complete?
11. What is the _Information Principle_?
12. How does XML fit into the relational model?

Consider the rough mapping of terms from relational theory to SQL

Relational term | SQL Term
----------------|----------
    relation    | table
    tuple       | row
    attribute   | column


SQL terms: operator, function, procedure, routine, method all pretty much mean
the same thing so this course will use "operator" to cover all these.

Examples of "operators"

* =     (equality testing)
* :=    (assignment)
* +
* SELECT
* DISTINCT
* UNION
* SUM
* GROUP BY

Compomises and tradeoffs should be made from a position of strong foundational
knowledge.


## Codd's original relational model

3 parts to the model

1. Structure
    * Types (also called "domains")
        * The conceptional pool that we pull all the possible values of a column from
    * n-ary relations
        * can think of as a table with n columns
            * rows correspond to tuples of the relation
            * columns correspond to attributes of the relations
    * keys. some types of key:
        * candidate key
            * a unique identifier
            * can be multiple candidate keys in a relation
        * primary key
            * chosen from one of the candidate keys
        * foreign key
2. Integrity
    A given DB will have unique constraints on it but these refer to
    contstraints taht are on **all** DBs:
    1. Entity integrity
        * has to do with primary keys
        * it says that primary key columns are not allowed to be null
        * CJD believes that nulls are a terrible idea in general
    2. Referential integrity
        * has to do with foreign keys
        * if B references A then A must exist
3. Manipulation
    * Relational algebra (a collection of operations on relations)
    Any algebra is a collection of operations on things e.g. matrix algebra
    operates on matrices, relational algebra operates on relations. Each
    operation take a number of relations as input and creates new relations as
    output.

    Because the output is the same type of thing as the input, then the output
    can be used as input for other operations. This is called the "closure
    property" of the algebra - these operataions can never produce anything but
    relations so things always stay in the same area.

    * Relational assignment
        * allows you to take an expression of the algebra and assign it to a variable
            C = A JOIN B (C remembers the value of A JOIN B)

Relational algebra and relational calculas are related ways of thinking about
the relational model
    TODO: I don't quite get that statement

You can formulate the relational model in either of 2 ways:

1. Structure + Intergrity + Relational algebra + an assigment operator
2. Structure + Intergrity + Relational calculus + an assigment operator

Ted Codd defined the following 8 operations:

1. Intersection
    * takes two relations
    * returns a realtion containing only tuples that appear in both
2. Union
    * takes two relations
    * returns a relation that contains all the tuples in both
3. Difference
    * has a direction to it like subtraction in arithmetic
    * A - B is the tuples that appear in A but do not appear in B
4. Product
    * is really carthesian product
    * takes two relations and returns one
    * Given a relation with N tuples and a relation with M tuples it returns a
      relation with all possible combinations of tuples N x M
5. Restrict
    * Gets a relation as input and picks certain tuples out of it. The output is itself a relation
    * sometimes called "select" but it is **totally different** to SQL SELECT
    * seems to be bit like SQL WHERE
6. Project
    * Takes a single relation and picks out certain attributes. The output is itself a relation
    * seems to be bit like SQL SELECT
7. Join
    * has many flavours
        * most important is "natural join" so when CJD says "join" he means "natual join"
    * takes two relations that have a common attribute
    * uses the common attribute to create a new relation that merges the original relation on the common attribute
    * natual join = all possible combinations where values match in the common attribute
8. Divide
    * most complex of all the operators
    * takes two relations and returns one
    * input relation 1 = "the dividend relation"
    * input relation #2 = the "divisor relation"
    * result = the "quotient relation"
    * returns a relation the _attributes_ in ??? which have a corresponding attribute that matches _every_ attribute in the "divisor"
        * TODO: understand this? is it useful?
        * https://www.simple-talk.com/sql/t-sql-programming/divided-we-stand-the-sql-of-relational-division/


The relational model is a "data model". The phrase "data model" has two different meanings when used:

1. An abstract, self contained, logical definition of the objects and operators
   of an _abstract machine_ that users will interact with.
    * objects allow us to model structure
    * operators allow us to model behaviour
   The model id distinct from the physical realisation _on_ a real machine of
   the abstract machine. In the realm of the physical machine is:
    * how the data is stored on disk (rowwise, column wise etc.)
    * whether there are fast access paths (indexes)
    * what IO occurs under the covers of a join
   For example, a "join" is part of the abstract machine but the indexes,
   optimisations etc. that go to implementing it are part of the implementation.
2. A model of the persistent data of some particular enterprise
    * CJD uses "model" in the first sense

Don't confuse the model and the implementation. For example:
* keys = model concept
* index = implementation concept used to implement keys

If we stick to the model (which is the user interface) then we can chagne the
implementation. This translates into protection of our investment in data and
training. Once of the big goals of the Codd model was physical data
independance because prior to the relational model it didn't really exist.

The model has nothing to do with performance. In this sense, "joins are slow"
does not make sense because "join" is a model property so has nothing to do
with speed. You can talk about an implementation of joins being slow but not
"joins" themselves.


# Video 2

### Anatomy of a relation

Every relation has

1. A heading
    * a (mathematical) set of (attribute-name, type-name) pairs ("two tuples")
    * Note: Underneath every attribute is a type
2. A body
    *  a (mathematical) set of tuples that conform to the types in the heading

The no. of attributes in a relation = _the degree_ (no. columns in table)
The no. of tuples in the body = _the cardinality_ (no. data rows in table)

### Relations and duplicates (nope!)

* If a relation is a mathmatical set then by defnition it cannot contain
  duplicate tuples.
* Relations **NEVER** contain duplicate tuples.
* SQL does not enforce this.

### Relations and ordering (they don't have any)

Relations are sets so they have no built-in ordering.

No row ordering:
* The tuples of a relation are unordered (again a relation is a set). When we
  draw a table we have to pick an ordering but ignore it because it isn't
  really there.

No column ordering:

* The _attributes_ have no ordering left->right Again when we draw on paper we
  need to pick an order - ignore it, it is not really there
* The relation heading is also a set => no ordering
    consequences
        * there is no "first column"
        * there are no "previous" and "next" columns
    SQL departs from the relational model here and CJD hates it.


From the rules about sets we can say:

* Every subset of a tuple is a tuple
* Every subset a heading is a heading
    * even if your heading has only one element it is still a heading
* Every subset of a body is a body
    * even if your body has only one tuple it is still a body

Equality of tuples

Two tuples are equal iff
1. they have the same attributes ((attr-name, attr-value) pair)
2. attributes with the same name have the same value

We call tuples "duplicates" if they are equal.

Many features of the relational model rely on the above e.g. the defn of join, the defn. of what a key is.


### Relations are always normalized (1st normal form)

Relations are always normalized.

* At every row and column intersection there is always exactly one value of the appropriate type
* The terms "normalized" and "in the first normal form" mean exactly the same thing.
* It is possible to define higher levels of normalizeation e.g. 2nd normal form


### A relation and a table are not the same thing!

* A table can be regarded as a concrete _picture or sketch_ of an abstract idea
* Tables don't capture some aspects of relations well e.g. lack of ordering
* A table is a picture of a relation - it suggests things about the relation that are not true

### Base relations vs derived relations

#### Base relations

* are the primary ones that you think are important enough to be part of the database
* they always have names

#### Derived relations

* are derived from base relations using queries
* sometimes have names too e.g. in a view (virtual relation)
* views are sort of saved functions
* users can operate on views as if they were base relations.
* We can think of views as being materialized in memory at the time the query
  is run (in reality this would be terribly slow and would not work for updates
  so does not happen)
* views are saved sub-queries that are run when the referencing query is run
* A view **is** a relation and a table **is** a relation. Similarly a view **is** a table

CJD thinks it is important _not_ to make a distinction between relations that are
stored on disk and those that are in memory - you should not care - it is all relations!

* snapsnots are relations
* views are relations
* base tables are relations
* query results are relations

e.g. we might have a suppliers table but "suppliers in paris" is a derived relation.

`CREATE TABLE` in SQL creates _base relations_.

The relational model has **nothing** to say about physical storage so it is
incorrect to say that a "base relations are physically stored" and "derived
relations are virtual". All the RM cares about is that the base relations can
be somehow figured out from what **is** stored.

### Relvars

* So far what we have called "relations" are relational _values_
* A relvar is a variable whose permitted values are relations

There are 2 types of relvar

1. Base (or "real") relvar - one that is not virtual
2. Virtual relvar - One that is defined in terms of a relational expression in
   terms of one or more other relvars.

Table names in SQL are variables that can take on a value that is a relation

`CREATE TABLE t ...`

* t is a relation variable whose values are relation values - its values will change over time
* t is a relation variable whose current value is a particular relation


* What you really have in a database is _relation variables_. Rather than
  saying "this DB has 4 tables in it" we should say "this DB has 4 relvars in
  it"

* When you do an insert/update/delete you are assigning _relation values_  to _relation variables_
* insert, update and delete can be expressed in terms of assignment.
    `DELETE S WHERE (CITY = paris)`
    is just a way of saying
    `S := S WHERE NOT (CITY = paris)`
    (S is assigned the result of the expression on RHS)

When he says "relation" it is an abbreviation of "relation value". Similarly
when he says "relvar" he means "relation variable"

CJD has his own "Tutorial D" language which will be used in this course to
illustrate points where SQL is not "relational enough"

### Aside: Values vs variables

* Value
    * an _individual constant_ (in logic terms)
    * no location in time or space
        * a value is alwyas the same no matter when in time you ask about it
        * they are available for use anywhere in the universe at any place and
          any time e.g. you can use the integer 4 on multiple countries at the
          same time to represent very different things
    * can't be changed (immutable)
    * can be represented in memory (by some encoding)
* Variable
    * a holder for the representation of a value
    * has a location in time and space
        * the value of a variable depends on the time it is considered at
        * e.g. the integer 4 in a variable only has one meaning in space e.g.
          if it means "we have 4 apples" then it does not mean "we have fired 4
          missles"
    * can be updated

Values and variables can be arbitrarily complex

"All logical differences are big differences" - Wittgenstein

## Aside: null

* represents the absence of information

## Aside: Arithmetic

* The oldest and most elementary branch of mathematics.
* It consists in the study of numbers, especially the properties of the
  traditional operations between them
    1. addition,
    2. subtraction,
    3. multiplication
    4. division.
* also includes more advanced operations, such as manipulations of percentages,
  square roots, exponentiation, and logarithmic functions.
* Arithmetic is performed according to an order of operations: PEMDAS
    * Parenthese
    * Exponent and root
    * Multiply
    * Divide
    * Add
    * Subtract
* Any set of objects upon which all four arithmetic operations (except division
  by 0) can be performed, and where these four operations obey the usual laws,
  is called a _field_.

## Aside: Algebra

* Arithmetic works with actual numbers, algebra works with symbols standing in for numbers
* _Elementary_ algebra differs from arithmetic in the use of abstractions, such
  as using letters to stand for numbers that are either unknown or allowed to
  take on many values. For example, in x + 2 = 5 the letter x is unknown, but
  the law of inverses can be used to discover its value: x=3.
* In E=mc^2, the letters E and m are variables, and the letter c is a constant.
* Algebra gives methods for solving equations and expressing formulas that are
  much easier (for those who know how to use them) than the older method of
  writing everything out in words.
* Algebra began with computations similar to those of arithmetic, with letters
  standing for numbers.[6] This allowed proofs of properties that are true no
  matter which numbers are involved.

As it developed, algebra was extended to other non-numerical objects, such as
vectors, matrices, and polynomials.

## Aside: Calculus

* Calculus is the mathematical study of change in the same way that geometry is
  the study of shape and algebra is the study of operations and their
  application to solving equations.

It has two major branches:

1. Differential calculus (concerning rates of change and slopes of curves),
2. Integral calculus (concerning accumulation of quantities and the areas under
   and between curves)

These two branches are related to each other by the fundamental theorem of calculus.

UP TO END VIDEO 2

    TODO: setup a small practice database

# Video 3 Domains and types part 1

Relations are defined "over" types
Every attribute of a relation is of some type

The relational model implicitly requires support for user defined types
=> users can also make user defined operators to operate on those types

An "object relational" system done right is just a "relational" system done right.

"object relational model" stuff in 90's had one new thing over early SQL systems:

1. allow users to define their own types

CJD contends that was a feature that should have been there from day 1

"The so called object realtional model is just the relational model"


Attributes of relationals can have any type. They can be

* matrices
* booleans
* integers
* XML documents
    * the right way to put xml in a table is for the XML document to be an attribute of a tuple

They cannot be

* pointers
    * Pointers are deliberatle excluded by Codd
* relations have types themselves. A relation r cannot have an attribute which has the same as r itself

There are system defined types and users can define their own types - these should look the same to the user.

The relational model perscribes a system type of BOOLEAN. Real systems also include CHAR, INTEGER, RATIONAL


What is the difference between a pointer and a foreign key?
There are many differences including:

* pointers have a direction: A -> B but you cannot get back to A from B without another pointer
* foreign keys go both ways

CJD has a whole paper on it somewhere.

Domains and Types are the same thing

1. Equality comparisons and "domain check override"
2. Data value atomicity and first normal form


"Everybody" knows that two values can be tested for equality if the come from the same domain e.g.

SNO = user defined type for supplier numbers
PNO = user defined type for part numbers

S.SNO = SP.SNO /* ok, same domain(type) */
S.SNO = P.PNO /* bad, comparing different domains */

Any relational operation that calls for an implicit or explicit equality comparison between values of different domains should fail at complile time.

explicit = WHERE A = B
implicity = a join

Any system that implements this "cross domain checking" before comparisions would need a way to override it because sometimes the user will know more than the system does.

Codd has "domain check override" versions of some of his operators

Codd's domain checking says
    P.QUANTITY = P.WEIGHT /* not ok because not meaningful */
    P.QUANTITY - P.WEIGHT = 0 /* is ok in domain checking but still not meaningful*/
both the expressions above have the same semantics but different syntax
this is odd

CJD claims that domain check override makes no sense at all

Consider
    S.SNO = 'X4' /* valid, might even be TRUE */
    P.PNO = 'X4' /* valid, might even be TRUE */
    S.SNO = P.PNO /* invalid! */
How can this be?


SNO and PNO have different types but each type might have the same representation  e.g. a character string

There should be a _logical difference_ (see above) between a type and representation.
The representation of the type should be kept hidden from the user - it is an implementation detail

CJD recommends having _selector_ operators, named the same as each type that convert the underlying representation of it into the type and a corresponding operator for the inverse operation

This mechanism
provides the domain checking when we need it and the ability to override it when we need it
it would provide "Cross domain checking" in a
clean
fully orthogonal
no ad-hoc
way

These selectors are invoked *implicitly* by the system.

Basically he is tlaking about type coercion between types and their underlying representations

S.SNO = 'X4' /* implicitly coerce the chracter string into an SNO type */

CJD is advocating for strong typing in database

P.WEIGHT * P.QUANTITY /* valid, -> WEIGHT, result is total weight */
P.QUANTITY + P.WEIGHT /* invalid, not a sensible question, caught by type system */
SPX.QUANTITY + SPY.QUANTITY /* valid, -> QUANTITY, both are quantities */

CJD syas that what Codd called domains are actually types and he will refer to them as such.

Data value atomicity

First normal form (1NF) requires that every attribute value in every tuple be atomic
SO what does "atomic" mean?
Codd defines "atomic" as "nondecomposable by the DBMS (excluding certain special functions)"
not an awesome definition.
What do things like strings, times, dates (which are decomposable)?
we just consider them to be sets
    string is a set of characters
    date is a set of numbers
this might be a bad design but it is a legal relation

if our "atomic" values are really sets then the notion of "atomic" depends on what level you are looking at.
sometimese we pretend stirngs are indivisible and sometimes we do not
There is no absolute notion of atomicity!o

You can consider these sets as relations themeselve (they are sets

SQL does not have table valued columns - this is one way that SQL departs from relational theory
SQL does have columns that contian values that are "multisets" of rows

To sum up

Domains (therefore attributes) can contain absolutely _any_ values: arrays, lists, XML documents. The values can be arbitrarily complex without violating 1NF

Domain tripeq Type

Since the relational model supports user defined types it implies that it also supports user defined operations (types would be useless without them)
=> An "Object relational" DBMS done right is just a relational DBMS done right!


The question of what types are supported is independant of (orthogonal to) the question of support for the relational model
OR
Types are orthogonal to tables

The relational model has never perscribed data types - it is not true to say that the relational model can only handle numbers, strings but cannot handle complex types
    Sure the model supports it but do implementations support making user defined types???
    It seems postgres at least can http://www.postgresql.org/docs/9.2/static/sql-createtype.html





"view definitions in the "catalog""
what is the catalog???

