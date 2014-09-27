# SQL and Relational Theory Masterclass

By Chris Date
Based on his book _SQL and Relational Theory_

Based on a 3 day seminar
Assumes I know SQL

Relational model is based on the papers written by Ted Codd beginning in 1969

CJD contends that

* SQL is complex
* It is very hard to test exhaustively so you need disipline
* That disipline of using the SQL "relationally"

Using SQL relationally is bood because

* helps you write correct programs

This course will

1. Cover relational theory fairl (explaining what it is but often not why)
2. Apply that theory to SQL practice

CJD believes SQL deviates from relational theory

SQL omits some useful parts of relational theory
SQL implements some parts of relational theory incorrectly
SQL also impements some things that have nothing to do with relational theory

When he refers to SQL he is referring to the standard 2008 version

SQL and the relational model are NOT the same thing.

Questions we will ask and answer the following questions:

1. What exactly is the _first normal form_?
1. What is the connection between _relations_ and _predicates_?
1. What is _semantic optimization_?
1. What is an _image relation_?
1. What is _semidifference_ and why is it important?
1. Why doesn't _deferred integrity checking_ make sense?
1. What is a relation variable?
1. What is _prenex normal form_?
1. Can a relation have an attribute whose values are themselves relations?
1. Is SQL relationally complete?
1. What is the _Information Principle_?
1. How does XML fit into the relational model?

Relational term | SQL Term
----------------|----------
    relation    | table
    tuple       | row
    attribute   | column

Note that the above are similar but not the same

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


Codd's original relational model

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
    * Relational algebra (a collecitons of operations on relations)
    Any algebra is a collection of operations on things e.g. matrix algebra
    operates on matrices, relational algebra operates on relations. Each
    operation take a number of relations as input and creates new relations as
    output.

    Because the output is the same type of thing as the input, then the output
    can be used as input for other operations.

    This is called the "closure property" of the algebra - these operataions
    can never produce anything but relations so things always stay in the same
    area.

    Ted Codd defined the following 8 operations:
        * Intersection
        * Union
        * Difference
        * Product
        * Restrict
        * Project
        * Join
        * Divide
    * Relational assignment
        * allows you to take an expression of the algebra and assign it to a variable
            C = A JOIN B (C remembers the value of A JOIN B)

UP TO 23:00

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
* Arithmetic is performed according to an order of operations.
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


As it developed, algebra was extended to other non-numerical objects, such as vectors, matrices, and polynomials.

