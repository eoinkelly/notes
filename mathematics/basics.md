# Branches of math

sum_(i=1)^n i^3=((n(n+1))/2)^2

`sum_(i=1)^n i^3=((n(n+1))/2)^2`

$sum_(i=1)^n i^3=((n(n+1))/2)^2$

Handy: [http://oeis.org/wiki/List_of_LaTeX_mathematical_symbols](http://oeis.org/wiki/List_of_LaTeX_mathematical_symbols)

* Calculus
    * the study of change
    * to do with relationships between quantities
    * Two major branches
        1. Differential calculus - the study of rates of change and slopes of curves
        2. Integral calculus - the study of areas under and between curves
    * The two branches are related using the _fundemental theory of calculus_
    * A calculus refers to any system of calculation guided by the symbolic
      manipulation of expressions e.g. lambda calculus
* Algebra
    * the study of operations and how they can be applied to solve equations
    * from an arabic word meaning the _reunion of broken parts_
    * the study of symbols and the rules for manipulating symbols
    * a unifying thread of almost all of mathematics
    * 2 forms: _Elementary_ and _Advanced_
        * Elementary algebra differs from arithmetic in the use of
          abstractions, such as using letters to stand for numbers that are
          either unknown or allowed to take on many values
    * Can also be a specialized kind of _mathematical object_ e.g. _linear algebra_
* Geometry
    * the study of shape
* Arithmetic
    * The study of numbers, especially the properties of the traditional
      operations between them — addition, subtraction, multiplication and
      division.
    * A part of _number theory_ - until the 19th century they were pretty much
      the same thing.
    * The oldest and most elementary branch of mathematics - origins around
      2000 BCE ish.
    * Any _set_ of objects on which _all four_ of the basic operations can be
      performed is called a **field** i.e. if you can add, subtract, multiply,
      divide these objects and they follow the normal rules then they are a
      _field_.
    * each _field_ has some special elements:
        * identity element
            * the element which when operated on with any other element returns
              the other element
        * inverse element
            * the element which when operated on with a particular element
              returns the identity element
    * 4 basic operations:
        * addition
            * commutative and associative
            * identity element is 0
            * inverse element of A is -A
            * can be performed using geometry using sticks!
        * subtraction
            * the inverse of addition
            * not commutative, not associative
            * because it is less convenient when things are not commutative and
              associative you can think of $a - b$ as $a + (-b)$ instead.
        * multiplication
            * the second basic operation of arithmetic
            * is commutative and associative
            * distributive over addition and subtraction - ???
            * identity element is 1
            * inverse element of an element is its reciprocal i.e. $x * (1/x) = 1$
        * division
            * the inverse of multiplication
            * anything divided by 0 is not defined
            * not commutative, not associative
            * because it is less convenient when things are not commutative and
            associative you can think of $a/b$ as $a * (1/b)$ instead.
    * note that addition and multiplication are the real _basic_ operations -
      subtraction and division are just their inverses.
    * Also are other more advanced operations:
        * percentages
        * square roots
        * exponentiation
        * logarithms

Aside: without the concept of 0 our positional number scheme would not be possible!

### Top level divisions of modern mathematics

1. Algebra
2. Geometry
3. Analysis
4. Number theory
    * arithmetic is in this division

# Common sets of Numbers

http://algebra.freehomeworkmathhelp.com/Numbers/sets_of_all_numbers_subsets.GIF

* Natural numbers $\mathbb{N}$
    * Also called _whole numbers_
    * Used for counting and ordering
    * A natural number is a number that occurs commonly and obviously in
      nature. As such, it is a whole, non-negative number.
    * There is no universal agreement about whether to include zero in the set
      of natural numbers but including 0 is now the common convention in most
      branches of mathematics.
    * There are a number of ways of defining what exactly the _natural numbers_
      are e.g. peano arithmetic
* Real numbers $\mathbb{R}$
    * a real number is a value that represents a quantity along a continuous line.
    * a value that can be represented on the number line (also called the _real line_)
    * reals = rationals + irrationals + transendentals
    * includes
        * transcendental numbers
        * irrational numbers
        * fractions
* Integers $\mathbb{Z}$
    * A number that can be written without a fractional component
    * integers = $\{ naturals, additive inverses of the naturals aka the negative naturals, 0 \}$
    * Examples:
        * 1, 2, 44 are integers
        * $\sqrt{2}$, 4.56, $3 1/4$ are not integers
* Complex numbers
    * complex numbers are a superset of real numbers
* Imaginary numbers
* Rational numbers
    * integers + fractions + ???
* Irrational numbers
    * Examples: $\sqrt{2}$
* Transcendental numbers
    * Examples: $\pi$


# Functions

* functions can be written in english or more succicently using mathemtatical
  symbols
* functions are the same iff they produce the same output for the same input
* it does not matter how they "look" i.e. they don't have to have the same
  "rule" or the same "way of turning the input into the output"

## Important functions

* $f(x) = x$ the identity function
* $f(x) = c$ the constant function
* $f(x) = ax + b$
* $f(x) = x^n$
* $f(x) = 2x^3 + 5x^2 - 2x + 1$
    * a cubic _polynomial_
* $f(x) = \sqrt{x}$
* $f(x) = |x|$
* $f(x) = sin(x)$ (transcendental)
* $f(x) = cos(x)$ (transcendental)
* $f(x) = tan(x)$ (transcendental)

Functions can be _composed_ into a sort of conveyor belt that pipes the output
of one function into the input of another e.g. $f(g(x))$ or $f \circ g$.

## Polynomials

* a polynomial is an expression consisting of variables and coefficients, that
  involves only the operations of addition, subtraction, multiplication, and
  non-negative integer exponents.
    * http://en.wikipedia.org/wiki/Polynomial

## tancendental functions

> A transcendental function is an analytic function that does not satisfy a
> polynomial equation, in contrast to an algebraic function. (The polynomials
> are sometimes required to have rational coefficients.) In other words, a
> transcendental function "transcends" algebra in that it cannot be expressed
> in terms of a finite sequence of the algebraic operations of addition,
> multiplication, and root extraction.  Examples of transcendental functions
> include the exponential function, the logarithm, and the trigonometric
> functions.

### Defn: domain of a function

* The set of input values can I put into a function and get a _valid_ output
* Note: it is a _set_ of values
* Example: for $f(x) = x^2$ the domain is $\{ x \in \mathbb{R}\}$ or _x is
  contained in the real numbers_
* Examples:
    * for $f(x) = \sqrt{x}$ the domain is $\{x \in [ -1, \infty ) \}$
    * for $f(x) = 1/\sqrt{x}$ the domain is $\{x \in \mathbb{R} | x \neq 0 \}$

### Defn: range of a function

* The _set_ of all possible values the output of a function can take on given all possible inputs

# Square root function

The square root function takes a number an spits out some outputs. When you
multiply that new number by itself you get back the original number.

There are two possible outputs of this function for any given number - the
positive and negative roots. This means that the function could do two
different things and still be "correct" and we can't say for sure which should
happen. This is an ambiguity - it maps one input value to two possible output values.

In code we could probably have this thing just return an array of the possible
answers but mathematics is not comfortable with that becasue ???.

    QUESTION: Why is it a bad thing for a function to retrun an array of
    answers in math? (Sun 26 Apr 12:34:18 2015)

Math really wants a function that takes _one_ number and returns _one_ number
so we _define_ the square root function to be

> The _non negative_ number which squares to x

This means that by convention we always pick the positive root. A somewhat
surprising consequence of defining the square root function in this way is that

$\sqrt{x^2} \neq x$

but we **can** say that

$\sqrt{x^2} = |x|$

The domain of $\sqrt{}$ is $\{ x \in [0, \infty) \}$

* the domain here is described here using an _interval_
* The [ means to include the 0
* the ) means to exclude the $\infty$ (because infinity is not a number)

Aside: Infinity ($\infty$) is not a number

```
# wolfram alpha command
plot sqrt(x), x=-10 to 10

# a quad root
(1 - x^4)^(1/4) >= 0
```

# Cube and quad root functions

Need to think more about these

## Discrete vs Continuous data

https://mathbitsnotebook.com/Algebra1/FunctionGraphs/FNGContinuousDiscrete.html

Continuous function

* A _continuous function_ $f(x)$ is one where $x$ can take on * + []*any** value in the chosen interval including fractions, decimals and irrationals.
* continuous functions are graphed with points connected by a _continuous line_ (because every value is a possible value)
* continuous data usually requires an instrument to measure (it is often some real world quantity)

Discrete function
u
* A _discrete function_ $f(x)$ is one where $x$ can take on only certain points in the interval, usually only integers or whole numbers.
* Discrete functions are graphed by drawing only the points which make sense for the data
* Discrete data is often measured by _counting_.
* It is often preceeded by the words _The number of ..._

# Difference between full derivative and partial derivative

If you have a function which depends on two input variables e.g. x and y you can get the "partial derivative" of x by assuming that y is constant as x changes i.e. y doesn't change in response to changes in x

> The key difference is that when you take a partial derivative, you operate under a sort of assumption that you hold one variable fixed while the other changes. When computing a total derivative, you allow changes in one variable to affect the other.

Example:

$$f(x) = 3x + 2y$$

TODO: re-learn the mechanics how to take derivatives
