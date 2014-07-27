# Issue 38 - Part 1

This is a series of articles in PragProg about haskell.

* issues 38 - 46 ish starting http://pragprog.com/magazines

* From http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell

* Functional programming is all about _data_
* Start by thinking about data and only then think about the transformations you
  could apply to it
* Author believes that if you start thinking in terms of data structures then FP
  falls quite naturally out of it.
* The unix shell is an example of functional thinking

* New data types are cheap and easy to make
    * anywhere I would normally use a hash in ruby I could make a new data type
      in haskell. The data type would bring some rules with it that a hash does
      not have.
    * Also haskell can automatically generate some functions that will work on
      it using type classes e.g. comparing it, ordering it etc. In ruby this
      would be a sort of a struct that can implement a bunch of interfaces.

* Haskell makes it so easy to create data types that we can do some very
  accurate modelling of our data
* This can help prevent some classes of errors e.g. the `Maybe a` type _forces_
  us to handle the `Nothing` situation in our code which removes the problems we
  have with `nil` in ruby.

# Parametric polymorphism vs OO Polymorphism

* Parametric polymorphism: use the same code for everything
* OO polymorphism: allow different values to be treated the same by calling
  object-specific code.

# Representing "Waiting for X" as a type in Haskell

Sometimes we don't have all the values our record will need e.g. We might be
waiting for user input of a particular part.

```haskell
-- consider a Person type
data Person String (Maybe Int) [String]

-- this lambda represents a person what we are waiting on an address for
nearlyJoe = \address = Person "Joe" (Just 25) address

-- we can turn it into an ordinary Person value by invoking it
reallyJoe = nearlyJoe ["14 wari st.", "wellington"]
```

### Asides:

* #each in ruby would not make any sense in a language that does not have
  mutable values.

Tony Hoare: "code with no obvious deficiences vs code with obviously no
deficiences"

## Favour folds over explicit recursion

* Quote: "explicit recursion in FP is a bit of an anti-pattern"
* `fold` does not have to return just a single value if you choose your
  accumulator properly e.g. have a list or tree as the accumulator. In fact you
  can define maps in terms of folds.
* I should favour folds rather than explicit recursion
* Learning to do FP is about spotting the opportunies for maps and folds to
  transform data
* If I find myself doing explicit recursion I should look for an opportunity to
  use a fold because that will probably look better
