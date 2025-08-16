# Brian Beckman - Monoids talk

Any pure funcitons can be replaced with data lookup

doubler x = x + x

```ruby
def doubler(x)
  x + x
end

def doubler(x)
  doubles = [0,2,4,6,8,10] # etc
  doubles[x]
end
```

types and sets are almost the same thing

int x; # x is the name of a box in memory that can hold ints x :: int -- x is an
element of the `int` set

he reads -> as 'to'

```haskell
-- this type signature says "doubler takes a value of *any* type and returns a value of the same type. There are no constraints on what this type can be."
-- NB: so all functions in haskell are totally generic unles syou add class constraints!!!
doubler :: a -> a

-- this is a better type signature for this function because it constrains the types that doubler will happily receive
-- this type signature is what haskell would infer for you if you did not provide one
doubler :: (Num a) => a -> a
doubler x = x + x
```

"The essence of a monoid is that you have two things of the same type and you
combine them to create another thing of the same type"

haskell has a really good syntax for function composition

```javascript
// function composition in JS
var f = function (x) { ... }
var g = function (x) { ... }
var h = function (x) {
  return g(f(x));
}
// JS has no types so we have to manually make sure the types line up
```

```haskell
f :: x -> x
f x = ...

g :: x -> x
g x = ...

h :: x -> x
h x = g (f x)
-- h x = g . f x
-- h = g . f

```

- a monoid is a collection of things plus a rule for combining the things and
  that rule obeys certain rules
- the numbers on a clock form a monoid

mempty = 12 mappend = (x + y) mod 12

- the "things" in a clock are numbers, in a monoid they are functions a monoid
  is collection of functions with a rule for combining them. this rule follows
  certain rules

1. must be associative: (1 + 4) + 5 == 1 + (4 + 5)
2. there must be an identity value in the collection (number on the clock,
   function in the monoid) that means that any thing from the collection
   combined (using the combining function) yields itself

so a clock is a visual representation of an "integer monoid"

- it doesn't contain all the integers but the ones it does are included because
  they follow the "combination rule"

"functions under composition form a monoid" - i think this means their types the
lense we look at functions at is just the type signature - a function is a
"thing that turns an a into a b"

f :: a -> b g :: b -> c h :: a -> c h = g . f -- in this case the types are not
a monoid (because they can only be combined in one direction) but they are a
"monoidal category"

it seems that functions only form a monoid _if all the types are the same_

functions live in a monoid, the data lives in a monad
