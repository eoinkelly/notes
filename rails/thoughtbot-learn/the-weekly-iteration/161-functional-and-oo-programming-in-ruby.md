## Ruby: Functaional or Object-orientated?

OO
  * mutating data in the middle of functions
  * data encapsulations

Functional programming
  * public data structures
  * does not modify any of its args
  * data structures coming in and out of functions

Q: Are class methods functinal programming?
  * A: ish

you can do a while loop in FP using recursion
have a default arg on the func that is only used on the first time through the function

pattern: to make for/while loops you do recursion with one arg that functions as a counter and gets incremened/decremented each time through the function

"There is a difference between mutation and modification"


fold/inject is basically recursion, starting with a base case and recursing across the list of things
learn has an inject exercise
inject can be used to implement map etc.

"every time you see each, you are calling something for side effects"
when you call each on a list it returns the same lsit so it is a noop from a FP POV
each (and tap) generally indicate that side-effects are happening

Joe ferris does not like inheriting from struct in real life


### The expression problem

TODO: be able to explain this properly

* easy to add new cases
* harder to add new behaviour to all the existing cases

or vice versa - it seems we can't have both

If you think you will add more behaviour that new cases you should choose X otherwise choose Y

"add new behaviour by adding new objects"

TODO: research the visitor pattern
